pub mod typechecked_ast;

use std::collections::{HashMap, VecDeque};
use std::ops::Index;
use rand::distributions::{Alphanumeric, DistString};
use crate::ast;
use crate::ast::definitions::ExplicitType;
use crate::cil::common::Stage;
use crate::cil::typecheck::typechecked_ast::{Block, CallExpression, convert_type, DataDeclaration, Declaration, DeclareVariableStatement, DefineVariableStatement, Expression, FunctionDeclaration, FunctionDefinition, LiteralExpression, Module, ModuleName, ReturnStatement, Signature, Statement, Type, UseDataExpression, VariableLookupExpression};

type ModuleId = u32;

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub typ: Type,
    pub id: u32,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub id: u32,
    pub parent: u32,
    pub stage: Stage,
    pub expected_return_type: Option<Type>,
    pub variables: Vec<Variable>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub modules: HashMap<ModuleId, Module>
}

#[derive(Debug, Clone)]
pub struct TypeChecker {
    pub program: Program,
    pub scopes: VecDeque<Scope>,

    scope_id_counter: u32,
    variable_id_counter: u32,
    module_id_counter: u32,
    modules: Vec<ast::Module>,
}

impl Scope {
    pub fn lookup_variable(&self, name: String) -> Option<Variable> {
        for var in self.variables.iter() {
            if var.name == name.clone() {
                return Some(var.clone());
            }
        }

        return None
    }
}

impl TypeChecker {
    pub fn new() -> Self {
        let global_scope = Scope {
            id: 1,
            parent: 0,
            stage: Stage::Global,
            expected_return_type: None,
            variables: vec![],
        };

        let mut scopes = VecDeque::new();
        scopes.push_front(global_scope);

        TypeChecker {
            program: Program {
                modules: HashMap::new(),
            },
            scopes,

            scope_id_counter: 1,
            variable_id_counter: 0,
            module_id_counter: 0,

            modules: vec![],
        }
    }

    pub fn new_scope(&mut self) -> Scope {
        let new_scope_id = self.scope_id_counter + 1;

        if let Some(current_scope) = self.current_scope() {
            Scope {
                id: new_scope_id,
                parent: current_scope.id,
                stage: Stage::Local,
                expected_return_type: None,
                variables: vec![],
            }
        } else {
            unreachable!("no scope");
        }
    }

    pub fn current_scope_mut(&mut self) -> Option<&mut Scope> {
        self.scopes.front_mut()
    }
    pub fn current_scope(&self) -> Option<&Scope> {
        return self.scopes.front();
    }

    pub fn insert_scope(&mut self, scope: Scope) {
        self.scopes.push_front(scope);
    }

    pub fn find_scope_by_id(&self, scope_id: u32) -> Option<&Scope> {
        for scope in self.scopes.iter() {
            if scope.id == scope_id {
                return Some(scope)
            }
        }

        None
    }

    pub fn lookup_variable_in_scope(&self, scope_id: u32, name: String) -> Option<Variable> {
        let scope = self.find_scope_by_id(scope_id).expect("could not find scope");

        if scope.parent == 0 {
            return None;
        }

        return if let Some(var) = scope.lookup_variable(name.clone()) {
            Some(var)
        } else {
            self.lookup_variable_in_scope(scope.parent, name.clone())
        }
    }

    pub fn lookup_variable(&self, name: String) -> Option<Variable> {
        let current_scope = self.current_scope();
        if current_scope.is_none() {
            return None
        }

        return self.lookup_variable_in_scope(current_scope.unwrap().id, name);
    }

    pub fn get_variable_id(&mut self) -> u32 {
        self.variable_id_counter += 1;
        let scope = self.current_scope().expect("could not get scope");
        scope.id * self.variable_id_counter
    }

    pub fn push_variable(&mut self, var: Variable) {
        let scope = self.scopes.front_mut().expect("no scope");
        scope.variables.push(var);
    }

    pub fn expression_returns(&self, expr: &Expression, current_module: &mut Module) -> Type {
        match expr {
            Expression::Literal(_) => Type::Usize,
            Expression::Call(call) => {
                let module = self.program.modules.get(&call.name.id).unwrap_or(current_module);
                let decl = module.get_function_declaration(call.name.name.clone()).expect("could not find declaration");
                decl.signature.returns[0].clone()
            }
            Expression::Binary(_) => Type::Usize,
            Expression::VariableLookup(var) => {
                var.variable.typ.clone()
            }
            Expression::UseData(data) => Type::Pointer(Box::new(Type::U8)),
        }
    }

    pub fn module_by_name(&mut self, name: String) -> &Module {
        self.program.modules.iter().find(|e| e.clone().1.name == name ).unwrap().1
    }

    pub fn module_has_function_in_scope(&mut self, module: &mut Module, name: &ast::expressions::LookupExpression) -> bool {
        if name.child.is_none() {
            return module.has_function_declaration(name.name.clone());
        }

        let module_name = name.name.clone();
        let function_name = name.child.clone().unwrap().name;

        let module = self.module_by_name(module_name);
        module.has_function_declaration(function_name)
    }

    pub fn module_get_function_declaration(&mut self, module: &mut Module, name: &ast::expressions::LookupExpression) -> (ModuleId, Option<FunctionDeclaration>) {
        assert!(self.module_has_function_in_scope(module, name));
        if name.child.is_none() {
            return (module.id, module.get_function_declaration(name.name.clone()));
        }

        let module_name = name.name.clone();
        let function_name = name.child.clone().unwrap().name;

        let module = self.module_by_name(module_name);
        (module.id, module.get_function_declaration(function_name))
    }

    pub fn typecheck_expression(&mut self, ast_expression: &ast::expressions::Expression, module: &mut Module) -> Expression {
        match ast_expression {
            ast::expressions::Expression::Literal(literal) => {
                Expression::Literal(LiteralExpression(literal.val))
            }
            ast::expressions::Expression::Call(call) => {
                if !self.module_has_function_in_scope(module, &call.name) {
                    todo!("throw function not found error")
                }

                // safe to unwrap, we already checked if function is in scope.
                let (id, declaration) = self.module_get_function_declaration(module, &call.name);
                let declaration = declaration.unwrap();
                let mut arguments = Vec::<Box<Expression>>::new();

                for i in 0..call.arguments.len() {
                    let argument = call.arguments.index(i);
                    let signature_accept = &declaration.signature.accepts[i];
                    let typechecked_argument = self.typecheck_expression(&argument.value, module);
                    if *signature_accept != self.expression_returns(&typechecked_argument, module) {
                        println!("func: {} sig: {:?} arg: {:?}", call.name.to_string(), signature_accept, self.expression_returns(&typechecked_argument, module));
                        todo!("throw type error");
                    }
                    arguments.push(Box::new(typechecked_argument));
                }

                Expression::Call(CallExpression {
                    name: ModuleName {
                        id,
                        name: declaration.name,
                    },
                    arguments,
                })
            }
            ast::expressions::Expression::Lookup(lookup) => {
                if lookup.child.is_none() {
                    if let Some(variable) = self.lookup_variable(lookup.name.clone()) {
                        return Expression::VariableLookup(VariableLookupExpression{variable });
                    } else {
                        todo!("throw variable not found error")
                    }
                }

                todo!("support struct indirection")
            }
            ast::expressions::Expression::StringLiteral(string) => {
                let name = Alphanumeric.sample_string(&mut rand::thread_rng(), 16);

                let data_decl = DataDeclaration {
                    name: name.to_owned(),
                    data: string.val.clone().into_bytes(),
                };
                module.declarations.push(Declaration::Data(data_decl));

                Expression::UseData(UseDataExpression { name: name.to_owned() } )
            }

            e => todo!("implement: {:?}", e)
        }
    }

    pub fn typecheck_function_declaration(&mut self, function: &ast::statements::FunctionStatement, module: &mut Module) -> FunctionDeclaration {
        let mut accepts: Vec<Type> = vec![];
        let mut returns: Vec<Type> = vec![];

        for param in function.parameters.iter() {
            accepts.push(convert_type(&param.defined_type));
        }

        if function.return_type.clone() != ExplicitType::Empty {
            returns.push(Type::Usize);
        }

        FunctionDeclaration {
            name: function.name.clone(),
            signature: Signature { accepts, returns },
        }
    }

    pub fn typecheck_declarations(&mut self, ast_module: &ast::Module, module: &mut Module) {
        for stmt in &ast_module.statements {
            let decl = match stmt.as_function() {
                Some(function) => Declaration::Function(self.typecheck_function_declaration(&function, module)),
                None => continue
            };

            module.declarations.push(decl);
        };
    }

    pub fn typecheck_block(&mut self, ast_block: &ast::statements::BlockStatement, module: &mut Module) -> Block {
        let mut block = Block {
            statements: vec![],
            parameters: vec![],
        };

        for stmt in ast_block.statements.iter() {
            let typechecked_stmt = match stmt {
                ast::statements::Statement::Let(let_statement) => {
                    let expr = self.typecheck_expression(&let_statement.expr, module);

                    let variable = Variable {
                        name: let_statement.name.clone(),
                        typ:  Type::Usize,
                        id: self.get_variable_id(),
                    };

                    self.push_variable(variable.clone());
                    vec![
                        Statement::DeclareVariable(DeclareVariableStatement{ variable: variable.clone() }),
                        Statement::DefineVariable(DefineVariableStatement { variable, expr }),
                    ]
                }
                ast::statements::Statement::Return(ret) => {
                    let expr = self.typecheck_expression(&ret.expr, module);

                    if let Some(scope) = self.current_scope() {
                        if Some(self.expression_returns(&expr, module)) != scope.expected_return_type {
                            todo!("implement type errors")
                        }
                    } else {
                        unreachable!("no scope?!?")
                    }

                    vec![Statement::Return(ReturnStatement{expr})]
                }

                s => todo!("implement s: {:?}", s)
            };

            block.statements.extend(typechecked_stmt);
        }

        block
    }

    pub fn typecheck_function_definition(&mut self, function: &ast::statements::FunctionStatement, module: &mut Module) -> FunctionDefinition {
        let mut scope = self.new_scope();
        scope.expected_return_type = Some(convert_type(&function.return_type));

        for param in &function.parameters {
            let id = self.get_variable_id();

            scope.variables.push(Variable {
                name: param.name.clone(),
                typ: convert_type(&param.defined_type),
                id,
            })
        }
        let parameters = scope.variables.clone();

        self.insert_scope(scope);
        let mut block = self.typecheck_block(&function.block, module);
        block.parameters = parameters;

        FunctionDefinition {
            name: function.name.clone(),
            block,
        }

    }

    pub fn typecheck_function_definitions(&mut self, ast_module: &ast::Module, module: &mut Module) {
        for stmt in &ast_module.statements {
            if let Some(function) = stmt.as_function(){
                if function.block.is_empty() { continue }
                let definition = self.typecheck_function_definition(&function, module);
                module.definitions.push(definition);
            }
        }
    }

    pub fn typecheck_imports(&mut self, ast_module: &ast::Module, module: &mut Module) {
        for stmt in &ast_module.statements {
            match stmt.as_import() {
                Some(import) => {
                    let target = self.program.modules.iter().find(|m| { m.1.name == import.name });
                    if let Some(target) = target {
                        module.imports.push(*target.0);
                    } else {
                        let cache = self.modules.clone();
                        let target_ast_module = cache.iter().find(|m| { m.name == import.name }).expect("module not found");
                        self.typecheck_module(target_ast_module);
                        let target = self.program.modules.iter().find(|m| { m.1.name == import.name}).unwrap();
                        module.imports.push(*target.0);
                    }

                }
                None => continue,
            }
        }
    }

    pub fn typecheck_module(&mut self, ast_module: &ast::Module) {
        self.module_id_counter += 1;
        let id = self.module_id_counter as ModuleId;
        let mut module = Module::new(id, ast_module.name.clone());

        self.typecheck_imports(ast_module, &mut module);
        self.typecheck_declarations(ast_module, &mut module);
        self.typecheck_function_definitions(ast_module, &mut module);

        self.program.modules.insert(id, module);
    }

    pub fn typecheck_modules(&mut self, modules: Vec<ast::Module>) -> Program {
        self.modules.extend(modules);
        let cache = self.modules.clone();
        let main = cache.iter().find(|m| { m.name == "main" }).expect("provided program does not have main module");
        self.typecheck_module(main);


        println!("typechecking modules: {:?}", self.program);

        self.program.clone()
    }
}
