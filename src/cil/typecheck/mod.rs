pub mod typechecked_ast;
pub mod type_id;
mod runtime;
mod constants;

use std::collections::{HashMap, VecDeque};
use std::ops::Index;
use rand::distributions::{Alphanumeric, DistString};
use crate::ast;
use crate::ast::definitions::ExplicitType;
use crate::cil::common::{expand_tags, Stage, Tag};
use crate::cil::typecheck::runtime::Runtime;
use crate::cil::typecheck::type_id::{aliases, Primitive, TypeId, TypePool};
use crate::cil::typecheck::typechecked_ast::{BinaryExpression, Block, BooleanExpression, CallExpression, DataDeclaration, Declaration, DeclareVariableStatement, DefineVariableStatement, Expression, ExpressionStatement, FunctionDeclaration, FunctionDefinition, LiteralExpression, Module, ModuleName, ReturnStatement, Signature, Statement, UseDataExpression, VariableLookupExpression, WhileStatement};

pub type ModuleId = u32;

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub typ: TypeId,
    pub id: u32,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub id: u32,
    pub parent: u32,
    pub stage: Stage,
    pub expected_return_type: Option<TypeId>,
    pub variables: Vec<Variable>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub modules: HashMap<ModuleId, Module>
}

#[derive(Debug, Clone)]
pub struct TypeChecker {
    pub program: Program,
    pub scopes: Vec<Scope>,

    scope_id_counter: u32,
    variable_id_counter: u32,
    module_id_counter: u32,
    modules: Vec<ast::Module>,

    type_pool: TypePool,
}

impl Scope {
    pub fn lookup_variable(&self, name: String) -> Option<Variable> {
        for var in self.variables.iter() {
            if var.name == name.clone() {
                return Some(var.clone());
            }
        }

        None
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

        let mut scopes = Vec::new();
        scopes.push(global_scope);

        TypeChecker {
            program: Program {
                modules: HashMap::new(),
            },
            scopes,

            scope_id_counter: 1,
            variable_id_counter: 0,
            module_id_counter: 0,

            modules: vec![],

            type_pool: TypePool::new(),
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
    pub fn current_scope(&self) -> Option<&Scope> {
        return self.scopes.last();
    }

    pub fn insert_scope(&mut self, scope: Scope) {
        self.scopes.push(scope);
    }

    pub fn find_scope_by_id(&self, scope_id: u32) -> Option<&Scope> {
        self.scopes.iter().find(|s| { s.id == scope_id })
    }

    pub fn lookup_variable_in_scope(&self, scope_id: u32, name: String) -> Option<Variable> {
        let scope = self.find_scope_by_id(scope_id).expect("could not find scope");

        if scope.stage == Stage::Global {
            return None;
        }

        if let Some(var) = scope.lookup_variable(name.clone()) {
            Some(var)
        } else {
            self.lookup_variable_in_scope(scope.parent, name.clone())
        }
    }

    pub fn lookup_variable(&self, name: String) -> Option<Variable> {
        let current_scope = self.current_scope()?;
        self.lookup_variable_in_scope(current_scope.id, name)
    }

    pub fn get_variable_id(&mut self) -> u32 {
        self.variable_id_counter += 1;
        let scope = self.current_scope().expect("could not get scope");
        scope.id * self.variable_id_counter
    }

    pub fn push_variable(&mut self, var: Variable) {
        let scope = self.scopes.last_mut().expect("no scope");
        scope.variables.push(var);
    }

    pub fn expression_returns(&self, expr: &Expression, current_module: &mut Module) -> TypeId {
        match expr {
            Expression::Literal(_) => self.type_pool.find_alias(aliases::USIZE).unwrap(),
            Expression::Call(call) => {
                let module = self.program.modules.get(&call.name.id).unwrap_or(current_module);
                let decl = module.get_function_declaration(call.name.name.clone()).expect("could not find declaration");
                decl.signature.returns.clone()
            }
            // TODO: Support U8 literals as well.
            Expression::Binary(bin) => {
                if bin.op.returns_bool() {
                    self.type_pool.find_primitive(&Primitive::Bool).expect("no bool?!??")
                } else {
                    self.type_pool.find_alias(aliases::USIZE).unwrap()
                }

            },
            Expression::VariableLookup(var) => {
                var.variable.typ.clone()
            }
            Expression::UseData(_) => self.type_pool.find_pointer(self.type_pool.find("u8".to_string()).unwrap()).unwrap(),
            Expression::Bool(_) => self.type_pool.find_primitive(&Primitive::Bool).expect("no bool?!??"),
        }
    }

    pub fn module_by_name(&self, name: String) -> &Module {
        self.program.modules.iter().find(|e| e.1.name == name ).unwrap().1
    }

    pub fn module_by_id(&self, id: &ModuleId) -> &Module {
        self.program.modules.iter().find(|e| e.0 == id).unwrap().1
    }

    pub fn module_has_function_in_scope(&self, module: &mut Module, name: &ast::expressions::LookupExpression) -> bool {
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
                Expression::Literal(LiteralExpression {
                    typ: self.type_pool.find_alias(aliases::USIZE).expect("could not find usize"),
                    value: literal.val,
                })
            }
            ast::expressions::Expression::Call(call) => {
                if !self.module_has_function_in_scope(module, &call.name) {
                    todo!("throw function not found error")
                }

                // safe to unwrap, we already checked if function is in scope.
                let (_, declaration) = self.module_get_function_declaration(module, &call.name);
                let declaration = declaration.unwrap();
                let mut arguments = vec![];

                for i in 0..call.arguments.len() {
                    let argument = call.arguments.index(i);
                    let signature_accept = &declaration.signature.accepts[i];
                    let typechecked_argument = self.typecheck_expression(&argument.value, module);

                    if *signature_accept != self.expression_returns(&typechecked_argument, module) {
                        todo!("throw type error");
                    }
                    arguments.push(typechecked_argument);
                }

                Expression::Call(CallExpression {
                    name: declaration.name,
                    arguments,
                })
            }
            ast::expressions::Expression::Lookup(lookup) => {
                // TODO: Support struct indirection or looking up constants from another module.
                assert!(lookup.child.is_none());

                // 1. Try to find it as a normal variable.
                if let Some(variable) = self.lookup_variable(lookup.name.clone()) {
                    return Expression::VariableLookup(VariableLookupExpression{variable });
                }

                // TODO: 2. Struct indirection.

                // 3. Try find it as a constant expression
                let name = constants::ResolvedModuleName {
                    module: lookup.name.clone(),
                    name: "".to_owned(),
                };

                println!("trying to find constant: {:?}", name);

                let scope = self.get_constant_scope(module);
                if scope.has(&name) {
                    let var = scope.get(&name).unwrap();
                    return Expression::Literal(LiteralExpression {
                        typ: var.value.get_type_id().clone(),
                        value: var.value.inner,
                    })
                }

                todo!("throw error")
            }
            ast::expressions::Expression::StringLiteral(string) => {
                let name = Alphanumeric.sample_string(&mut rand::thread_rng(), 16);

                let mut data_decl = DataDeclaration {
                    name: ModuleName {
                        id: module.id,
                        name,
                    },
                    data: string.val.clone().into_bytes(),
                    tags: vec![],
                };

                // null terminate strings
                data_decl.data.push(0u8);

                module.declarations.push(Declaration::Data(data_decl.clone()));

                Expression::UseData(UseDataExpression { name: data_decl.name } )
            }
            ast::expressions::Expression::Binary(binary) => {
                let lhs = self.typecheck_expression(&binary.lhs, module);
                let rhs = self.typecheck_expression(&binary.rhs, module);

                Expression::Binary(BinaryExpression { op: binary.op, lhs: Box::new(lhs), rhs: Box::new(rhs) })
            }
            ast::expressions::Expression::Bool(b) => {
                Expression::Bool(BooleanExpression {
                    value: b.val,
                })
            }

            e => todo!("implement: {:?}", e)
        }
    }

    pub fn typecheck_function_declaration(&mut self, function: &ast::statements::FunctionStatement, module: &mut Module) -> FunctionDeclaration {
        let mut accepts: Vec<TypeId> = vec![];
        let mut returns: TypeId = self.type_pool.find_primitive(&Primitive::Void).expect("wtf, no void?");

        for param in function.parameters.iter() {
            let type_id = match self.type_pool.find_explicit_type(&param.typ) {
                Ok(t) => t,
                Err(e) => todo!("throw type error: {:?}", e)
            };

            accepts.push(type_id);
        }

        if function.return_type.clone() != ExplicitType::Empty {
            let type_id = match self.type_pool.find_explicit_type(&function.return_type) {
                Ok(t) => t,
                Err(e) => todo!("throw type error: {:?}", e)
            };

            returns = type_id;
        }

        let tags = expand_tags(&function.tags);

        FunctionDeclaration {
            name: ModuleName {
                id: module.id,
                name: function.name.clone()
            },
            signature: Signature { accepts, returns },
            tags,
        }
    }

    pub fn get_constant_scope(&mut self, module: &mut Module) -> constants::Scope {
        let mut scope = module.get_constants();
        scope.ignore_name();

        for id in &module.imports {
            let other = self.module_by_id(id);
            scope.merge(&other.get_constants());
        }

        scope
    }

    pub fn typecheck_const_declaration(&mut self, const_: &ast::statements::ConstStatement, module: &mut Module) {
        let scope = self.get_constant_scope(module);
        let mut computer = constants::Computer::new(&scope);
        let value = computer.compute_expression(&const_.expr, &const_.explicit_type, &self.type_pool);
        if value.is_err() {
            todo!("throw const computer error: {:?}", value.err().unwrap());
        }
        let value =value.unwrap();

        module.constants.push(constants::Variable {
            name: constants::ResolvedModuleName {
                module: module.name.clone(),
                name: const_.name.clone(),
            },
            typ: value.get_type_id().clone(),
            value,
        });
    }

    pub fn typecheck_declarations(&mut self, ast_module: &ast::Module, module: &mut Module) {
        for stmt in &ast_module.statements {
            let decl = match stmt {
                ast::statements::Statement::Function(function) => Declaration::Function(self.typecheck_function_declaration(&function, module)),
                ast::statements::Statement::Const(const_) => {
                    self.typecheck_const_declaration(&const_, module);
                    continue
                },
                _ => continue
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

                    println!("declaring: {}@{:?}",let_statement.name, expr);

                    let variable = Variable {
                        name: let_statement.name.clone(),
                        typ:  self.expression_returns(&expr, module),
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
                ast::statements::Statement::Expression(expr) => {
                    let expr =self.typecheck_expression(&expr.expr, module);
                    vec![Statement::Expression(ExpressionStatement(expr))]
                }
                ast::statements::Statement::While(w) => {
                    let cond = self.typecheck_expression(&w.condition, module);
                    if self.expression_returns(&cond, module) != self.type_pool.find_primitive(&Primitive::Bool).expect("no bool?") {
                        todo!("throw no bool error");
                    }

                    let scope = self.new_scope();
                    let block = self.typecheck_block(&w.body, module);

                    _ = scope;

                    vec![Statement::While(WhileStatement {
                        cond,
                        block,
                    })]
                }
                ast::statements::Statement::Define(define) => {
                    // TODO: This will be different with structs.
                    let name = define.name.name.clone();
                    let expr = self.typecheck_expression(&define.expr, module);

                    let variable = self.lookup_variable(name);
                    if variable.is_none() {
                        todo!("throw variable not found error");
                    }

                    vec![Statement::DefineVariable(DefineVariableStatement {
                        variable: variable.unwrap(),
                        expr,
                    })]
                }
                ast::statements::Statement::Const(con) => {
                    todo!("in block constants")
                }
                s => todo!("implement s: {:?}", s)
            };

            block.statements.extend(typechecked_stmt);
        }

        block
    }

    pub fn typecheck_function_definition(&mut self, function: &ast::statements::FunctionStatement, module: &mut Module) -> FunctionDefinition {
        let decl = module.get_function_declaration(function.name.clone());
        if decl.is_none() {
            todo!("throw function not declared error");
        }

        let mut scope = self.new_scope();
        let type_id = match self.type_pool.find_explicit_type(&function.return_type) {
            Ok(t) => t,
            Err(e) => todo!("throw type error: {:?}", e)
        };
        scope.expected_return_type = Some(type_id);

        for param in &function.parameters {
            let id = self.get_variable_id();
            let type_id = match self.type_pool.find_explicit_type(&param.typ) {
                Ok(t) => t,
                Err(e) => todo!("throw type error: {:?}", e)
            };

            scope.variables.push(Variable {
                name: param.name.clone(),
                typ: type_id,
                id,
            })
        }
        let parameters = scope.variables.clone();

        self.insert_scope(scope);
        let mut block = self.typecheck_block(&function.block, module);
        block.parameters = parameters;

        FunctionDefinition {
            name: decl.unwrap().name,
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
        let id = self.module_id_counter as ModuleId;
        self.module_id_counter += 1;
        let mut module = Module::new(id, ast_module.name.clone());

        self.typecheck_imports(ast_module, &mut module);
        self.typecheck_declarations(ast_module, &mut module);
        self.typecheck_function_definitions(ast_module, &mut module);

        self.program.modules.insert(id, module);
    }

    pub fn inject_runtime(&mut self, with_libc: bool) {
        let id = self.module_id_counter;
        self.module_id_counter += 1;

        let rt = Runtime::new(id, with_libc);
        self.program.modules.insert(id, rt.finalize(self));
    }

    pub fn typecheck_modules(&mut self, modules: Vec<ast::Module>) -> Program {
        self.modules.extend(modules);
        let cache = self.modules.clone();
        let main = cache.iter().find(|m| { m.name == "main" }).expect("provided program does not have main module");
        self.typecheck_module(main);

        self.inject_runtime(true);

        self.program.clone()
    }
}
