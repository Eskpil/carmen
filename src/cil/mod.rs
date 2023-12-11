use std::collections::{HashMap, VecDeque};
use std::ops::Deref;
use crate::ast::BinaryOp;

pub mod generate;
pub mod typecheck;

/*

    to help out the code generation as much as possible we should ideally not provide it with names of variables but
    rather an id. Variable id's could be a construct of both a increment and the scope id. We also need a TypeId system
    which can correlate structs, enums, primitives and function signatures into a specific type id. Currently the only
    type we support is Usize.

 */

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Stage {
    Global,
    Local,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Usize,
    U8,
    Pointer(Box<Type>)
}

#[derive(Debug, Clone)]
pub struct Signature {
    pub accepts: Vec<Type>,
    pub returns: Vec<Type>,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub name: String,
    pub signature: Signature,
}

#[derive(Debug, Clone)]
pub struct DataDeclaration {
    pub name: String,
    pub data: Vec<u8>,
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Function(FunctionDeclaration),
    Data(DataDeclaration)
}

#[derive(Debug, Clone)]
pub struct LiteralExpression(pub u64);

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    pub op: BinaryOp,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub name: String,
    pub arguments: Vec<Box<Expression>>
}

#[derive(Debug, Clone)]
pub struct VariableLookupExpression {
    pub variable: Variable,
}

#[derive(Debug, Clone)]
pub struct UseDataExpression {
    pub name: String,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(LiteralExpression),
    Binary(BinaryExpression),
    Call(CallExpression),
    VariableLookup(VariableLookupExpression),
    UseData(UseDataExpression),
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub expr: Expression,
}

#[derive(Debug, Clone)]
pub struct DeclareVariableStatement {
    pub variable: Variable,
    pub expr: Expression,
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement(pub Expression);

#[derive(Debug, Clone)]
pub enum Statement {
    Return(ReturnStatement),
    DeclareVariable(DeclareVariableStatement),
    Expression(ExpressionStatement),
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub parameters: Vec<Variable>,
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub name: String,
    pub block: Block,
}



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
    pub declarations: Vec<Declaration>,
    pub definitions: Vec<FunctionDefinition>
}

#[derive(Debug, Clone)]
pub struct TypeChecker {
    pub program: Program,
    pub scopes: VecDeque<Scope>,

    scope_id_counter: u32,
    variable_id_counter: u32,
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
                declarations: Vec::new(),
                definitions: Vec::new(),
            },
            scopes,

            scope_id_counter: 1,
            variable_id_counter: 0,
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

    pub fn find_function_declaration(&self, name: String) -> Option<FunctionDeclaration> {
        for declaration in self.program.declarations.clone() {
            match declaration {
                Declaration::Function(function) => {
                    if function.name == name {
                        return Some(function);
                    }
                },
                _ => continue
            }
        }

        None
    }

    pub fn push_variable(&mut self, var: Variable) {
        let scope = self.scopes.front_mut().expect("no scope");
        scope.variables.push(var);
    }

    pub fn expression_returns(&self, expr: &Expression) -> Type {
        match expr {
            Expression::Literal(_) => Type::Usize,
            Expression::Call(call) => {
                let declaration = self.find_function_declaration(call.name.clone()).expect("could not find declaration");
                declaration.signature.returns[0].clone()
            }
            Expression::Binary(_) => Type::Usize,
            Expression::VariableLookup(var) => {
                var.variable.typ.clone()
            }
            Expression::UseData(data) => Type::Pointer(Box::new(Type::U8)),
        }
    }
}