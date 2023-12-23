use crate::ast::BinaryOp;
use crate::cil::common::Tag;
use crate::cil::typecheck::{Variable, typechecked_ast::Signature, ModuleId};
use crate::cil::typecheck::type_id::TypeId;

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleName {
    pub id: ModuleId,
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub name: ModuleName,
    pub signature: Signature,
    pub tags: Vec<Tag>,
}

#[derive(Debug, Clone)]
pub struct DataDeclaration {
    pub name: ModuleName,
    pub data: Vec<u8>,
    pub tags: Vec<Tag>,
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Function(FunctionDeclaration),
    Data(DataDeclaration)
}

#[derive(Debug, Clone)]
pub struct LiteralExpression {
    pub typ: TypeId,
    pub value: u64,
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    pub op: BinaryOp,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub name: ModuleName,
    pub arguments: Vec<Expression>
}

#[derive(Debug, Clone)]
pub struct VariableLookupExpression {
    pub variable: Variable,
}

#[derive(Debug, Clone)]
pub struct UseDataExpression {
    pub name: ModuleName,
}

#[derive(Debug, Clone)]
pub struct BooleanExpression {
    pub value: bool,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(LiteralExpression),
    Binary(BinaryExpression),
    Call(CallExpression),
    VariableLookup(VariableLookupExpression),
    UseData(UseDataExpression),
    Bool(BooleanExpression),
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub expr: Expression,
}

#[derive(Debug, Clone)]
pub struct DeclareVariableStatement {
    pub variable: Variable,
}

#[derive(Debug, Clone)]
pub struct DefineVariableStatement {
    pub variable: Variable,
    pub expr: Expression,
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement(pub Expression);

#[derive(Debug, Clone)]
pub struct WhileStatement {
    pub cond: Expression,
    pub block: Block,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(ReturnStatement),
    DeclareVariable(DeclareVariableStatement),
    DefineVariable(DefineVariableStatement),
    Expression(ExpressionStatement),
    While(WhileStatement)
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub parameters: Vec<Variable>,
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub name: ModuleName,
    pub block: Block,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub id: ModuleId,

    pub name: String,
    pub imports: Vec<ModuleId>,

    pub declarations: Vec<Declaration>,
    pub definitions: Vec<FunctionDefinition>
}

impl Module {
    pub fn new(id: ModuleId, name: String) -> Self{
       Self {
           id,
           name,
           imports: vec![],
           declarations: vec![],
           definitions: vec![],
       }
    }

    pub fn has_function_declaration(&self, name: String) -> bool {
        self.get_function_declaration(name).is_some()
    }

    pub fn get_function_declaration(&self, name: String) -> Option<FunctionDeclaration> {
        for decl in &self.declarations {
            match decl {
                Declaration::Function(func) => {
                    if func.name.name == name {
                        return Some(func.clone());
                    }
                }
                _ => continue
            }
        }

        None
    }
}
