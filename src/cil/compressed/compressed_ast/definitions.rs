use crate::ast::BinaryOp;
use crate::cil::common::Tags;
use crate::cil::compressed::compressed_ast;
use crate::cil::compressed::compressed_ast::{Signature, Type};
use crate::cil::typecheck::typechecked_ast::ModuleName;

type VariableId = u32;
type Name = String;

#[derive(Debug, Clone)]
pub struct LiteralExpression {
    pub typ: Type,
    pub value: u64,
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    pub op: BinaryOp,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct VariableLookupExpression {
    pub id: VariableId,
}

#[derive(Debug, Clone)]
pub struct UseDataExpression {
    pub name: Name,
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub name: Name,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(LiteralExpression),
    Binary(BinaryExpression),
    VariableLookup(VariableLookupExpression),
    UseData(UseDataExpression),
    Call(CallExpression),
}

#[derive(Debug, Clone)]
pub struct DeclareVariableStatement {
    pub id: VariableId,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct DefineVariableStatement {
    pub id: VariableId,
    pub expr: Expression,
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub expr: Expression,
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub expr: Expression,
}

#[derive(Debug, Clone)]
pub struct LoopStatement {
    pub cond: Expression,
    pub block: Block,
}

#[derive(Debug, Clone)]
pub enum Statement {
    DeclareVariable(DeclareVariableStatement),
    DefineVariable(DefineVariableStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Loop(LoopStatement),
}

#[derive(Debug, Clone)]
pub struct Block {
    pub parameters: Vec<(VariableId, Type)>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub for_declaration: Name,
    pub block: Block,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub name: Name,
    pub module_name: ModuleName,
    pub signature: Signature,
    pub tags: Tags,
}

#[derive(Debug, Clone)]
pub struct DataDeclaration {
    pub name: String,
    pub module_name: ModuleName,
    pub data: Vec<u8>,
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Function(FunctionDeclaration),
    Data(DataDeclaration)
}

#[derive(Debug, Clone)]
pub struct Program {
    pub declarations: Vec<compressed_ast::Declaration>,
    pub definitions: Vec<compressed_ast::FunctionDefinition>,
}