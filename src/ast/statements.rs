use super::definitions::ExplicitType;
use super::expressions::{Expression, LookupExpression, SubstrateExpression};
use crate::lexer::Span;

use crate::cil::common::Tags;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IfStatement {
    pub span: Span,
    pub cond: Expression,

    pub if_block: BlockStatement,
    pub else_block: Option<BlockStatement>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NamedParameter {
    pub name: String,
    pub span: Span,
    pub typ: ExplicitType,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlockStatement {
    pub span: Span,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionStatement {
    pub span: Span,
    pub name: String,
    pub parameters: Vec<NamedParameter>,
    pub block: Option<BlockStatement>,
    pub return_type: ExplicitType,
    pub external: bool,
    pub tags: Tags,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExpressionStatement {
    pub span: Span,
    pub expr: Expression,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WhileStatement {
    pub span: Span,
    pub condition: Expression,
    pub body: BlockStatement,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LetStatement {
    pub span: Span,
    pub name: String,
    pub explicit_type: ExplicitType,
    pub expr: Expression,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConstStatement {
    pub span: Span,
    pub name: String,
    pub explicit_type: ExplicitType,
    pub expr: Expression,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VarStatement {
    pub span: Span,
    pub name: String,
    pub explicit_type: ExplicitType,
    pub expr: Expression,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReturnStatement {
    pub span: Span,
    pub expr: Expression,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WithStatement {
    pub span: Span,
    pub from: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImportStatement {
    pub span: Span,
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DefineStatement {
    pub name: LookupExpression,
    pub expr: Expression,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DefineSubstrateStatement {
    pub substrate: SubstrateExpression,
    pub expr: Expression,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Statement {
    If(IfStatement),
    Function(FunctionStatement),
    Block(BlockStatement),
    Expression(ExpressionStatement),
    While(WhileStatement),

    Let(LetStatement),
    Const(ConstStatement),
    Var(VarStatement),

    Return(ReturnStatement),
    Import(ImportStatement),

    Define(DefineStatement),
    DefineSubstrate(DefineSubstrateStatement),
}

impl IfStatement {
    pub fn new(
        span: Span,
        cond: Expression,
        if_block: BlockStatement,
        else_block: Option<BlockStatement>,
    ) -> Self {
        Self {
            span,
            cond,
            if_block,
            else_block,
        }
    }
}

impl NamedParameter {
    pub fn new(name: String, typ: ExplicitType, span: Span) -> Self {
        Self { name, span, typ }
    }
}

impl Statement {
    pub fn as_import(&self) -> Option<ImportStatement> {
        match self.clone() {
            Statement::Import(import) => Some(import),
            _ => None,
        }
    }

    pub fn as_function(&self) -> Option<FunctionStatement> {
        match self.clone() {
            Statement::Function(function) => Some(function),
            _ => None,
        }
    }

    pub fn as_block(&self) -> Option<BlockStatement> {
        match self.clone() {
            Statement::Block(block) => Some(block),
            _ => None
        }
    }
}

impl BlockStatement {
    pub fn is_empty(&self) -> bool {
        self.statements.is_empty()
    }
}
