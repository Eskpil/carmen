use super::definitions::{ExplicitType};
use super::expressions::{Expression, LookupExpression};
use crate::lexer::Span;

use serde::{Deserialize, Serialize};
use crate::cil::common::Tags;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IfStatement {
    pub span: Span,

    pub cond: Expression,

    pub if_block: Vec<Statement>,
    pub if_span: Span,

    pub else_block: Option<Vec<Statement>>,
    pub else_span: Option<Span>,
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
    pub block: BlockStatement,
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
pub enum Statement {
    If(IfStatement),
    Function(FunctionStatement),
    Block(BlockStatement),
    Expression(ExpressionStatement),
    While(WhileStatement),
    Let(LetStatement),
    Return(ReturnStatement),
    Import(ImportStatement),
    Define(DefineStatement),
}

impl IfStatement {
    pub fn new(
        span: Span,
        cond: Expression,
        if_block: Vec<Statement>,
        if_span: Span,
        else_block: Option<Vec<Statement>>,
        else_span: Option<Span>,
    ) -> Self {
        Self {
            span,
            cond,
            if_block,
            if_span,
            else_block,
            else_span,
        }
    }
}

impl NamedParameter {
    pub fn new(name: String, typ: ExplicitType, span: Span) -> Self {
        Self {
            name,
            span,
            typ,
        }
    }
}

impl Statement {
    pub fn as_import(&self) -> Option<ImportStatement> {
        match self.clone() {
            Statement::Import(import) => Some(import),
            _ => None
        }
    }

    pub fn as_function(&self) -> Option<FunctionStatement> {
        match self.clone() {
            Statement::Function(function) => Some(function),
            _ => None
        }
    }
}

impl BlockStatement {
    pub fn is_empty(&self) -> bool {
        self.statements.is_empty()
    }
}