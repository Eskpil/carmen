use super::BinaryOp;
use crate::lexer::Span;
use core::fmt;
use serde::{Deserialize, Serialize};
use std::boxed::Box;
use std::fmt::Formatter;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LookupExpression {
    pub span: Span,
    pub name: String,
    pub child: Option<Box<LookupExpression>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NamedArgument {
    pub name: String,
    pub span: Span,
    pub value: Expression,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmptyExpression {
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LiteralExpression {
    pub span: Span,
    pub val: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BooleanExpression {
    pub span: Span,
    pub val: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StringLiteralExpression {
    pub span: Span,
    pub val: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IdentifierExpression {
    pub span: Span,
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BinaryExpression {
    pub span: Span,
    pub op: BinaryOp,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnaryExpression {
    pub span: Span,
    pub op: BinaryOp,
    pub expr: Box<Expression>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CallExpression {
    pub span: Span,
    pub name: LookupExpression,
    pub arguments: Vec<NamedArgument>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SubstrateExpression {
    pub span: Span,
    pub name: LookupExpression,
    pub expr: Box<Expression>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ArrayInitExpression {
    pub span: Span,
    pub values: Vec<Expression>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpreadExpression {
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Expression {
    Empty(EmptyExpression),
    Literal(LiteralExpression),
    Bool(BooleanExpression),
    StringLiteral(StringLiteralExpression),
    Identifier(IdentifierExpression),
    Binary(BinaryExpression),
    Unary(UnaryExpression),
    Call(CallExpression),
    Lookup(LookupExpression),
    Substrate(SubstrateExpression),
    ArrayInit(ArrayInitExpression),
    Spread(SpreadExpression),
}

impl NamedArgument {
    pub fn new(name: String, expr: Expression, span: Span) -> Self {
        Self {
            name,
            span,
            value: expr,
        }
    }
}

impl Expression {
    pub fn span(&self) -> Span {
        match self.clone() {
            Expression::Empty(e) => e.span,
            Expression::Identifier(var) => var.span,
            Expression::Literal(lit) => lit.span,
            Expression::StringLiteral(lit) => lit.span,
            Expression::Bool(b) => b.span,
            Expression::Binary(b) => b.span,
            Expression::Unary(u) => u.span,
            Expression::Call(c) => c.span,
            Expression::Lookup(l) => l.span,
            Expression::Substrate(s) => s.span,
            Expression::ArrayInit(a) => a.span,
            Expression::Spread(s) => s.span,
        }
    }

    pub fn is_spread(&self) -> bool {
        match self {
            Expression::Spread(_) => true,
            _ => false,
        }
    }

    pub fn is_substrate(&self) -> bool {
        match self {
            Expression::Substrate(_) => true,
            _ => false,
        }
    }

    pub fn is_lookup(&self) -> bool {
        match self {
            Expression::Lookup(_) => true,
            _ => false,
        }
    }

    pub fn as_lookup(&self) -> Option<LookupExpression> {
        match self.clone() {
            Expression::Lookup(l) => Some(l),
            _ => None,
        }
    }

    pub fn as_substrate(&self) -> Option<SubstrateExpression> {
        match self.clone() {
            Expression::Substrate(s) => Some(s),
            _ => None,
        }
    }
}

impl fmt::Display for LookupExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}
