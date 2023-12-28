use crate::ast::expressions::Expression;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ArrayType {
    pub of: Box<ExplicitType>,
    pub size_expr: Expression,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PointerType {
    pub to: Box<ExplicitType>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ExplicitType {
    Name(String),
    Pointer(PointerType),
    Array(ArrayType),

    // Leave this to the type inference.
    Empty,
}

impl ExplicitType {
    pub fn as_array(&self) -> Option<ArrayType> {
        match self {
            ExplicitType::Array(a) => Some(a.clone()),
            _ => None,
        }
    }
}
