pub mod expressions;
pub mod statements;
pub mod definitions;

use core::fmt;
use std::fmt::Formatter;
use crate::lexer::TokenKind;
use std::process;

use serde::{Serialize, Deserialize};

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Less,
    Greater,

    LessEquals,
    GreaterEquals,

    Equals,
    NotEquals,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Module {
    pub name: String,
    pub statements: Vec<statements::Statement>,
}

impl BinaryOp {
    pub fn from_token_kind(kind: &TokenKind) -> Self {
        match kind {
            TokenKind::Add => Self::Add,
            TokenKind::Sub => Self::Sub,
            TokenKind::Mul => Self::Mul,
            TokenKind::Div => Self::Div,
            TokenKind::Percent => Self::Mod,

            TokenKind::Greater => Self::Greater,
            TokenKind::Less => Self::Less,

            TokenKind::GreaterEquals => Self::GreaterEquals,
            TokenKind::LessEquals => Self::LessEquals,

            TokenKind::Equals => Self::Equals,
            TokenKind::NotEquals => Self::NotEquals,

            token => {
                eprintln!("Token: {:?} is unsuitable for binary operations.", token);
                process::exit(1);
            }
        } 
    }

    pub fn returns_bool(&self) -> bool {
        matches!(self, BinaryOp::Less | BinaryOp::Greater | BinaryOp::LessEquals | BinaryOp::GreaterEquals | BinaryOp::Equals | BinaryOp::NotEquals )
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Mod => "%",

            Self::Greater => ">",
            Self::Less => "<",

            Self::LessEquals => "<=",
            Self::GreaterEquals => ">=",

            Self::Equals => "==",
            Self::NotEquals => "!=",
        };

        write!(f, "{s}")
    }
}