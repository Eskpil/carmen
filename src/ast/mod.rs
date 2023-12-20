pub mod expressions;
pub mod statements;
pub mod definitions;
mod util;

use crate::lexer::TokenKind;
use std::process;

use serde::{Serialize, Deserialize};

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Less,
    Greater,
    Mod,
    GreaterEquals,
    LessEquals,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Module {
    pub name: String,
    pub statements: Vec<statements::Statement>,
}

impl BinaryOp {
    pub fn from_token_kind(kind: &TokenKind) -> Self {
        match kind.clone() {
            TokenKind::Add => Self::Add,
            TokenKind::Sub => Self::Sub,
            TokenKind::Mul => Self::Mul,
            TokenKind::Div => Self::Div,
            TokenKind::Greater => Self::Greater,
            TokenKind::Less => Self::Less,
            TokenKind::Percent => Self::Mod,
            TokenKind::GreaterEquals => Self::GreaterEquals,
            TokenKind::LessEquals => Self::LessEquals,
            token => {
                eprintln!("Token: {:?} is unsuitable for binary operations.", token);
                process::exit(1);
            }
        } 
    }

    pub fn returns_bool(&self) -> bool {
        match self.clone() {
            Self::Greater 
            | Self::Less
            | Self::GreaterEquals
            | Self::LessEquals => true,
            _ => false,              
        } 
    }

    pub fn to_string(&self) -> String {
        let s = match self.clone() {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Greater => ">",
            Self::Less => "<",
            Self::Mod => "%",
            Self::GreaterEquals => ">=",
            Self::LessEquals => "<=",
        };

        s.into()
    }
}
