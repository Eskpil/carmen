use serde::{Deserialize, Serialize};
use super::util;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ExplicitType {
    Name(String),
    Pointer(Box<ExplicitType>),

    // Leave this to the type inference.
    Empty,
}

impl ExplicitType {
    pub fn name(name: String) -> Self {
        Self::Name(name)
    }

    pub fn to_name(&self) -> String {
        match self {
            ExplicitType::Name(n) => n.clone(),
            o => unreachable!(
                "Expected DefinedType::Name() but found DefinedType::{:?}",
                o
            ),
        }
    }

    pub fn print(&self, indent: usize) {
        match self {
            ExplicitType::Name(s) => {
                util::print_indent(indent, "Name:".into());
                util::print_indent(indent + 1, s.clone());
            }
            ExplicitType::Pointer(to) => {
                util::print_indent(indent, "Pointer::".into());
                to.print(indent + 1);
            }
            ExplicitType::Empty => {
                util::print_indent(indent, "Empty".into());
            }
        }
    }
}