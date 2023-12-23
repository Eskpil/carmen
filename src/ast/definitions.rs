use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ExplicitType {
    Name(String),
    Pointer(Box<ExplicitType>),

    // Leave this to the type inference.
    Empty,
}