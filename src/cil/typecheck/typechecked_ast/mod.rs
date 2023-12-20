pub mod definitions;
pub use definitions::*;
use crate::ast;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Usize,
    U8,
    Pointer(Box<Type>)
}

#[derive(Debug, Clone)]
pub struct Signature {
    pub accepts: Vec<Type>,
    pub returns: Vec<Type>,
}

pub fn convert_type(explicit_type: &ast::definitions::ExplicitType) -> Type {
    let typ =match explicit_type {
        ast::definitions::ExplicitType::Name(name) => {
            match name.as_str() {
                "usize" => Type::Usize,
                "u8" => Type::U8,
                other => todo!("implement explicit type: {other}")
            }
        }
        ast::definitions::ExplicitType::Pointer(to) => {
            Type::Pointer(Box::new(convert_type(&*to)))
        }
        other => todo!("implement type: {:?}", other)
    };

    typ
}
