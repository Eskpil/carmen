pub mod definitions;
pub use definitions::*;
use crate::cil::typecheck::type_id::TypeId;

#[derive(Debug, Clone)]
pub struct Signature {
    pub accepts: Vec<TypeId>,
    pub returns: TypeId,
}