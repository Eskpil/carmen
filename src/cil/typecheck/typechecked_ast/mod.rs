pub mod definitions;
use crate::cil::typecheck::type_id::TypeId;
pub use definitions::*;

#[derive(Debug, Clone)]
pub struct Signature {
    pub accepts: Vec<TypeId>,
    pub returns: TypeId,
}
