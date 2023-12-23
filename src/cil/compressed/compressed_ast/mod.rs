mod definitions;
pub use definitions::*;

#[derive(Debug, Clone)]
pub struct Integer {
    pub signed: bool,
    pub byte_size: usize,
}

#[derive(Debug, Clone)]
pub enum Type {
    Integer(Integer),
}

#[derive(Debug, Clone)]
pub struct Signature {
    pub returns: Type,
    pub accepts: Vec<Type>,
}