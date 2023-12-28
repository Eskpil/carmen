use crate::ast::definitions::ExplicitType;
use std::any::Any;

pub mod aliases {
    pub const USIZE: &str = "usize";
}

pub trait Type {
    fn size(&self) -> usize;
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum TypeError {
    NotFound(String),
}

pub type TypeResult<T> = Result<T, TypeError>;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum Tag {
    Primitive,

    Pointer,
    Alias,

    Slice,
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum Primitive {
    U8,
    U16,
    U32,
    U64,

    I8,
    I16,
    I32,
    I64,

    Bool,

    Void,

    Unknown,
}

#[derive(Debug, Clone)]
pub struct Slice {
    pub of: Box<TypeId>,
    pub size: usize,
}

#[derive(Debug, Clone)]
pub struct Alias {
    pub name: String,
    pub to: Box<TypeId>,
}

#[derive(Debug, Clone)]
pub struct Pointer {
    pub to: Box<TypeId>,
}

#[derive(Debug)]
pub struct TypeId {
    pub id: u32,
    pub tag: Tag,
    pub d: Box<dyn Any>,
}

#[derive(Debug, Clone)]
pub struct TypePool {
    pub types: Vec<TypeId>,

    incrementer: u32,
}

impl Primitive {}

impl Type for Primitive {
    fn size(&self) -> usize {
        match *self {
            Primitive::U8 => 1,
            Primitive::U16 => 2,
            Primitive::U32 => 4,
            Primitive::U64 => 8,

            Primitive::I8 => 1,
            Primitive::I16 => 2,
            Primitive::I32 => 4,
            Primitive::I64 => 8,

            Primitive::Bool => 8,

            Primitive::Void => 0,

            Primitive::Unknown => unreachable!(),
        }
    }
}

impl From<String> for Primitive {
    fn from(value: String) -> Self {
        match value.as_str() {
            "u8" => Primitive::U8,
            "u16" => Primitive::U16,
            "u32" => Primitive::U32,
            "u64" => Primitive::U64,

            "i8" => Primitive::I8,
            "i16" => Primitive::I16,
            "i32" => Primitive::I32,
            "i64" => Primitive::I64,

            "bool" => Primitive::Bool,

            "void" => Primitive::Void,

            _ => Primitive::Unknown,
        }
    }
}

impl Alias {
    pub fn new(name: String, to: Box<TypeId>) -> Self {
        Self { name, to }
    }
}

impl Pointer {
    pub fn new(to: Box<TypeId>) -> Self {
        assert!(!to.is_alias());

        Self { to }
    }
}

impl Slice {
    pub fn new(of: Box<TypeId>, size: usize) -> Self {
        Self { of, size }
    }
}

impl TypeId {
    #[inline]
    pub fn is_primitive(&self) -> bool {
        self.tag == Tag::Primitive
    }

    #[inline]
    pub fn is_alias(&self) -> bool {
        self.tag == Tag::Alias
    }

    #[inline]
    pub fn is_pointer(&self) -> bool {
        self.tag == Tag::Pointer
    }

    #[inline]
    pub fn is_void(&self) -> bool {
        if !self.is_primitive() {
            return false;
        }

        self.to_primitive() == Primitive::Void
    }

    #[inline]
    pub fn is_slice(&self) -> bool {
        self.tag == Tag::Slice
    }

    #[inline]
    pub fn is_integer_class(&self) -> bool {
        if self.is_void() {
            false
        } else {
            match self.tag {
                Tag::Alias => {
                    let alias = self.to_alias();
                    alias.to.is_integer_class()
                }
                Tag::Primitive => true,
                _ => false,
            }
        }
    }

    pub fn size(&self) -> usize {
        match self.tag {
            Tag::Primitive => {
                let primitive = self.to_primitive();
                primitive.size()
            }
            Tag::Pointer => Primitive::U64.size(),
            Tag::Alias => {
                let alias = self.to_alias();
                alias.to.size()
            }
            // TODO: Should the type info contain the slice slice?
            Tag::Slice => {
                let slice = self.to_slice();
                slice.of.size() * slice.size
            }
        }
    }

    pub fn for_primitive(id: u32, primitive: Primitive) -> TypeId {
        TypeId {
            id,
            tag: Tag::Primitive,
            d: Box::new(primitive),
        }
    }

    pub fn to_primitive(&self) -> Primitive {
        assert!(self.is_primitive());
        *self.d.downcast_ref::<Primitive>().unwrap()
    }

    pub fn for_alias(id: u32, alias: Alias) -> TypeId {
        TypeId {
            id,
            tag: Tag::Alias,
            d: Box::new(alias),
        }
    }

    pub fn to_alias(&self) -> Alias {
        assert!(self.is_alias());
        self.d.downcast_ref::<Alias>().unwrap().clone()
    }

    pub fn for_pointer(id: u32, pointer: Pointer) -> TypeId {
        TypeId {
            id,
            tag: Tag::Pointer,
            d: Box::new(pointer),
        }
    }

    pub fn to_pointer(&self) -> Pointer {
        assert!(self.is_pointer());
        self.d.downcast_ref::<Pointer>().unwrap().clone()
    }

    pub fn for_slice(id: u32, slice: Slice) -> TypeId {
        TypeId {
            id,
            tag: Tag::Slice,
            d: Box::new(slice),
        }
    }

    pub fn to_slice(&self) -> Slice {
        assert!(self.is_slice());
        self.d.downcast_ref::<Slice>().unwrap().clone()
    }
}

impl Clone for TypeId {
    fn clone(&self) -> Self {
        let data: Box<dyn Any> = match self.tag {
            Tag::Primitive => {
                let primitive = self.to_primitive();
                Box::new(primitive)
            }
            Tag::Alias => {
                let alias = self.to_alias();
                Box::new(alias)
            }
            Tag::Pointer => {
                let pointer = self.to_pointer();
                Box::new(pointer)
            }
            Tag::Slice => {
                let slice = self.to_slice();
                Box::new(slice)
            }
        };

        TypeId {
            id: self.id,
            tag: self.tag,
            d: data,
        }
    }
}

impl PartialEq for TypeId {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for TypeId {}

impl TypePool {
    pub fn new() -> Self {
        let mut pool = Self {
            types: vec![],
            incrementer: 0,
        };
        pool.generate_primitives();
        pool.generate_aliases();
        pool
    }

    pub(crate) fn push_slice(&mut self, slice: Slice) -> TypeId {
        let id = self.next_id();
        let type_id = TypeId::for_slice(id, slice);
        self.push(type_id.clone());
        type_id
    }

    fn push(&mut self, type_id: TypeId) {
        self.types.push(type_id);
    }

    fn generate_primitive(&mut self, primitive: Primitive) {
        let id = self.next_id();
        let type_id = TypeId::for_primitive(id, primitive);
        self.types.push(type_id.clone());
        let id = self.next_id();
        self.types
            .push(TypeId::for_pointer(id, Pointer::new(Box::new(type_id))))
    }

    fn generate_primitives(&mut self) {
        self.generate_primitive(Primitive::Void);

        self.generate_primitive(Primitive::U8);
        self.generate_primitive(Primitive::U16);
        self.generate_primitive(Primitive::U32);
        self.generate_primitive(Primitive::U64);

        self.generate_primitive(Primitive::I8);
        self.generate_primitive(Primitive::I16);
        self.generate_primitive(Primitive::I32);
        self.generate_primitive(Primitive::I64);

        self.generate_primitive(Primitive::Bool);
    }

    fn generate_usize_alias(&mut self) {
        let u64_id = self.find_primitive(&Primitive::U64).unwrap();
        let id = self.next_id();
        self.types.push(TypeId::for_alias(
            id,
            Alias::new("usize".to_string(), Box::new(u64_id.clone())),
        ));
    }

    fn generate_aliases(&mut self) {
        self.generate_usize_alias();
    }

    pub fn find_pointer(&self, to: TypeId) -> Option<TypeId> {
        self.types
            .iter()
            .find(|id| id.is_pointer() && *id.to_pointer().to == to)
            .cloned()
    }

    pub fn find_primitive(&self, primitive: &Primitive) -> Option<TypeId> {
        self.types
            .iter()
            .find(|id| id.is_primitive() && id.to_primitive() == *primitive)
            .cloned()
    }

    pub fn find_alias(&self, name: &str) -> Option<TypeId> {
        self.types
            .iter()
            .find(|id| id.is_alias() && id.to_alias().name == name)
            .cloned()
    }

    pub fn find(&self, name: String) -> TypeResult<TypeId> {
        let primitive = Primitive::from(name.clone());
        if primitive != Primitive::Unknown {
            let type_id = self.find_primitive(&primitive).unwrap();
            return Ok(type_id);
        }

        // TODO: Check user defined types structs/enums.

        let alias = self.find_alias(&name);
        if let Some(alias) = alias {
            return Ok(alias);
        }

        Err(TypeError::NotFound(format!("type: {name} not found")))
    }

    pub fn find_explicit_type(&self, explicit_type: &ExplicitType) -> TypeResult<TypeId> {
        match explicit_type {
            ExplicitType::Name(name) => self.find(name.clone()),
            ExplicitType::Pointer(to) => Ok(self
                .find_pointer(self.find_explicit_type(&to.to).unwrap())
                .unwrap()),
            ExplicitType::Empty => Ok(self.find_primitive(&Primitive::Void).expect("no void?")),
            o => todo!("implement: {:?}", o),
        }
    }

    pub fn next_id(&mut self) -> u32 {
        self.next().unwrap()
    }
}

impl Iterator for TypePool {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        let id = self.incrementer;
        self.incrementer += 1;
        Some(id)
    }
}
