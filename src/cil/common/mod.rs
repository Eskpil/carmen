#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Stage {
    Global,
    Local,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Tag {
    NoMangle,
    External,
}