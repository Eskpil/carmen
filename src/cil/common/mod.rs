use core::fmt;
use serde::{Deserialize, Serialize};
use std::fmt::Formatter;
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Stage {
    Global,
    Local,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
pub enum Tag {
    NoMangle,
    External,
    Imported,
    Local,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
pub enum Endianness {
    Big,
    Little,
}

pub type Tags = Vec<Tag>;

impl From<String> for Tag {
    fn from(value: String) -> Self {
        match value.as_str() {
            "external" => Tag::External,
            "imported" => Tag::Imported,
            "no_mangle" => Tag::NoMangle,
            "local" => Tag::Local,
            o => todo!("throw not a tag error: {o}"),
        }
    }
}

impl fmt::Display for Tag {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Tag::Local => "Tag::Local",
            Tag::NoMangle => "Tag::NoMangle",
            Tag::Imported => "Tag::Imported",
            Tag::External => "Tag::External",
        };

        write!(f, "{name}")
    }
}

pub fn ensure_tag(tags: &mut Tags, tag: Tag) {
    if !tags.contains(&tag) {
        tags.push(tag);
    }
}

pub fn expand_tags(tags: &Tags) -> Tags {
    let mut new = vec![];
    new.extend(tags);

    if new.contains(&Tag::Imported) {
        ensure_tag(&mut new, Tag::NoMangle);
    }

    new
}

pub fn default_function_tags() -> Tags {
    let mut tags = vec![];

    tags.push(Tag::Local);

    tags
}
