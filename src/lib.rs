#![feature(seek_convenience)]

use num_enum::TryFromPrimitive;

pub mod constants;
pub mod index;
pub mod decoder;
pub mod error;

#[cfg(test)]
mod tests;

pub struct JClass {
    pub minor_version: u16,
    pub major_version: u16,
    pub constant_pool: Vec<JConstantEntry>,
    pub access: u16,
    pub this_class: u16,
    pub super_class: u16,
    pub interfaces: u16,
    pub fields: Vec<JField>,
    pub methods: Vec<JMethod>,
    pub attributes: Vec<JAttribute>,
}

///```
/// # use coffer::JEntryTag;
/// # use std::convert::TryFrom;
/// assert_eq!(JEntryTag::try_from(10u8), Ok(JEntryTag::MethodRef));
/// assert_eq!(JEntryTag::Long as u8, 5u8);
/// ```
#[repr(u8)]
#[derive(Debug, Eq, PartialEq, TryFromPrimitive)]
pub enum JEntryTag {
    UTF8 = 1,
    Int = 3,
    Float = 4,
    Long = 5,
    Double = 6,
    Class = 7,
    String = 8,
    FieldRef = 9,
    MethodRef = 10,
    InterfaceRef = 11,
    NameAndType = 12,
    MethodHandle = 15,
    MethodType = 16,
    Dynamic = 17,
    InvokeDynamic = 18,
    Module = 19,
    #[cfg(any(feature = "jvm9", feature = "jvm10", feature = "jvm11", feature = "jvm12", feature = "jvm13", feature = "jvm14", feature = "jvm15"))]
    Package = 20,
}


pub struct JConstantEntry {
    tag: JEntryTag,
    ext_data: Vec<u8>,
}

pub struct JField {
    access: u16,
    name: u16,
    descriptor: u16,
    attributes: Vec<JAttribute>,
}

pub struct JMethod {
    access: u16,
    name: u16,
    descriptor: u16,
    attributes: Vec<JAttribute>,
}

pub struct JAttribute {
    attribute_name: u16,
    ext_data: Vec<u8>,
}

