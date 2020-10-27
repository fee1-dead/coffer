/*
    This file is part of Coffer.

    Coffer is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Coffer is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Coffer. (LICENSE.md)  If not, see <https://www.gnu.org/licenses/>.
*/
#![feature(assoc_char_funcs)]

#[macro_use]
extern crate bitflags;

use num_enum::TryFromPrimitive;

pub mod constants;
pub mod index;
pub mod decoder;
pub mod error;

#[cfg(test)]
mod tests;
mod mod_utf8;

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

#[derive(Debug)]
pub struct JConstantEntry {
    tag: JEntryTag,
    ext_data: Vec<u8>,
}

#[derive(Debug)]
pub struct JField {
    access: u16,
    name: u16,
    descriptor: u16,
    attributes: Vec<JAttribute>,
}

#[derive(Debug)]
pub struct JMethod {
    access: u16,
    name: u16,
    descriptor: u16,
    attributes: Vec<JAttribute>,
}

#[derive(Debug)]
pub struct JAttribute {
    attribute_name: u16,
    ext_data: Vec<u8>,
}

