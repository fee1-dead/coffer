/*
 *     This file is part of Coffer.
 *
 *     Coffer is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     Coffer is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with Coffer. (LICENSE.md)  If not, see <https://www.gnu.org/licenses/>.
 */
//! Coffer is a lightweight and fast library for reading and writing Java classes.
//!
//! Read and write functions are exposed via the traits [`ReadWrite`] and [`ConstantPoolReadWrite`] when the information from the constant pool is needed to get the information.
//!
//! [`ReadWrite`] uses instances of `Read` and `Write`, and [`ConstantPoolReadWrite`] uses additional parameters: instances of [`ConstantPoolWriter`] and [`ConstantPoolReader`].
//! These traits represent read and write access to the constant pool.
//!
//! Many implementors of [`ReadWrite`] and [`ConstantPoolReadWrite`] uses a derive macro internally to avoid repeating implementation for structures that just calls the trait functions of its fields.
//!
//! [`ReadWrite`]: crate::ReadWrite
//! [`ConstantPoolReadWrite`]: crate::ConstantPoolReadWrite
//! [`ConstantPoolWriter`]: crate::ConstantPoolWriter
//! [`ConstantPoolReader`]: crate::ConstantPoolReader
#![cfg_attr(any(feature = "backtrace", test), feature(backtrace))]
#![warn(missing_docs)]

#[macro_use]
extern crate bitflags;

#[macro_use]
extern crate coffer_macros;

use prelude::*;

pub use coffer_macros::*;

use std::borrow::Cow;
use std::io::{Read, Write};

pub use crate::error::Error;
pub use crate::error::Result;

pub mod constants;
pub mod error;

pub mod mod_utf8;
pub mod full;
pub mod flags;
pub mod member;
pub mod prelude;
pub mod ty;
pub mod signature;
pub mod loadable;
pub mod attr;
pub mod version;
pub mod rw;
pub mod module;

#[cfg(test)]
mod tests;
pub(crate) mod insn;


pub use rw::*;
use rw::Clazz;

#[derive(Debug, Clone)]
pub struct Class {
    pub version: JavaVersion,
    pub access: ClassFlags,
    pub name: Cow<'static, str>,
    /// java/lang/Object has no superclass.
    pub super_name: Option<Cow<'static, str>>,
    pub interfaces: Vec<Cow<'static, str>>,
    pub fields: Vec<Field>,
    pub methods: Vec<Method>,
    pub attributes: Vec<ClassAttribute>
}

#[derive(ConstantPoolReadWrite)]
struct ClassWrapper {
    #[use_normal_rw]
    pub access: ClassFlags,
    #[str_type(Class)]
    pub name: Cow<'static, str>,
    #[str_optional]
    #[str_type(Class)]
    pub super_name: Option<Cow<'static, str>>,
    #[vec_len_type(u16)]
    pub interfaces: Vec<Clazz>,
    #[vec_len_type(u16)]
    pub fields: Vec<Field>,
    #[vec_len_type(u16)]
    pub methods: Vec<Method>,
    #[vec_len_type(u16)]
    pub attributes: Vec<ClassAttribute>
}

impl ReadWrite for Class {
    fn read_from<T: Read>(reader: &mut T) -> Result<Self, Error> {
        match u32::read_from(reader)? {
            0xCAFEBABE => {
                let ver = JavaVersion::read_from(reader)?;

                unimplemented!()
            }
            n => Err(Error::Invalid("class header", n.to_string().into()))
        }
    }

    fn write_to<T: Write>(&self, writer: &mut T) -> Result<(), Error> {
        unimplemented!()
    }
}