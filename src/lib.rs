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
// #![warn(missing_docs)]
#![allow(missing_doc_code_examples)] // TODO Change these later to higher levels

#[macro_use]
extern crate bitflags;

#[macro_use]
extern crate coffer_macros;

use std::borrow::Cow;
use std::io::{Read, Write};

use prelude::*;
pub use rw::*;

use crate::constants::JVM_MAGIC;
pub use crate::error::Error;
pub use crate::error::Result;

pub mod annotation;
pub mod attr;
pub mod code;
pub mod constants;
pub mod cp;
pub mod dynamic;
pub mod error;
pub mod flags;
pub mod loadable;
pub mod member;
pub mod mod_utf8;
pub mod module;
pub mod prelude;
pub mod rw;
pub mod signature;
pub mod total_floats;
pub mod ty;
pub mod version;

pub(crate) mod insn;
#[cfg(test)]
mod tests;

#[derive(Debug, Clone)]
pub struct Class {
    pub version: JavaVersion,
    pub access: ClassFlags,
    pub name: Cow<'static, str>,
    /// The name of the super class for this class.
    /// when it is unspecified in the source code, it is `"java/lang/Object"`.
    ///
    /// Although most java classes have their own superclasses,
    /// java/lang/Object has no superclass.
    pub super_name: Option<Cow<'static, str>>,
    pub interfaces: Vec<Cow<'static, str>>,
    pub fields: Vec<Field>,
    pub methods: Vec<Method>,
    pub attributes: Vec<ClassAttribute>,
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
    #[str_type(Class)]
    pub interfaces: Vec<Cow<'static, str>>,
    #[vec_len_type(u16)]
    pub fields: Vec<Field>,
    #[vec_len_type(u16)]
    pub methods: Vec<Method>,
    #[vec_len_type(u16)]
    pub attributes: Vec<ClassAttribute>,
}

impl ReadWrite for Class {
    fn read_from<T: Read>(reader: &mut T) -> Result<Self> {
        match u32::read_from(reader)? {
            JVM_MAGIC => {
                let version = JavaVersion::read_from(reader)?;
                let mut cp = MapCp::read_from(reader)?;
                let c = ClassWrapper::read_from(&mut cp, reader)?;
                for attr in &c.attributes {
                    if let ClassAttribute::BootstrapMethods(b) = attr {
                        cp.bootstrap_methods(b)?;
                        break;
                    }
                }
                Ok(Class {
                    version,
                    access: c.access,
                    name: c.name,
                    super_name: c.super_name,
                    interfaces: c.interfaces,
                    fields: c.fields,
                    methods: c.methods,
                    attributes: c.attributes,
                })
            }
            n => Err(Error::Invalid("class header", n.to_string().into())),
        }
    }

    fn write_to<T: Write>(&self, writer: &mut T) -> Result<()> {
        let mut bsm = vec![];
        for a in &self.attributes {
            if let ClassAttribute::BootstrapMethods(b) = a {
                bsm.extend_from_slice(b);
            }
        }
        let mut cp = VecCp::new();
        cp.bsm = bsm;

        JVM_MAGIC.write_to(writer)?;
        self.version.write_to(writer)?;

        let mut buf = vec![];
        self.access.write_to(&mut buf)?;
        cp.insert_class(self.name.clone()).write_to(&mut buf)?;
        self.super_name
            .as_ref()
            .map_or(0, |n| cp.insert_class(n.clone()))
            .write_to(&mut buf)?;
        (self.interfaces.len() as u16).write_to(&mut buf)?;
        for i in &self.interfaces {
            cp.insert_class(i.clone()).write_to(&mut buf)?;
        }
        (self.fields.len() as u16).write_to(&mut buf)?;
        for f in &self.fields {
            f.write_to(&mut cp, &mut buf)?;
        }
        (self.methods.len() as u16).write_to(&mut buf)?;
        for m in &self.methods {
            m.write_to(&mut cp, &mut buf)?;
        }
        (self.attributes.len() as u16).write_to(&mut buf)?;
        for a in &self.attributes {
            match a {
                ClassAttribute::BootstrapMethods(_) => {}
                _ => a.write_to(&mut cp, &mut buf)?,
            }
        }
        if !cp.bsm.is_empty() {
            let mut i: u16 = 0;
            let mut buf2 = vec![];
            while !cp.bsm.is_empty() {
                let v = cp.bsm;
                cp.bsm = vec![];
                i += v.len() as u16;
                for bsm in v {
                    bsm.write_to(&mut cp, &mut buf2)?;
                }
            }
            write_to!(&Cow::Borrowed("BootstrapMethods"), &mut cp, &mut buf)?;
            (buf2.len() as u32).write_to(&mut buf)?;
            i.write_to(&mut buf)?;
            buf.write_all(&buf2)?;
        }
        cp.write_to(writer)?;
        writer.write_all(&buf)?;
        Ok(())
    }
}
