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

extern crate self as coffer;

use std::io::{Read, Write};

use prelude::*;
pub use rw::*;

pub use crate::error::{Error, Result};

pub mod annotation;
pub mod attr;
pub mod code;
pub mod constants;
pub mod cp;
pub mod clazz;
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
