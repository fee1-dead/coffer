/*
    This file is part of Coffer.

    Coffer is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Coffer is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Coffer. (LICENSE.md)  If not, see <https://www.gnu.org/licenses/>.
*/
#![feature(seek_convenience)]
#![feature(arbitrary_enum_discriminant)]

#[macro_use]
extern crate bitflags;

#[macro_use]
extern crate coffer_proc_macros;

pub mod constants;
pub mod index;
pub mod jcoder;
pub mod error;
pub mod insn;
pub mod mod_utf8;
pub mod constant_pool;
pub mod full;
pub mod access;

#[cfg(test)]
mod tests;
pub(crate) mod byteswapper;

use std::io::{Read, Write};
pub use crate::error::Result;
use crate::full::{Type, BootstrapMethod, Constant};
use std::borrow::Cow;

/// The generic read and write trait. This indicates a structure can be read without additional contextual information.
///
/// This trait can be derived with #[derive(ReadWrite)] if all the types it can hold are also `ReadWrite`.
/// Furthermore, all integer types implement `ReadWrite`.
pub trait ReadWrite where Self: Sized {
    fn read_from<T: Read>(reader: &mut T) -> Result<Self>;
    fn write_to<T: Write>(&self, writer: &mut T) -> Result<()>;
}

/// A trait for writing constant pool entries.
pub trait ConstantPoolWriter {
    fn insert_constant(&mut self, value: &Constant) -> u16;
    /// InvokeDynamic and Dynamic is distinguished by the [Type](crate::Type) enum.
    fn insert_dynamic(&mut self, bsm: &BootstrapMethod, name: &str, ty: Type) -> u16;
    /// insert an indirect string such as String / Module / Package entry.
    fn insert_indirect_str(&mut self, tag: u8, st: &str) -> u16;
    fn insert_utf8(&mut self, st: &str) -> u16;
    fn insert_nameandtype(&mut self, name: &str, descriptor: &str) -> u16 {
        let a = self.insert_utf8(name);
        let b = self.insert_utf8(descriptor);
        self.nameandtype(a, b)
    }
    fn nameandtype(&mut self, name: u16, descriptor: u16) -> u16;
}

/// A trait for reading constant pool entries from ind
pub trait ConstantPoolReader {
    fn read_raw(&mut self, idx: u16) -> Option<crate::full::cp::RawConstantEntry>;
    fn read_nameandtype(&mut self, idx: u16) -> Option<(Cow<str>, Cow<str>)>;
    fn read_constant(&mut self, idx: u16) -> Option<Constant>;
    fn read_utf8(&mut self, idx: u16) -> Option<Cow<str>>;
}

/// The read and write trait where the structure must be read with constant pool information, and will insert constant entries into the the constant pool when writing.
///
/// Types that implement [ReadWrite](crate::ReadWrite) will implement this trait,
/// but it doesn't always delegate to the generic functions (for example, i32 can be read from bytes, but it can also be read as index to an Integer entry of the constant pool).
pub trait ConstantPoolReadWrite where Self: Sized {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self>;
    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> Result<()>;
}

macro_rules! impl_readwrite_nums {
    ($($i:ty, $s:literal)*) => {
        $(
            impl ReadWrite for $i {
                fn read_from<T: Read>(reader: &mut T) -> Result<Self> {
                    let mut bytes = [0u8; $s];
                    reader.read_exact(&mut bytes)?;
                    Ok(<$i>::from_be_bytes(bytes))
                }
                fn write_to<T: Write>(&self, writer: &mut T) -> Result<()> {
                    writer.write_all(&self.to_be_bytes())?;
                    Ok(())
                }
            }
        )*
    };
}
impl_readwrite_nums! { u8, 1  i8, 1  u16, 2  i16, 2  u32, 4  i32, 4  f32, 4  u64, 8  i64, 8  f64, 8  u128, 16  i128, 16 }

impl ReadWrite for String {
    fn read_from<T: Read>(reader: &mut T) -> Result<Self> {
        let length = u16::read_from(reader)?;
        let mut buf = Vec::with_capacity(length as usize);
        reader.read_exact(&mut buf)?;
        Ok(crate::mod_utf8::modified_utf8_to_string(&buf)?)
    }

    fn write_to<T: Write>(&self, writer: &mut T) -> Result<()> {
        let string = crate::mod_utf8::string_to_modified_utf8(self);
        (string.len() as u16).write_to(writer)?;
        writer.write_all(&string)?;
        Ok(())
    }
}