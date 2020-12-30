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

use std::io::{Read, Write, Result};

pub trait ReadWrite where Self: Sized {
    fn read_from<T: Read>(reader: &mut T) -> Result<Self>;
    fn write_to<T: Write>(&self, writer: &mut T) -> Result<()>;
}

macro_rules! impl_readwrite_ints {
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
impl_readwrite_ints! { u8, 1 i8, 1 u16, 2 i16, 2 u32, 4 i32, 4 u64, 8 i64, 8 u128, 16 i128, 16 }