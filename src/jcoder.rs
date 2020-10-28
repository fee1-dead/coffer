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
use crate::error::{Error, Result};
use std::io::{Read, Seek, SeekFrom, Write};

/// Decoder for Big Endian Values. Does not support little endian.
///
/// ```
/// # use coffer::jcoder::JDecoder;
/// # use std::io::Cursor;
/// let mut buf : [u8; 30] = [
/// 0xBE, 0xEF,
/// 0xCA, 0xFE, 0xBA, 0xBE,
/// 0x70, 0x80, 0x20, 0x10, 0x20, 0x90, 0x24, 0x12,
/// 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F
/// ];
/// let mut cursor = Cursor::new(buf);
/// assert_eq!((&mut cursor).u16().unwrap(), 0xBEEFu16);
/// assert_eq!((&mut cursor).u32().unwrap(), 0xCAFEBABEu32);
/// assert_eq!((&mut cursor).u64().unwrap(), 0x7080201020902412u64);
/// assert_eq!((&mut cursor).u128().unwrap(), 0x101112131415161718191A1B1C1D1E1Fu128)
/// ```
pub struct Decoder<T: Read + Seek> {
    inner: T,
    pub(crate) idx: u64,
}
macro_rules! read_fn {
    ($type:ty, $fnName: ident, $bytesize:literal) => {
        fn $fnName(&mut self) -> Result<$type> {
            let mut buf = [0u8; $bytesize];
            let bytes = self.read(&mut buf)?;
            if bytes != $bytesize {
                Err(Error::EOF)
            } else {
                Ok(<$type>::from_be_bytes(buf))
            }
        }
    };
}
macro_rules! read_fn_all {
    ($($type:ty, $fnName: ident, $bytesize:literal),*) => ($( read_fn! { $type, $fnName, $bytesize } )*);
}
pub trait JDecoder: Read {
    read_fn_all! { u128, u128, 16, u64, u64, 8, u32, u32, 4, u16, u16, 2, u8, u8, 1, f32, f32, 4, f64, f64, 8, i8, i8, 1, i32, i32, 4, i64, i64, 8 }

    fn utf(&mut self) -> Result<String> {
        let length = self.u32()? as usize;
        let mut slice = vec![0u8; length];
        let length_read = self.read(&mut slice)?;
        if length_read < length {
            Err(Error::EOF)
        } else {
            Ok(crate::mod_utf8::modified_utf8_to_string(&slice)?)
        }
    }
}
macro_rules! write_fn {
    ($type:ty, $fnName: ident, $bytesize:literal) => {
        fn $fnName(&mut self, i: $type) -> Result<()> {
            let buf = i.to_be_bytes();
            let bytes = self.write(&buf)?;
            if bytes != $bytesize {
                Err(Error::EOF)
            } else {
                Ok(())
            }
        }
    };
}

macro_rules! write_fn_all {
    ($($type:ty, $fnName: ident, $bytesize:literal),*) => ($( write_fn! { $type, $fnName, $bytesize } )*);
}

impl<T> JDecoder for T where T: Read {}

pub trait JEncoder: Write {
    write_fn_all! { u128, u128, 16, u64, u64, 8, u32, u32, 4, u16, u16, 2, u8, u8, 1, f32, f32, 4, f64, f64, 8, i8, i8, 1, i32, i32, 4, i64, i64, 8 }

    fn utf(&mut self, str: &str) -> Result<String> {
        let slice = crate::mod_utf8::string_to_modified_utf8(str)?;
        let length = slice.len();
        let length_written = self.write(&slice)?;
        if length_written < length {
            Err(Error::EOF)
        } else {
            Ok(crate::mod_utf8::modified_utf8_to_string(&slice)?)
        }
    }
}