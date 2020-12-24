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
use std::io::Read;
use crate::error::{Error, Result};
use crate::byteswapper::ByteSwap;
use crate::jcoder::JDecoder;
use std::borrow::Cow;

const CONST_ENT_SIZE: usize = std::mem::size_of::<ConstantEntry>();

#[allow(dead_code)] // used in macro
#[inline]
fn constant_entry_transmute<'a, F>(tag: u8, func: F) -> Result<ConstantEntry<'a>> where F: FnOnce(&mut [u8; CONST_ENT_SIZE]) -> std::io::Result<()> {
    let mut slice = [0u8; CONST_ENT_SIZE];
    slice[0] = tag;
    func(&mut slice)?;
    Ok(unsafe { std::mem::transmute(slice) })
}

macro_rules! transmute_entry {
    ($self:ident, $tag:ident, $($buf_size:literal, $range_from:literal..$range_to:literal),+) => {
        crate::constant_pool::constant_entry_transmute($tag, |slice| {
            $(
                let mut buf = [0; $buf_size];
                $self.read_exact(&mut buf)?;
                buf.be_to_ne();
                slice[$range_from..$range_to].copy_from_slice(&buf);
            )+
            Ok(())
        })?
    };
}

trait ConstantEntryRead: Read + Sized {
    fn read_constant_entry(&mut self) -> Result<ConstantEntry> {
        let tag = self.read_u8()?;
        Ok(match tag {
            1 => {
                let utflen = self.read_u16()?;
                let mut buf = Vec::with_capacity(utflen as usize);
                self.read_exact(&mut buf)?;
                ConstantEntry::UTF8(crate::mod_utf8::modified_utf8_to_string(&buf)?)
            }
            3 | 4 => transmute_entry!(self, tag, 4, 4..8),
            5 | 6 => transmute_entry!(self, tag, 8, 8..16),
            7 | 8 | 16 | 19 | 20 => transmute_entry!(self, tag, 2, 2..4),
            9 | 10..=12 | 17 | 18 => transmute_entry!(self, tag, 2, 2..4, 2, 4..6),
            15 => transmute_entry!(self, tag, 1, 1..2, 2, 2..4),
            _ => return Err(Error::Unrecognized("Constant Entry Tag", tag.to_string()))
        })
    }
}

impl<T: Read + Sized> ConstantEntryRead for T {}

#[repr(u8)]
pub enum ConstantEntry<'a> {
    UTF8(Cow<'a, [u8]>) = 1,
    Integer(i32) = 3,
    Float(f32),
    Long(i64),
    Double(f64),
    Class(u16),
    String(u16),
    Field(u16, u16),
    Method(u16, u16),
    InterfaceMethod(u16, u16),
    NameAndType(u16, u16),
    MethodHandle(u8, u16) = 15,
    MethodType(u16),
    Dynamic(u16, u16),
    InvokeDynamic(u16, u16),
    Module(u16),
    Package(u16),
}
