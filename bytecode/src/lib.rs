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

#[macro_use]
extern crate bitflags;

#[macro_use]
extern crate coffer_macros;

pub use coffer_macros::*;

use std::borrow::Cow;
use std::io::{Read, Write};

pub use crate::error::Error;
pub use crate::error::Result;
use crate::full::{BootstrapMethod, Constant, MemberRef, Type, Catch, Dynamic, Label, OrDynamic, MethodHandleKind};
use crate::full::cp::RawConstantEntry;

pub mod constants;
pub mod error;

pub mod mod_utf8;
pub mod full;
pub mod access;

#[cfg(test)]
mod tests;
pub(crate) mod insn;

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
    fn insert_raw(&mut self, value: RawConstantEntry) -> u16;
    fn insert_constant(&mut self, value: Constant) -> u16 {
        match value {
            Constant::I32(i) => self.insert_int(i),
            Constant::F32(f) => self.insert_float(f),
            Constant::I64(l) => self.insert_long(l),
            Constant::F64(d) => self.insert_double(d),
            Constant::String(s) => self.insert_indirect_str(8, s),
            Constant::Class(s) => self.insert_indirect_str(7, s),
            Constant::Member(mem) => self.insert_member(mem),
            Constant::MethodType(t) => {
                let desc = self.insert_utf8(t.to_string());
                self.insert_raw(RawConstantEntry::MethodType(desc))
            },
            Constant::MethodHandle(h) => {
                let mem = self.insert_member(h.member);
                self.insert_raw(RawConstantEntry::MethodHandle(unsafe { *(&h.kind as *const MethodHandleKind as *const u8) }, mem))
            }
        }
    }
    fn insert_ordynamic<T, F>(&mut self, or_dyn: OrDynamic<T>, f: F) -> u16 where F: FnOnce(&mut Self, T) -> u16 {
        match or_dyn {
            OrDynamic::Dynamic(d) => self.insert_dynamic(d),
            OrDynamic::Static(t) => f(self, t)
        }
    }
    fn insert_bsm(&mut self, bsm: BootstrapMethod) -> u16;
    /// InvokeDynamic and Dynamic is distinguished by the [Type](crate::Type) enum.
    fn insert_dynamic(&mut self, d: Dynamic) -> u16 {
        let bsm = self.insert_bsm(*d.bsm);
        let e = if d.descriptor.is_method() {
            RawConstantEntry::InvokeDynamic
        } else {
            RawConstantEntry::Dynamic
        };
        let name_and_type = self.insert_nameandtype(d.name, d.descriptor);
        self.insert_raw(e(bsm, name_and_type))
    }
    /// insert an indirect string such as String / Module / Package entry, used by the procedural macro.
    fn insert_indirect_str<T: Into<Cow<'static, str>>>(&mut self, tag: u8, st: T) -> u16 {
        let str_ref = self.insert_utf8(st.into().into_owned());
        self.insert_raw(match tag {
            7 => RawConstantEntry::Class,
            8 => RawConstantEntry::String,
            19 => RawConstantEntry::Module,
            20 => RawConstantEntry::Package,
            _ => {
                #[cfg(debug_assertions)]
                panic!("invalid tag for indirect string: {}", tag);

                #[cfg(not(debug_assertions))]
                unsafe { std::hint::unreachable_unchecked() }
            }
        }(str_ref))
    }
    fn insert_utf8<T: Into<Cow<'static, str>>>(&mut self, st: T) ->  u16 {
        self.insert_raw(RawConstantEntry::UTF8(st.into()))
    }
    fn insert_nameandtype<T: Into<Cow<'static, str>>, T2: Into<Cow<'static, str>>>(&mut self, name: T, descriptor: T2) -> u16 {
        let a = self.insert_utf8(name);
        let b = self.insert_utf8(descriptor);
        self.insert_raw(RawConstantEntry::NameAndType(a, b))
    }
    fn insert_class<T: Into<Cow<'static, str>>>(&mut self, c: T) -> u16 {
        let idx = self.insert_utf8(c);
        self.insert_raw(RawConstantEntry::Class(idx))
    }
    fn insert_int(&mut self, i: i32) -> u16 {
        self.insert_raw(RawConstantEntry::Int(i))
    }
    fn insert_long(&mut self, l: i64) -> u16 {
        self.insert_raw(RawConstantEntry::Long(l))
    }
    fn insert_float(&mut self, f: f32) -> u16 {
        self.insert_raw(RawConstantEntry::Float(f))
    }
    fn insert_double(&mut self, d: f64) -> u16 {
        self.insert_raw(RawConstantEntry::Double(d))
    }
    fn insert_member(&mut self, mem: MemberRef) -> u16 {
        let entry = match (&mem.descriptor, mem.itfs) {
            (Type::Method(..), true) => RawConstantEntry::InterfaceMethod,
            (Type::Method(..), false) => RawConstantEntry::Method,
            _ => RawConstantEntry::Field
        }(self.insert_class(mem.owner), self.insert_nameandtype(mem.name, mem.descriptor));
        self.insert_raw(entry)
    }
    /// map a label to the actual offset in the code array.
    /// this is not implemented by default, and it will be defined in a wrapper type in implementation of ConstantPoolReadWrite for `Code`.
    #[inline]
    fn label(&mut self, _lbl: &Label) -> u16 {
        #[cfg(debug_assertions)]
            unreachable!();
        #[cfg(not(debug_assertions))]
        unsafe {
            core::hint::unreachable_unchecked();
        }
    }

    /// Get the index of the catch.
    #[inline]
    fn catch(&mut self, _catch: &Catch) -> Option<u16> {
        #[cfg(debug_assertions)]
        unreachable!();
        #[cfg(not(debug_assertions))]
            unsafe {
            core::hint::unreachable_unchecked();
        }
    }
}

/// A trait for reading constant pool entries.
pub trait ConstantPoolReader {
    fn read_raw(&mut self, idx: u16) -> Option<crate::full::cp::RawConstantEntry>;
    fn read_nameandtype(&mut self, idx: u16) -> Option<(Cow<'static, str>, Type)> {
        match self.read_raw(idx) {
            Some(RawConstantEntry::NameAndType(n, t)) => self.read_utf8(n).and_then(|n| self.read_utf8(t).as_deref().map(str::parse).and_then(Result::ok).map(|t| (n, t))),
            _ => None
        }
    }
    fn read_constant(&mut self, idx: u16) -> Option<Constant> {
        match self.read_raw(idx) {
            Some(RawConstantEntry::Int(i)) => Some(Constant::I32(i)),
            Some(RawConstantEntry::Long(l)) => Some(Constant::I64(l)),
            Some(RawConstantEntry::Float(f)) => Some(Constant::F32(f)),
            Some(RawConstantEntry::Double(d)) => Some(Constant::F64(d)),
            _ => self.read_member(idx).map(Constant::Member)
        }
    }
    fn read_int(&mut self, idx: u16) -> Option<i32> {
        match self.read_raw(idx) {
            Some(RawConstantEntry::Int(i)) => Some(i),
            _ => None
        }
    }
    fn read_long(&mut self, idx: u16) -> Option<i64> {
        match self.read_raw(idx) {
            Some(RawConstantEntry::Long(l)) => Some(l),
            _ => None
        }
    }
    fn read_float(&mut self, idx: u16) -> Option<f32> {
        match self.read_raw(idx) {
            Some(RawConstantEntry::Float(f)) => Some(f),
            _ => None
        }
    }
    fn read_double(&mut self, idx: u16) -> Option<f64> {
        match self.read_raw(idx) {
            Some(RawConstantEntry::Double(d)) => Some(d),
            _ => None
        }
    }

    fn read_indirect_str(&mut self, tag: u8, idx: u16) -> Option<Cow<'static, str>> {
        self.read_raw(idx).map(|c| match c {
            RawConstantEntry::Module(u) if tag == 19 => self.read_utf8(u),
            RawConstantEntry::Package(u) if tag == 20 => self.read_utf8(u),
            RawConstantEntry::String(u) if tag == 8 => self.read_utf8(u),
            RawConstantEntry::Class(u) if tag == 7 => self.read_utf8(u),
            _ => None
        }).flatten().map(Into::into)
    }

    fn read_class(&mut self, idx: u16) -> Option<Cow<'static, str>> {
        self.read_indirect_str(7, idx)
    }

    fn read_utf8(&mut self, idx: u16) -> Option<Cow<'static, str>> {
        match self.read_raw(idx) {
            Some(RawConstantEntry::UTF8(s)) => Some(s),
            _ => None
        }
    }
    fn read_or_dynamic<T, F>(&mut self, idx: u16, f: F) -> Option<OrDynamic<T>> where F: FnOnce(&mut Self, u16) -> Option<T> {
        let dy = self.read_dynamic(idx);
        if let Some(d) = dy {
            Some(OrDynamic::Dynamic(d))
        } else {
            f(self, idx).map(OrDynamic::Static)
        }
    }
    fn read_invokedynamic(&mut self, idx: u16) -> Option<Dynamic> {
        match self.read_raw(idx) {
            Some(RawConstantEntry::InvokeDynamic(s, a)) =>  {
                let mut ptr = unsafe { Box::from_raw(std::ptr::NonNull::dangling().as_ptr()) };
                let (name, descriptor) = self.read_nameandtype(a)?;
                self.resolve_later(s, &mut ptr);
                Some(Dynamic {
                    bsm: ptr,
                    name, descriptor
                })
            },
            _ => None
        }
    }
    fn read_dynamic(&mut self, idx: u16) -> Option<Dynamic> {
        match self.read_raw(idx) {
            Some(RawConstantEntry::Dynamic(s, a)) =>  {
                let mut ptr = unsafe { Box::from_raw(std::ptr::NonNull::dangling().as_ptr()) };
                let (name, descriptor) = self.read_nameandtype(a)?;
                self.resolve_later(s, &mut ptr);
                Some(Dynamic {
                    bsm: ptr,
                    name, descriptor
                })
            },
            _ => None
        }
    }
    fn read_member(&mut self, idx: u16) -> Option<MemberRef> {
        match self.read_raw(idx) {
            Some(RawConstantEntry::Method(o, nt)) |
            Some(RawConstantEntry::Field(o, nt)) =>
                self.read_indirect_str(7, o)
                    .and_then(|o|
                        self.read_nameandtype(nt)
                            .map(|(n,t)| (o, n, t)))
                    .map(|(o, n, t)| MemberRef { owner: o, name: n, descriptor: t, itfs: false }),
            Some(RawConstantEntry::InterfaceMethod(o, nt)) => self.read_indirect_str(7, o)
                .and_then(|o|
                    self.read_nameandtype(nt)
                        .map(|(n,t)| (o, n, t)))
                .map(|(o, n, t)| MemberRef { owner: o, name: n, descriptor: t, itfs: true }),
            _ => None
        }
    }


    fn resolve_later(&mut self, bsm_idx: u16, ptr: &mut Box<BootstrapMethod>);

    // Implementations from Code

    /// get a uniquely identified label from an actual offset of the code array.
    /// The label is lazily inserted into the Code vector.
    #[inline]
    fn get_label(&mut self, _idx: u32) -> Label {
        #[cfg(debug_assertions)]
        unimplemented!();
        #[cfg(not(debug_assertions))]
        unsafe {
            core::hint::unreachable_unchecked();
        }
    }

    /// Get a catch based on the element index. This is implemented using a wrapper when reading the `Code` struct.
    #[inline]
    fn get_catch(&mut self, _idx: u16) -> Option<Catch> {
        #[cfg(debug_assertions)]
        unimplemented!();
        #[cfg(not(debug_assertions))]
            unsafe {
            core::hint::unreachable_unchecked();
        }
    }
}

/// The read and write trait where the structure must be read with constant pool information, and will insert constant entries into the the constant pool when writing.
///
/// Types that implement [ReadWrite](crate::ReadWrite) will implement this trait,
/// but it doesn't always delegate to the generic functions (for example, i32 can be read from bytes, but it can also be read as index to an Integer entry of the constant pool).
pub trait ConstantPoolReadWrite where Self: Sized {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self>;
    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> Result<()>;
}

#[macro_export]
/// helpful macro to return an error if the constant entry is not found or occupied by a double-sized entry.
macro_rules! try_cp_read {
    ($cp: ident, $reader: ident, $fn: ident) => ({
        let idx = $crate::ReadWrite::read_from($reader)?;
        $crate::try_cp_read!(idx, $cp.$fn(idx))
    });

    ($idx: expr, $opt: expr) => ({
        match $opt {
            Some(s) => Ok(s),
            None => Err($crate::error::Error::Invalid("Constant pool entry index", Into::into($idx.to_string())))
        }
    })
}

#[macro_export]
macro_rules! try_cp_read_idx {
    ($cp: ident, $idx: expr, $fn: ident) => ({
        $crate::try_cp_read!($idx, $cp.$fn($idx))
    });
}

/// Helper macro to disambigurate two implementations for one type.
#[macro_export]
macro_rules! read_from {
    ($writer: expr) => {
        $crate::ReadWrite::read_from($writer)
    };
    ($cp: expr, $writer: expr) => {
        $crate::ConstantPoolReadWrite::read_from($cp, $writer)
    };
}

/// Helper macro to disambigurate two implementations for one type.
#[macro_export]
macro_rules! write_to {
    ($self: expr, $writer:expr) => {
        $crate::ReadWrite::write_to($self, $writer)
    };
    ($self: expr, $cp: expr, $writer: expr) => {
        $crate::ConstantPoolReadWrite::write_to($self, $cp, $writer)
    };
}

macro_rules! impl_readwrite_nums {
    ($(($i:ty, $s:literal)),*) => {
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

impl_readwrite_nums! { (u8, 1),  (i8, 1),  (u16, 2),  (i16, 2),  (u32, 4),  (i32, 4),  (f32, 4),  (u64, 8),  (i64, 8),  (f64, 8),  (u128, 16),  (i128, 16) }

impl ReadWrite for String {
    fn read_from<T: Read>(reader: &mut T) -> Result<Self> {
        let length = u16::read_from(reader)?;
        let mut buf = vec![0; length as usize];
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

impl<'a> ReadWrite for Cow<'a, str> {
    fn read_from<T: Read>(reader: &mut T) -> Result<Self> {
        Ok(Cow::Owned(read_from!(reader)?))
    }

    fn write_to<T: Write>(&self, writer: &mut T) -> Result<()> {
        self.as_ref().to_owned().write_to(writer)?;
        Ok(())
    }
}

macro_rules! cprw_impls {
    ($(($ty: ty, $read: ident, $insert: ident)),*) => {
        $(
            impl ConstantPoolReadWrite for $ty {
                fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self> {
                    try_cp_read!(cp, reader, $read)
                }

                fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> Result<()> {
                    cp.$insert(*self).write_to(writer)
                }
            }
        )*
    };
}

cprw_impls!((i32, read_int, insert_int), (i64, read_long, insert_long), (f32, read_float, insert_float), (f64, read_double, insert_double));

