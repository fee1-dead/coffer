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

use std::convert::TryFrom;
use std::rc::Rc;

use crate::full::*;
use crate::prelude::*;

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
    /// Inserts a raw constant pool entry to the constant pool.
    ///
    /// Returns an index that points to the inserted entry.
    fn insert_raw(&mut self, value: RawConstantEntry) -> u16;

    /// Inserts a constant entry to the constant pool.
    ///
    /// Returns an index that points to the inserted entry.
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

    /// Inserts an value that could be dynamically computed.
    ///
    /// This needs a closure which would insert the static variant to the constant pool.
    ///
    /// Returns an index that points to the inserted entry.
    fn insert_ordynamic<T, F>(&mut self, or_dyn: OrDynamic<T>, f: F) -> u16 where F: FnOnce(&mut Self, T) -> u16 {
        match or_dyn {
            OrDynamic::Dynamic(d) => self.insert_dynamic(d),
            OrDynamic::Static(t) => f(self, t)
        }
    }

    /// Inserts a bootstrap method.
    ///
    /// Because dynamic entries refer to an index in the bootstrap methods
    /// attribute entry, this must be present in a constant pool writer as
    /// inserting a dynamic entry would require an index to the bootstrap method.
    ///
    /// Returns an index to the bootstrap method in the BootstrapMethods attribute
    /// of the class file.
    fn insert_bsm(&mut self, bsm: BootstrapMethod) -> u16;

    /// Inserts a dynamic computed constant/callsite.
    ///
    /// When the type of the dynamic is a method descriptor, an InvokeDynamic entry is inserted.
    /// Otherwise, a dynamic entry is inserted.
    ///
    /// Returns an index that points to the inserted entry.
    fn insert_dynamic(&mut self, d: Dynamic) -> u16 {
        let bsm = self.insert_bsm(Rc::try_unwrap(d.bsm).unwrap().into_inner().unwrap());
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
    fn insert_package<T: Into<Cow<'static, str>>>(&mut self, c: T) -> u16 {
        let idx = self.insert_utf8(c);
        self.insert_raw(RawConstantEntry::Package(idx))
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
            (Type::Method { .. }, true) => RawConstantEntry::InterfaceMethod,
            (Type::Method { .. }, false) => RawConstantEntry::Method,
            _ => RawConstantEntry::Field
        }(self.insert_class(mem.owner), self.insert_nameandtype(mem.name, mem.descriptor));
        self.insert_raw(entry)
    }
    fn insert_method_handle(&mut self, handle: MethodHandle) -> u16 {
        let mem = self.insert_member(handle.member);
        self.insert_raw(RawConstantEntry::MethodHandle(handle.kind as u8, mem))
    }

    /// Map a label to the actual offset in the code array.
    ///
    /// This does not need to be implemented because it is used internally,
    /// however a wrapper type should always delagate this function to their inner impl.
    #[inline]
    fn label(&mut self, _lbl: &Label) -> u16 {
        #[cfg(debug_assertions)]
        unreachable!();
        #[cfg(not(debug_assertions))]
            unsafe {
            core::hint::unreachable_unchecked();
        }
    }

    /// Find the index of a catch.
    ///
    /// This does not need to be implemented because it is used internally,
    /// however a wrapper type should always delagate this function to their inner impl.
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
///
/// Receivers are mutable to support implementations that lazily populate their content. (which might become immutable in the future if this doesn't work)
pub trait ConstantPoolReader {
    /// Reads a raw entry.
    fn read_raw(&mut self, idx: u16) -> Option<RawConstantEntry>;

    /// Reads a name and type tuple.
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
            Some(RawConstantEntry::String(s)) => self.read_utf8(s).map(Constant::String),
            Some(RawConstantEntry::Class(c)) => self.read_utf8(c).map(Constant::Class),
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
                let mut cell = Rc::new(LazyBsm::new());
                let (name, descriptor) = self.read_nameandtype(a)?;
                self.resolve_later(s, cell.clone());
                Some(Dynamic {
                    bsm: cell,
                    name, descriptor
                })
            },
            _ => None
        }
    }
    fn read_dynamic(&mut self, idx: u16) -> Option<Dynamic> {
        match self.read_raw(idx) {
            Some(RawConstantEntry::Dynamic(s, a)) =>  {
                let mut cell = Rc::new(LazyBsm::new());
                let (name, descriptor) = self.read_nameandtype(a)?;
                self.resolve_later(s, cell.clone());
                Some(Dynamic {
                    bsm: cell,
                    name, descriptor
                })
            },
            _ => None
        }
    }
    fn read_method_handle(&mut self, idx: u16) -> Option<MethodHandle> {
        match self.read_raw(idx) {
            Some(RawConstantEntry::MethodHandle(kind, idx)) => {
                if let (Some(m), Ok(k)) = (self.read_member(idx), MethodHandleKind::try_from(kind)) {
                    Some(MethodHandle {
                        kind: k,
                        member: m
                    })
                } else {
                    None
                }
            }
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

    /// Registers a bootstrap method to be resolved.
    fn resolve_later(&mut self, bsm_idx: u16, bsm: Rc<LazyBsm>);

    /// Attempts to complete resolution of bootstrap methods by providing a list of bootstrap methods.
    fn bootstrap_methods(&mut self, bsms: &[BootstrapMethod]) -> Result<()>;
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

/// The read and write trait where information must be retrieved along with constant pool information.
///
/// And will insert constant entries into the the constant pool when writing.
pub trait ConstantPoolReadWrite where Self: Sized {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self>;
    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> Result<()>;
}

/// helpful macro to return an error if the constant entry is not found or occupied by a double-sized entry.
#[macro_export]
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

/// Helper macro to disambigurate if a type implements both [`ConstantPoolReadWrite`] and [`ReadWrite`].
///
/// [`ConstantPoolReadWrite`]: ConstantPoolReadWrite
/// [`ReadWrite`]: ReadWrite
#[macro_export]
macro_rules! read_from {
    ($writer: expr) => {
        $crate::ReadWrite::read_from($writer)
    };
    ($cp: expr, $writer: expr) => {
        $crate::ConstantPoolReadWrite::read_from($cp, $writer)
    };
}

/// Helper macro to disambigurate if a type implements both [`ConstantPoolReadWrite`] and [`ReadWrite`].
///
/// [`ConstantPoolReadWrite`]: ConstantPoolReadWrite
/// [`ReadWrite`]: ReadWrite
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