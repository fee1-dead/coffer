use std::convert::TryFrom;
use std::sync::Arc;

use once_cell::sync::OnceCell;
use wtf_8::{Wtf8Str, Wtf8String};

use crate::prelude::*;
use crate::total_floats::{TotalF32, TotalF64};

/// The generic read and write trait. This indicates a structure can be read without additional contextual information.
///
/// This trait can be derived with #[derive(ReadWrite)] if all the types it can hold are also `ReadWrite`.
/// Furthermore, all integer types implement `ReadWrite`.
pub trait ReadWrite
where
    Self: Sized,
{
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
            Constant::F32(f) => self.insert_float(f.0),
            Constant::I64(l) => self.insert_long(l),
            Constant::F64(d) => self.insert_double(d.0),
            Constant::String(s) => self.insert_indirect_str(8, s),
            Constant::Class(s) => self.insert_indirect_str(7, s),
            Constant::Member(mem) => self.insert_member(mem),
            Constant::MethodType(t) => {
                let desc = self.insert_wtf8(Cow::Owned(t.to_string().into()));
                self.insert_raw(RawConstantEntry::MethodType(desc))
            }
            Constant::MethodHandle(h) => {
                let mem = self.insert_member(h.member);
                self.insert_raw(RawConstantEntry::MethodHandle(h.kind as u8, mem))
            }
        }
    }

    /// Inserts an value that could be dynamically computed.
    ///
    /// This needs a closure which would insert the static variant to the constant pool.
    ///
    /// Returns an index that points to the inserted entry.
    fn insert_ordynamic<T, F>(&mut self, or_dyn: OrDynamic<T>, f: F) -> u16
    where
        F: FnOnce(&mut Self, T) -> u16,
    {
        match or_dyn {
            OrDynamic::Dynamic(d) => self.insert_dynamic(d),
            OrDynamic::Static(t) => f(self, t),
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
        let bsm = self.insert_bsm(d.get_bsm().clone());
        let e = if d.descriptor.is_method() {
            RawConstantEntry::InvokeDynamic
        } else {
            RawConstantEntry::Dynamic
        };
        let name_and_type =
            self.insert_nameandtype(d.name, Cow::Owned(d.descriptor.to_string().into()));
        self.insert_raw(e(bsm, name_and_type))
    }
    /// insert an indirect string such as String / Module / Package entry, used by the procedural macro.
    fn insert_indirect_str(&mut self, tag: u8, st: Cow<'static, Wtf8Str>) -> u16 {
        let str_ref = self.insert_wtf8(st);
        self.insert_raw(match tag {
            7 => RawConstantEntry::Class,
            8 => RawConstantEntry::String,
            19 => RawConstantEntry::Module,
            20 => RawConstantEntry::Package,
            _ => {
                panic!("invalid tag for indirect string: {}", tag);
            }
        }(str_ref))
    }
    fn insert_wtf8(&mut self, st: Cow<'static, Wtf8Str>) -> u16 {
        self.insert_raw(RawConstantEntry::WTF8(st))
    }
    fn insert_nameandtype(
        &mut self,
        name: Cow<'static, Wtf8Str>,
        descriptor: Cow<'static, Wtf8Str>,
    ) -> u16 {
        let a = self.insert_wtf8(name);
        let b = self.insert_wtf8(descriptor);
        self.insert_raw(RawConstantEntry::NameAndType(a, b))
    }
    fn insert_class(&mut self, c: Cow<'static, Wtf8Str>) -> u16 {
        let idx = self.insert_wtf8(c);
        self.insert_raw(RawConstantEntry::Class(idx))
    }
    fn insert_package(&mut self, c: Cow<'static, Wtf8Str>) -> u16 {
        let idx = self.insert_wtf8(c);
        self.insert_raw(RawConstantEntry::Package(idx))
    }
    fn insert_int(&mut self, i: i32) -> u16 {
        self.insert_raw(RawConstantEntry::Int(i))
    }
    fn insert_long(&mut self, l: i64) -> u16 {
        self.insert_raw(RawConstantEntry::Long(l))
    }
    fn insert_float(&mut self, f: f32) -> u16 {
        self.insert_raw(RawConstantEntry::Float(TotalF32(f)))
    }
    fn insert_double(&mut self, d: f64) -> u16 {
        self.insert_raw(RawConstantEntry::Double(TotalF64(d)))
    }
    fn insert_member(&mut self, mem: MemberRef) -> u16 {
        let entry = match (&mem.descriptor, mem.itfs) {
            (Type::Method { .. }, true) => RawConstantEntry::InterfaceMethod,
            (Type::Method { .. }, false) => RawConstantEntry::Method,
            _ => RawConstantEntry::Field,
        }(
            self.insert_class(mem.owner),
            self.insert_nameandtype(mem.name, Cow::Owned(mem.descriptor.to_string().into())),
        );
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
        unreachable!();
    }

    /// Find the index of a catch.
    ///
    /// This does not need to be implemented because it is used internally,
    /// however a wrapper type should always delagate this function to their inner impl.
    #[inline]
    fn catch(&mut self, _catch: &Catch) -> Option<u16> {
        unreachable!();
    }
}

/// A trait for reading constant pool entries.
///
/// Receivers are mutable to support implementations that lazily populate their content. (which might become immutable in the future if this doesn't work)
pub trait ConstantPoolReader {
    /// Reads a raw entry.
    fn read_raw(&mut self, idx: u16) -> Option<RawConstantEntry>;

    /// Reads a name and type tuple.
    fn read_nameandtype(&mut self, idx: u16) -> Option<(Cow<'static, Wtf8Str>, Type)> {
        match self.read_raw(idx) {
            Some(RawConstantEntry::NameAndType(n, t)) => self.read_wtf8(n).and_then(|n| {
                self.read_wtf8(t)
                    .as_deref()
                    .map(crate::ty::parse_type)
                    .and_then(Result::ok)
                    .map(|t| (n, t))
            }),
            _ => None,
        }
    }
    fn read_constant(&mut self, idx: u16) -> Option<Constant> {
        match self.read_raw(idx) {
            Some(RawConstantEntry::Int(i)) => Some(Constant::I32(i)),
            Some(RawConstantEntry::Long(l)) => Some(Constant::I64(l)),
            Some(RawConstantEntry::Float(f)) => Some(Constant::F32(f)),
            Some(RawConstantEntry::Double(d)) => Some(Constant::F64(d)),
            Some(RawConstantEntry::String(s)) => self.read_wtf8(s).map(Constant::String),
            Some(RawConstantEntry::Class(c)) => self.read_wtf8(c).map(Constant::Class),
            Some(RawConstantEntry::MethodType(m)) => self
                .read_wtf8(m)
                .as_deref()
                .map(parse_type)
                .and_then(Result::ok)
                .map(Constant::MethodType),
            Some(RawConstantEntry::MethodHandle(k, m)) => MethodHandleKind::try_from(k)
                .ok()
                .zip(self.read_member(m))
                .map(|(kind, member)| MethodHandle { kind, member })
                .map(Constant::MethodHandle),
            _ => self.read_member(idx).map(Constant::Member),
        }
    }
    fn read_int(&mut self, idx: u16) -> Option<i32> {
        match self.read_raw(idx) {
            Some(RawConstantEntry::Int(i)) => Some(i),
            _ => None,
        }
    }
    fn read_long(&mut self, idx: u16) -> Option<i64> {
        match self.read_raw(idx) {
            Some(RawConstantEntry::Long(l)) => Some(l),
            _ => None,
        }
    }
    fn read_float(&mut self, idx: u16) -> Option<f32> {
        match self.read_raw(idx) {
            Some(RawConstantEntry::Float(f)) => Some(f.0),
            _ => None,
        }
    }
    fn read_double(&mut self, idx: u16) -> Option<f64> {
        match self.read_raw(idx) {
            Some(RawConstantEntry::Double(d)) => Some(d.0),
            _ => None,
        }
    }

    fn read_indirect_str(&mut self, tag: u8, idx: u16) -> Option<Cow<'static, Wtf8Str>> {
        self.read_raw(idx)
            .and_then(|c| match c {
                RawConstantEntry::Module(u) if tag == 19 => self.read_wtf8(u),
                RawConstantEntry::Package(u) if tag == 20 => self.read_wtf8(u),
                RawConstantEntry::Module(u) if tag == 19 => self.read_wtf8(u),
                RawConstantEntry::Package(u) if tag == 20 => self.read_wtf8(u),
                RawConstantEntry::String(u) if tag == 8 => self.read_wtf8(u),
                RawConstantEntry::Class(u) if tag == 7 => self.read_wtf8(u),
                _ => None,
            })
            .map(Into::into)
    }

    fn read_class(&mut self, idx: u16) -> Option<Cow<'static, Wtf8Str>> {
        self.read_indirect_str(7, idx)
    }

    fn read_wtf8(&mut self, idx: u16) -> Option<Cow<'static, Wtf8Str>> {
        match self.read_raw(idx) {
            Some(RawConstantEntry::WTF8(s)) => Some(s),
            _ => None,
        }
    }
    fn read_or_dynamic<T, F>(&mut self, idx: u16, f: F) -> Option<OrDynamic<T>>
    where
        F: FnOnce(&mut Self, u16) -> Option<T>,
    {
        let dy = self.read_dynamic(idx);
        if let Some(d) = dy {
            Some(OrDynamic::Dynamic(d))
        } else {
            f(self, idx).map(OrDynamic::Static)
        }
    }
    fn read_invokedynamic(&mut self, idx: u16) -> Option<Dynamic> {
        match self.read_raw(idx) {
            Some(RawConstantEntry::InvokeDynamic(s, a)) => {
                let cell = Arc::new(OnceCell::default());
                let (name, descriptor) = self.read_nameandtype(a)?;
                self.resolve_later(s, cell.clone());
                Some(Dynamic {
                    bsm: cell,
                    name,
                    descriptor,
                })
            }
            _ => None,
        }
    }
    fn read_dynamic(&mut self, idx: u16) -> Option<Dynamic> {
        match self.read_raw(idx) {
            Some(RawConstantEntry::Dynamic(s, a)) => {
                let cell = Arc::new(OnceCell::default());
                let (name, descriptor) = self.read_nameandtype(a)?;
                self.resolve_later(s, cell.clone());
                Some(Dynamic {
                    bsm: cell,
                    name,
                    descriptor,
                })
            }
            _ => None,
        }
    }
    fn read_method_handle(&mut self, idx: u16) -> Option<MethodHandle> {
        match self.read_raw(idx) {
            Some(RawConstantEntry::MethodHandle(kind, idx)) => {
                if let (Some(m), Ok(k)) = (self.read_member(idx), MethodHandleKind::try_from(kind))
                {
                    Some(MethodHandle { kind: k, member: m })
                } else {
                    None
                }
            }
            _ => None,
        }
    }
    fn read_member(&mut self, idx: u16) -> Option<MemberRef> {
        match self.read_raw(idx) {
            Some(RawConstantEntry::Method(o, nt)) | Some(RawConstantEntry::Field(o, nt)) => self
                .read_indirect_str(7, o)
                .and_then(|o| self.read_nameandtype(nt).map(|(n, t)| (o, n, t)))
                .map(|(o, n, t)| MemberRef {
                    owner: o,
                    name: n,
                    descriptor: t,
                    itfs: false,
                }),
            Some(RawConstantEntry::InterfaceMethod(o, nt)) => self
                .read_indirect_str(7, o)
                .and_then(|o| self.read_nameandtype(nt).map(|(n, t)| (o, n, t)))
                .map(|(o, n, t)| MemberRef {
                    owner: o,
                    name: n,
                    descriptor: t,
                    itfs: true,
                }),
            _ => None,
        }
    }

    /// Registers a bootstrap method to be resolved.
    fn resolve_later(&mut self, bsm_idx: u16, bsm: Arc<OnceCell<BootstrapMethod>>);

    /// Attempts to complete resolution of bootstrap methods by providing a list of bootstrap methods.
    fn bootstrap_methods(&mut self, bsms: &[BootstrapMethod]) -> Result<()>;
    // Implementations from Code

    /// get a uniquely identified label from an actual offset of the code array.
    /// The label is lazily inserted into the Code vector.
    #[inline]
    fn get_label(&mut self, _idx: u32) -> Label {
        unimplemented!();
    }

    /// Get a catch based on the element index. This is implemented using a wrapper when reading the `Code` struct.
    #[inline]
    fn get_catch(&mut self, _idx: u16) -> Option<Catch> {
        unimplemented!();
    }
}

/// The read and write trait where information must be retrieved along with constant pool information.
///
/// And will insert constant entries into the the constant pool when writing.
pub trait ConstantPoolReadWrite
where
    Self: Sized,
{
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self>;
    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> Result<()>;
}

/// helpful macro to return an error if the constant entry is not found or occupied by a double-sized entry.
#[macro_export]
macro_rules! try_cp_read {
    ($cp: ident, $reader: ident, $fn: ident) => {{
        let idx = $crate::ReadWrite::read_from($reader)?;
        $crate::try_cp_read!(idx, $cp.$fn(idx))
    }};

    ($idx: expr, $opt: expr) => {{
        match $opt {
            Some(s) => Ok(s),
            None => Err($crate::error::Error::Invalid(
                "Constant pool entry index",
                Into::into($idx.to_string()),
            )),
        }
    }};
}

#[macro_export]
macro_rules! try_cp_read_idx {
    ($cp: ident, $idx: expr, $fn: ident) => {{
        $crate::try_cp_read!($idx, $cp.$fn($idx))
    }};
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

macro_rules! impl_readwrite_total_floats {
    ($($name:ident),*) => {
        $(
            impl ReadWrite for $name {
                fn read_from<T: Read>(reader: &mut T) -> Result<Self> {
                    ReadWrite::read_from(reader).map($name)
                }
                fn write_to<T: Write>(&self, writer: &mut T) -> Result<()> {
                    ReadWrite::write_to(&self.0, writer)
                }
            }
        )*
    };
}

impl_readwrite_total_floats!(TotalF32, TotalF64);

impl ReadWrite for Wtf8String {
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

impl<'a> ReadWrite for Cow<'a, Wtf8Str> {
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

cprw_impls!(
    (i32, read_int, insert_int),
    (i64, read_long, insert_long),
    (f32, read_float, insert_float),
    (f64, read_double, insert_double)
);

macro_rules! total_floats {
    ($($name:ident),*) => {$(
        impl ConstantPoolReadWrite for $name {
            fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self> {
                ConstantPoolReadWrite::read_from(cp, reader).map($name)
            }
            fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> Result<()> {
                ConstantPoolReadWrite::write_to(&self.0, cp, writer)
            }
        }
    )*};
}

total_floats!(TotalF32, TotalF64);
