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
//! Loadable values. These types can be loaded on to the stack, and it is usually stored in the constant pool.

pub use crate::member::MemberRef;
pub use crate::ty::Type;

use crate::prelude::*;
use crate::read_from;
use std::convert::TryFrom;
use std::hash::{Hash, Hasher};

/// The kind of a method handle. It generally represents an instruction related to a member with one exception.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, ReadWrite)]
#[tag_type(u8)]
pub enum MethodHandleKind {
    /// A method handle reading from a virtual field.
    GetField = 1,
    /// A method handle reading from a static field.
    GetStatic,
    /// A method handle writing to a virtual field.
    PutField,
    /// A method handle writing to a static field.
    PutStatic,
    /// A method handle invoking a virtual method.
    InvokeVirtual,
    /// A method handle invoking a static method.
    InvokeStatic,
    /// A method handle invoking a special method.
    InvokeSpecial,
    /// A method handle invoking a construtor.
    NewInvokeSpecial,
    /// A method handle invoking an interface method.
    InvokeInterface,
}

impl TryFrom<u8> for MethodHandleKind {
    type Error = u8;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        use MethodHandleKind::*;
        Ok(match value {
            1 => GetField,
            2 => GetStatic,
            3 => PutField,
            4 => PutStatic,
            5 => InvokeVirtual,
            6 => InvokeStatic,
            7 => InvokeSpecial,
            8 => NewInvokeSpecial,
            9 => InvokeInterface,
            v => return Err(v),
        })
    }
}

/// A method handle. It can be loaded on to the stack from the constant pool directly.
///
/// There are some restrictions of its kind and its member, one can use [`check`](MethodHandle::check) to check the validity.
#[derive(Clone, PartialEq, Hash, Debug)]
pub struct MethodHandle {
    /// The kind of the handle.
    pub kind: MethodHandleKind,
    /// The member that this handle points to.
    pub member: MemberRef,
}

impl MethodHandle {
    /// Checks if this methodhandle is valid. Returns `Ok(())` if it is, returns `Err` with a detailed message if it is not.
    ///
    /// Rules:
    ///  - Handles with types [`GetField`](MethodHandleKind::GetField), [`GetStatic`](MethodHandleKind::GetStatic), [`PutField`](MethodHandleKind::PutField) and [`PutStatic`](MethodHandleKind::PutStatic) should have a `MemberRef` with a field type descriptor.
    ///  - Handles with types [`InvokeInterface`](MethodHandleKind::InvokeInterface), [`InvokeSpecial`](MethodHandleKind::InvokeSpecial), [`InvokeStatic`](MethodHandleKind::InvokeStatic), [`InvokeVirtual`](MethodHandleKind::InvokeVirtual) and [`NewInvokeSpecial`](MethodHandleKind::NewInvokeSpecial) should have a `MemberRef` with a method type descriptor.
    ///  - Handles cannot refer to static initializers, i.e. `MemberRef` with a name of `<clinit>`
    ///  - Handles with type [`NewInvokeSpecial`](MethodHandleKind::NewInvokeSpecial) must refer to a constructor method, i.e. `MemberRef` with a name of `<init>`
    ///  - Handles whose types are not [`NewInvokeSpecial`](MethodHandleKind::NewInvokeSpecial) cannot refer to a constructor method.
    ///
    pub fn check(&self) -> Result<()> {
        macro_rules! should_not_be_method {
            ($($kind: path),*) => {
                match self {
                    $(
                        MethodHandle {
                            kind: $kind,
                            member: MemberRef {
                                descriptor: Type::Method { .. },
                                ..
                            }
                        } => return Err(Error::Invalid("MethodHandle", concat!("kind is ", stringify!($kind), "but descriptor is method").into())),
                    )*
                    _ => {}
                }
            };
        }
        macro_rules! should_be_method {
            ($($kind:path),*) => {
                match self {
                    $(
                        MethodHandle {
                            kind: $kind,
                            member: MemberRef {
                                descriptor: Type::Method { .. },
                                ..
                            }
                        } => {},
                        MethodHandle {
                            kind: $kind,
                            ..
                        } => return Err(Error::Invalid("MethodHandle", concat!("kind is ", stringify!($kind), "but descriptor is NOT method").into())),
                    )*
                    _ => {}
                }
            };
        }
        should_not_be_method! { MethodHandleKind::GetField, MethodHandleKind::GetStatic, MethodHandleKind::PutField, MethodHandleKind::PutStatic }
        should_be_method! { MethodHandleKind::InvokeInterface, MethodHandleKind::InvokeSpecial, MethodHandleKind::InvokeStatic, MethodHandleKind::InvokeVirtual, MethodHandleKind::NewInvokeSpecial }
        match self.kind {
            MethodHandleKind::InvokeVirtual
            | MethodHandleKind::InvokeStatic
            | MethodHandleKind::InvokeSpecial
            | MethodHandleKind::InvokeInterface
                if self.member.name == "<init>" || self.member.name == "<clinit>" =>
            {
                Err(Error::Invalid(
                    "MethodHandle",
                    Cow::Borrowed("name must not be <init> or <clinit>"),
                ))
            }
            MethodHandleKind::NewInvokeSpecial if self.member.name != "<init>" => {
                Err(Error::Invalid(
                    "MethodHandle",
                    Cow::Borrowed("name for NewInvokeSpecial must be <init>"),
                ))
            }
            _ => Ok(()),
        }
    }
}
impl ConstantPoolReadWrite for MethodHandle {
    fn read_from<C: ConstantPoolReader, R: Read>(
        cp: &mut C,
        reader: &mut R,
    ) -> Result<Self, Error> {
        let kind = read_from!(reader)?;
        let member = read_from!(cp, reader)?;
        let res = Self { kind, member };
        res.check()?;
        Ok(res)
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(
        &self,
        cp: &mut C,
        writer: &mut W,
    ) -> Result<(), Error> {
        self.check()?;
        self.kind.write_to(writer)?;
        self.member.write_to(cp, writer)
    }
}

/// A constant value that is located in the constant pool which can be loaded onto the stack.
#[derive(Clone, PartialEq, Debug)]
pub enum Constant {
    /// A 32 bit integer.
    I32(i32),
    /// A single-precision floating-point number.
    F32(f32),
    /// A 64 bit integer.
    I64(i64),
    /// A double-precision floating-point number.
    F64(f64),
    /// A String.
    String(Cow<'static, str>),
    /// A class. The packages are seperated by `/` instead of `.` i.e. `java/lang/String` instead of `java.lang.String`.
    Class(Cow<'static, str>),
    /// A member, can be a field or a method.
    Member(MemberRef),
    /// A method descriptor.
    MethodType(Type),
    /// A method handle.
    MethodHandle(MethodHandle),
}

impl Constant {
    /// returns `true` if this constant is a `f64` or `i64`, i.e., `long` or `double` in java.
    ///
    /// These types take up two slots in the stack.
    #[inline]
    pub const fn is_wide(&self) -> bool {
        matches!(self, Constant::I64(_) | Constant::F64(_))
    }

    /// Creates an instance of Constant that is a string.
    #[inline]
    pub fn string<T: Into<Cow<'static, str>>>(s: T) -> Self {
        Self::String(s.into())
    }
}

impl ConstantPoolReadWrite for Constant {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self> {
        let idx = ReadWrite::read_from(reader)?;
        cp.read_constant(idx).ok_or_else(|| {
            crate::error::Error::Invalid("constant pool entry index", idx.to_string().into())
        })
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> Result<()> {
        ReadWrite::write_to(&cp.insert_constant(self.clone()), writer)
    }
}

impl From<i32> for Constant {
    fn from(i: i32) -> Self {
        Self::I32(i)
    }
}

#[allow(clippy::derive_hash_xor_eq)]
// Hash cannot be directly derived for floating point types; hash by actual bits of the fp values because that is what will be written in byte form.
impl Hash for Constant {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Constant::I32(i) => i.hash(state),
            Constant::F32(f) => (*f).to_bits().hash(state),
            Constant::I64(i) => i.hash(state),
            Constant::F64(f) => (*f).to_bits().hash(state),
            Constant::String(s) => s.hash(state),
            Constant::Class(s) => s.hash(state),
            Constant::MethodType(s) => s.hash(state),
            Constant::MethodHandle(m) => m.hash(state),
            Constant::Member(mem) => mem.hash(state),
        }
    }
}
