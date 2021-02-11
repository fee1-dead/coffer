
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
use std::borrow::Cow;
use std::fmt::Display;
use std::fmt::Formatter;
use std::hash::{Hash, Hasher};
use std::io::{Read, Write};
use std::str::FromStr;

use annotation::Annotation;
pub use code::*;
pub use signature::*;

use crate::{ConstantPoolReader, ConstantPoolReadWrite, ConstantPoolWriter, Error, read_from, ReadWrite, Result, try_cp_read};
use crate::access::AccessFlags;
use crate::full::annotation::{AnnotationValue, FieldTypeAnnotation, MethodTypeAnnotation};
use crate::full::version::JavaVersion;

pub mod annotation;
pub mod version;

mod signature;
pub mod cp;
mod code;

/// A method handle. It can be loaded on to the stack from the constant pool directly.
///
/// There are some restrictions of its kind and its member, one can use [`check`](MethodHandle::check) to check the validity.
#[derive(Clone, PartialEq, Hash, Debug)]
pub struct MethodHandle {
    pub kind: MethodHandleKind,
    pub member: MemberRef
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
                                descriptor: Type::Method(..),
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
                                descriptor: Type::Method(..),
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
            MethodHandleKind::InvokeVirtual |
            MethodHandleKind::InvokeStatic |
            MethodHandleKind::InvokeSpecial |
            MethodHandleKind::InvokeInterface if self.member.name == "<init>" || self.member.name == "<clinit>" =>
                Err(Error::Invalid("MethodHandle", Cow::Borrowed("name must not be <init> or <clinit>"))),
            MethodHandleKind::NewInvokeSpecial if self.member.name != "<init>" =>
                Err(Error::Invalid("MethodHandle", Cow::Borrowed("name for NewInvokeSpecial must be <init>"))),
            _ => Ok(())
        }
    }
}
impl ConstantPoolReadWrite for MethodHandle {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self, Error> {
        let kind = read_from!(reader)?;
        let member = read_from!(cp, reader)?;
        let res = Self {
            kind, member
        };
        res.check()?;
        Ok(res)
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> Result<(), Error> {
        self.check()?;
        self.kind.write_to(writer)?;
        self.member.write_to(cp, writer)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Constant {
    I32(i32),
    F32(f32),
    I64(i64),
    F64(f64),
    String(Cow<'static, str>),
    Class(Cow<'static, str>),
    Member(MemberRef),
    MethodType(Type),
    MethodHandle(MethodHandle)
}

impl Constant {
    fn is_wide(&self) -> bool {
        matches!(self, Constant::I64(_) | Constant::F64(_))
    }
}

impl ConstantPoolReadWrite for Constant {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self> {
        let idx = ReadWrite::read_from(reader)?;
        cp.read_constant(idx).ok_or_else(|| crate::error::Error::Invalid("constant pool entry index", idx.to_string().into()))
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
            Constant::Member(mem) => mem.hash(state)
        }
    }
}


#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub enum Type {
    Byte, Char, Double, Float, Int, Long, Boolean, Short, Ref(Cow<'static, str>), ArrayRef(u8, Box<Type>),
    /// The Method type. First is the parameter list and second is the return type. If the return type is `None`, it represents a `void` return type.
    ///
    /// It is invalid if any of the parameter types and the return type is a method type.
    Method(Vec<Type>, Option<Box<Type>>)
}

impl Type {
    /// returns `true` if this type is `Long` or `Double`.
    #[inline]
    pub fn is_wide(&self) -> bool {
        matches!(self, Type::Long | Type::Double)
    }

    #[inline]
    pub fn is_method(&self) -> bool { matches!(self, Type::Method(..)) }
}

impl From<Type> for Cow<'static, str> {
    fn from(t: Type) -> Self {
        match t {
            Type::Byte => "B".into(),
            Type::Char => "C".into(),
            Type::Double => "D".into(),
            Type::Float => "F".into(),
            Type::Int => "I".into(),
            Type::Long => "J".into(),
            Type::Boolean => "Z".into(),
            Type::Short => "S".into(),
            Type::Ref(t) => format!("L{};", t).into(),
            Type::ArrayRef(dim, t) => format!("{}{}", "[".repeat(dim as usize), t).into(),
            Type::Method(ty, ret) => {
                let mut s = String::from('(');
                for t in ty {
                    s.push_str(Cow::from(t).as_ref());
                }
                s.push(')');
                if let Some(t) = ret {
                    s.push_str(Cow::from(*t).as_ref());
                } else {
                    s.push('V');
                }
                s.into()
            }
        }
    }
}

impl FromStr for Type {
    type Err = crate::error::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        fn get_type(c: &mut std::str::Chars, st: &str) -> Result<Type, crate::error::Error> {
            let next_char = c.next();
            Ok(match next_char {
                Some('B') => Type::Byte,
                Some('C') => Type::Char,
                Some('D') => Type::Double,
                Some('F') => Type::Float,
                Some('I') => Type::Int,
                Some('J') => Type::Long,
                Some('Z') => Type::Boolean,
                Some('S') => Type::Short,
                Some('L') => {
                    let mut st = String::new();
                    while c.as_str().chars().next().unwrap_or(';') != ';' {
                        st.push(c.next().unwrap())
                    }
                    if c.next().is_none() {
                        return unexpected_end()
                    } else {
                        Type::Ref(Cow::Owned(st))
                    }
                }
                Some('[') => {
                    let mut dim: u8 = 1;
                    while let Some('[') = c.as_str().chars().next() {
                        c.next();
                        dim = dim.checked_add(1).ok_or_else(crate::error::Error::ArithmeticOverflow)?;
                    }
                    let r = get_type(c, st)?;
                    Type::ArrayRef(dim, Box::new(r))
                }
                Some('(') => {
                    let mut types = Vec::new();
                    while c.as_str().chars().next().unwrap_or(')') != ')' {
                        types.push(get_type(c, st)?)
                    }
                    if c.next().is_none() {
                        return unexpected_end()
                    } else {
                        Type::Method(types, if let Some('V') = c.as_str().chars().next() {
                            None
                        } else {
                            Some(Box::new(get_type(c, st)?))
                        })
                    }
                }
                Some(ch) => {
                    return Err(crate::error::Error::Invalid("type character", ch.to_string().into()))
                }
                None => {
                    return unexpected_end()
                }
            })
        }
        get_type(&mut s.chars(), s)
    }
}
impl ConstantPoolReadWrite for Cow<'static, str> {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self> {
        let idx = ReadWrite::read_from(reader)?;
        cp.read_utf8(idx).ok_or_else(|| crate::error::Error::Invalid("constant pool entry index", idx.to_string().into())).map(Into::into)
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> Result<()> {
        cp.insert_utf8(self.clone()).write_to(writer)
    }
}
impl ConstantPoolReadWrite for Type {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self> {
        crate::try_cp_read!(cp, reader, read_utf8)?.parse()
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> Result<()> {
        cp.insert_utf8(self.to_string()).write_to(writer)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        use std::fmt::Write;
        match self {
            Type::Byte => { f.write_char('B') }
            Type::Char => { f.write_char('C') }
            Type::Double => { f.write_char('D') }
            Type::Float => { f.write_char('F') }
            Type::Int => { f.write_char('I') }
            Type::Long => { f.write_char('J') }
            Type::Boolean => { f.write_char('Z') }
            Type::Short => { f.write_char('S') }
            Type::Ref(s) => { write!(f, "L{};", s) }
            Type::ArrayRef(dim, t) => {
                "[".repeat(*dim as usize).fmt(f)?;
                t.fmt(f)
            }
            Type::Method(params, ret) => {
                f.write_char('(')?;
                for t in params {
                    t.fmt(f)?;
                }
                f.write_char(')')?;
                if let Some(ref t) = ret {
                    t.fmt(f)?;
                } else {
                    f.write_char('V')?;
                }
                Ok(())
            }
        }
    }
}

impl Type {
    #[inline]
    pub fn method<P: Into<Vec<Type>>>(params: P, ret: Option<Type>) -> Type {
        Type::Method(params.into(), ret.map(Box::new))
    }
    #[inline]
    pub fn reference<S>(str: S) -> Type where S: Into<Cow<'static, str>> {
        Type::Ref(str.into())
    }
    pub fn array(dim: u8, t: Type) -> Type {
        Type::ArrayRef(dim, Box::new(t))
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct MemberRef {
    pub owner: Cow<'static, str>,
    pub name: Cow<'static, str>,
    pub descriptor: Type,
    pub itfs: bool,
}

impl ConstantPoolReadWrite for MemberRef {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self, Error> {
        try_cp_read!(cp, reader, read_member)
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> Result<(), Error> {
        cp.insert_member(self.clone()).write_to(writer)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct RawAttribute {
    /// Whether to keep this attribute upon writing.
    /// Attributes that are related to local variables will default to `false`, whereas newly created attributes will be `true`.
    keep: bool,
    pub name: Cow<'static, str>,
    pub inner: Cow<'static, [u8]>
}

impl RawAttribute {
    pub fn new<S: Into<Cow<'static, str>>, B: Into<Cow<'static, [u8]>>>(name: S, inner: B) -> Self {
        Self {
            keep: true,
            name: name.into(),
            inner: inner.into()
        }
    }
    /// Used by the procedural macro.
    fn __new(name: Cow<'static, str>, inner: Vec<u8>) -> Self {
        Self {
            keep: false,
            name,
            inner: Cow::Owned(inner)
        }
    }
}

/// Completed
#[derive(PartialEq, Debug, Clone, ConstantPoolReadWrite)]
#[attr_enum]
pub enum FieldAttribute {
    Deprecated,
    Synthetic,
    Signature(FieldSignature),
    ConstantValue(Constant),
    RuntimeVisibleAnnotations(#[vec_len_type(u16)] Vec<Annotation>),
    RuntimeInvisibleAnnotations(#[vec_len_type(u16)]Vec<Annotation>),
    RuntimeVisibleTypeAnnotations(#[vec_len_type(u16)] Vec<FieldTypeAnnotation>),
    RuntimeInvisibleTypeAnnotations(#[vec_len_type(u16)] Vec<FieldTypeAnnotation>),
    #[raw_variant]
    Raw(RawAttribute)
}

#[derive(PartialEq, Debug, Clone)]
pub struct Field {
    pub access: AccessFlags,
    pub name: Cow<'static, str>,
    pub descriptor: Type,
    pub attrs: Vec<FieldAttribute>
}

#[derive(PartialEq, Debug, Clone, ConstantPoolReadWrite)]
pub struct ParameterAnnotations(#[vec_len_type(u16)] Vec<Annotation>);

impl std::ops::Deref for ParameterAnnotations {
    type Target = Vec<Annotation>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for ParameterAnnotations {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(PartialEq, Debug, Clone, ConstantPoolReadWrite)]
pub struct MethodParameter {
    #[str_optional]
    name: Option<Cow<'static, str>>,
    #[use_normal_rw]
    access: AccessFlags
}

#[derive(PartialEq, Debug, Clone, ConstantPoolReadWrite)]
#[attr_enum]
pub enum MethodAttribute {
    Code(Code),
    Deprecated,
    Synthetic,
    Signature(MethodSignature),
    RuntimeVisibleAnnotations(#[vec_len_type(u16)] Vec<Annotation>),
    RuntimeInvisibleAnnotations(#[vec_len_type(u16)] Vec<Annotation>),
    RuntimeVisibleTypeAnnotations(#[vec_len_type(u16)] Vec<MethodTypeAnnotation>),
    RuntimeInvisibleTypeAnnotations(#[vec_len_type(u16)] Vec<MethodTypeAnnotation>),
    RuntimeVisibleParameterAnnotations(#[vec_len_type(u8)] Vec<ParameterAnnotations>),
    RuntimeInvisibleParameterAnnotations(#[vec_len_type(u8)] Vec<ParameterAnnotations>),
    Exceptions(#[vec_len_type(u8)] Vec<Clazz>),
    AnnotationDefault(AnnotationValue),
    MethodParameters(#[vec_len_type(u8)] Vec<MethodParameter>),
    #[raw_variant]
    Raw(RawAttribute),
}

#[derive(PartialEq, Debug, Clone)]
pub struct Method {
    pub access: AccessFlags,
    pub name: Cow<'static, str>,
    pub descriptor: Type,
    pub attributes: Vec<MethodAttribute>
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, ConstantPoolReadWrite)]
#[tag_type(u8)]
pub enum VerificationType {
    Top, Int, Float, Long, Double, Null, UninitializedThis, Object(#[str_type(Class)] Cow<'static, str>),
    /// Following the label, must be a `NEW` instruction.
    UninitializedVariable(Label)
}

impl VerificationType {
    pub const fn is_wide(&self) -> bool {
        matches!(self, VerificationType::Double | VerificationType::Long)
    }
}

#[derive(Eq, PartialEq, Debug, Clone, ConstantPoolReadWrite)]
pub struct InnerClass {
    #[str_type(Class)]
    pub inner_fqname: Cow<'static, str>,
    #[str_optional]
    #[str_type(Class)]
    pub outer_fqname: Option<Cow<'static, str>>,
    /// None if the inner class is an anonymous class.
    #[str_optional]
    pub inner_name: Option<Cow<'static, str>>,
    #[use_normal_rw]
    pub inner_access: AccessFlags
}

#[derive(Clone, Eq, PartialEq, Debug, ConstantPoolReadWrite)]
pub struct Require {
    #[str_type(Module)]
    pub module: Cow<'static, str>,
    #[use_normal_rw]
    pub flags: AccessFlags,
    #[str_optional]
    pub version: Option<Cow<'static, str>>
}

#[repr(transparent)]
#[derive(Clone, Eq, PartialEq, Debug, ConstantPoolReadWrite)]
pub struct To(#[str_type(Module)] Cow<'static, str>);

#[repr(transparent)]
#[derive(Clone, Eq, PartialEq, Debug, ConstantPoolReadWrite)]
pub struct Clazz(#[str_type(Class)] Cow<'static, str>);

#[derive(Clone, Eq, PartialEq, Debug, ConstantPoolReadWrite)]
pub struct Provide {
    #[str_type(Class)]
    pub class: Cow<'static, str>,
    #[vec_len_type(u16)]
    pub with: Vec<Clazz>
}

#[derive(Eq, PartialEq, Debug, Clone, ConstantPoolReadWrite)]
pub struct Module {
    #[str_type(Module)]
    pub name: Cow<'static, str>,
    #[use_normal_rw]
    pub flags: AccessFlags,
    #[str_optional]
    pub version: Option<Cow<'static, str>>,
    #[vec_len_type(u16)]
    pub requires: Vec<Require>,
    #[vec_len_type(u16)]
    pub exports: Vec<Export>,
    #[vec_len_type(u16)]
    pub opens: Vec<Open>,
    #[vec_len_type(u16)]
    pub uses: Vec<Clazz>,
    #[vec_len_type(u16)]
    pub provides: Vec<Provide>
}

#[derive(PartialEq, Debug, Clone)]
pub enum ClassAttribute {
    Signature(ClassSignature),
    Synthetic, Deprecated, SourceFile(Cow<'static, str>), InnerClasses(Vec<InnerClass>),
    /// first: fully qualified name of the innermost outer class.
    /// second: name of the method that encloses this inner/anonymous class.
    /// third: descriptor of the method.
    EnclosingMethod(Cow<'static, str>, Cow<'static, str>, Type), SourceDebugExtension(Cow<'static, str>),
    BootstrapMethods(Vec<BootstrapMethod>), Module(Module), ModulePackages(Vec<Cow<'static, str>>), ModuleMainClass(Cow<'static, str>),
    NestHost(Cow<'static, str>), NestMembers(Vec<Cow<'static, str>>), Raw(RawAttribute)
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct BootstrapMethod {
    pub method: MethodHandle,
    pub arguments: Vec<OrDynamic<Constant>>
}

#[derive(PartialEq, Debug, Clone)]
pub struct Class {
    pub version: JavaVersion,
    pub access: AccessFlags,
    pub name: Cow<'static, str>,
    /// java/lang/Object has no superclass.
    pub super_name: Option<Cow<'static, str>>,
    pub interfaces: Vec<Cow<'static, str>>,
    pub fields: Vec<Field>,
    pub methods: Vec<Method>,
    pub attributes: Vec<ClassAttribute>
}