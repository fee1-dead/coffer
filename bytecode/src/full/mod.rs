
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
pub mod annotation;
pub mod version;

mod signature;
pub mod cp;

pub use signature::*;
use std::borrow::Cow;
use std::hash::{Hash, Hasher};
use crate::access::AccessFlags;

use std::fmt::Display;
use std::fmt::Formatter;
use annotation::Annotation;
use crate::full::annotation::{FieldTypeAnnotation, MethodTypeAnnotation, CodeTypeAnnotation, AnnotationValue};
use crate::full::version::JavaVersion;
use crate::{ConstantPoolReadWrite, ConstantPoolReader, ConstantPoolWriter, ReadWrite, Result, Error, read_from, try_cp_read};
use std::str::FromStr;
use std::io::{Read, Write};
use crate::full::cp::RawConstantEntry;

#[derive(Debug, Eq, PartialOrd, PartialEq, Ord, Hash, Copy, Clone)]
pub struct Label(u32);

impl ConstantPoolReadWrite for Label {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self> {
        Ok(cp.get_label(ReadWrite::read_from(reader)?))
    }
    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> Result<()> {
        ReadWrite::write_to(&cp.label(*self), writer)
    }
}

#[derive(Clone, PartialEq, Hash, Debug)]
pub struct MethodHandle {
    pub kind: MethodHandleKind,
    pub owner: Cow<'static, str>,
    pub name: Cow<'static, str>,
    pub descriptor: Type,
    pub interface: bool
}

impl MethodHandle {
    pub fn check(&self) -> Result<()> {
        macro_rules! should_not_be_method {
            ($($kind: path),*) => {
                match self {
                    $(
                        MethodHandle {
                            kind: $kind,
                            descriptor: Type::Method(..),
                            ..
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
                            descriptor: Type::Method(..),
                            ..
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
        Ok(())
    }
}
impl ConstantPoolReadWrite for MethodHandle {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self, Error> {
        let kind = read_from!(reader)?;
        let entry = try_cp_read!(cp, reader, read_raw)?;
        match entry {
            RawConstantEntry::Field(owner, nameandtype) | RawConstantEntry::Method(owner, nameandtype)=> {
                let owner = try_cp_read!(owner, cp.read_indirect_str(7, owner))?;
                let (name, descriptor) = try_cp_read!(nameandtype, cp.read_nameandtype(nameandtype))?;
                let res = Self {
                    kind, owner, name, descriptor, interface: false
                };
                res.check()?;
                Ok(res)
            }
            RawConstantEntry::InterfaceMethod(owner, nameandtype) => {
                let owner = try_cp_read!(owner, cp.read_indirect_str(7, owner))?;
                let (name, descriptor) = try_cp_read!(nameandtype, cp.read_nameandtype(nameandtype))?;
                let res = Self {
                    kind, owner, name, descriptor, interface: true
                };
                res.check()?;
                Ok(res)
            }
            _ => return Err(Error::Invalid("constant pool entry in method handle", format!("{:?}", entry).into()))
        }
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> Result<(), Error> {
        fn not_init(handle: &MethodHandle) -> Result<()> {
            match handle.name.as_ref() {
                "<init>" => Err(Error::Invalid("MethodHandle", Cow::Borrowed("name must not be <init>"))),
                "<clinit>" => Err(Error::Invalid("MethodHandle", Cow::Borrowed("name must not be <clinit>"))),
                _ => Ok(())
            }
        }
        self.kind.write_to(writer)?;
        match self.kind {
            MethodHandleKind::GetField  |
            MethodHandleKind::GetStatic |
            MethodHandleKind::PutField |
            MethodHandleKind::PutStatic => {
                let owner = cp.insert_indirect_str(7, self.owner.clone());
                let name = cp.insert_nameandtype(self.name.clone(), self.descriptor.to_string());
                cp.insert_raw(RawConstantEntry::Field(owner, name)).write_to(writer)?;
            }
            MethodHandleKind::InvokeVirtual => {
                not_init(self)?;
                let owner = cp.insert_indirect_str(7, self.owner.clone());
                let name = cp.insert_nameandtype(self.name.clone(), self.descriptor.to_string());
                cp.insert_raw(RawConstantEntry::Method(owner, name)).write_to(writer)?;
            }
            MethodHandleKind::InvokeStatic |
            MethodHandleKind::InvokeSpecial => {
                not_init(self)?;
                let owner = cp.insert_indirect_str(7, self.owner.clone());
                let name = cp.insert_nameandtype(self.name.clone(), self.descriptor.to_string());

                cp.insert_raw(if self.interface {
                    RawConstantEntry::InterfaceMethod(owner, name)
                } else {
                    RawConstantEntry::Method(owner, name)
                }).write_to(writer)?;
            }
            MethodHandleKind::NewInvokeSpecial => {
                if self.name != "<init>" {
                    return Err(Error::Invalid("MethodHandle", Cow::Borrowed("name for NewInvokeSpecial must be <init>")))
                }
                let owner = cp.insert_indirect_str(7, self.owner.clone());
                let name = cp.insert_nameandtype(self.name.clone(), self.descriptor.to_string());
                cp.insert_raw(RawConstantEntry::Method(owner, name)).write_to(writer)?;
            }
            MethodHandleKind::InvokeInterface => {
                not_init(self)?;
                let owner = cp.insert_indirect_str(7, self.owner.clone());
                let name = cp.insert_nameandtype(self.name.clone(), self.descriptor.to_string());
                cp.insert_raw(RawConstantEntry::InterfaceMethod(owner, name)).write_to(writer)?;
            }
        }
        Ok(())
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Constant {
    Null,
    I32(i32),
    F32(f32),
    I64(i64),
    F64(f64),
    String(Cow<'static, str>),
    Class(Cow<'static, str>),
    Field {
        owner: Cow<'static, str>,
        name: Cow<'static, str>,
        descriptor: Type
    },
    Method {
        interface: bool,
        owner: Cow<'static, str>,
        name: Cow<'static, str>,
        descriptor: Type
    },
    MethodType(Type),
    MethodHandle(MethodHandle)
}

impl ConstantPoolReadWrite for Constant {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self> {
        let idx = ReadWrite::read_from(reader)?;
        cp.read_constant(idx).ok_or_else(|| crate::error::Error::Invalid("constant pool entry index", idx.to_string().into()))
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> Result<()> {
        ReadWrite::write_to(&cp.insert_constant(self), writer)
    }
}

impl Constant {
    pub const NULL: Constant = Constant::Null;
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
            Constant::Null => {}
            Constant::I32(i) => { i.hash(state); }
            Constant::F32(f) => { (*f).to_bits().hash(state); }
            Constant::I64(i) => { i.hash(state); }
            Constant::F64(f) => { (*f).to_bits().hash(state); }
            Constant::String(s) => { s.hash(state); }
            Constant::Class(s) => { s.hash(state); }
            Constant::MethodType(s) => { s.hash(state); }
            Constant::MethodHandle(m) => { m.hash(state) }
            Constant::Field { owner, name, descriptor } => {
                owner.hash(state);
                name.hash(state);
                descriptor.hash(state);
            }
            Constant::Method { interface, owner, name, descriptor } => {
                interface.hash(state);
                owner.hash(state);
                name.hash(state);
                descriptor.hash(state);
            }
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum StackValueType {
    /// Represents A stack value of computational type one. This should not be used when the stack type is a f64 or i64.
    One,
    /// Represents two stack values of computational type one, or one stack value of computational type two.
    Two
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum FloatType {
    Double,
    Float
}

impl From<FloatType> for StackValueType {
    #[inline]
    fn from(ft: FloatType) -> Self {
        match ft {
            FloatType::Double => StackValueType::Two,
            FloatType::Float => StackValueType::One
        }
    }
}
impl From<FloatType> for LocalType {
    #[inline]
    fn from(ft: FloatType) -> Self {
        match ft {
            FloatType::Double => LocalType::Double,
            FloatType::Float => LocalType::Float
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum NaNBehavior {
    ReturnsOne,
    ReturnsNegativeOne
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum JumpCondition {
    ReferenceEquals,
    ReferenceNotEquals,
    IntegerEquals,
    IntegerNotEquals,
    IntegerLessThan,
    IntegerGreaterThan,
    IntegerLessThanOrEquals,
    IntegerGreaterThanOrEquals,
    IntegerEqualsZero,
    IntegerNotEqualsZero,
    IntegerLessThanZero,
    IntegerGreaterThanZero,
    IntegerLessThanOrEqualsZero,
    IntegerGreaterThanOrEqualsZero,
    IsNull,
    IsNonNull,
    Always
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum LoadOrStore {
    Load, Store
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum GetOrPut {
    Get, Put
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum MemberType {
    Static, Virtual
}
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum LocalType {
    Int, Long, Float, Double, Reference
}
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum ArrayType {
    ByteOrBool, Short, Char, Int, Long, Float, Double, Reference
}
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum NumberType {
    Int, Long, Float, Double
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum BitType {
    Byte, Short, Char, Int, Long, Float, Double
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum IntType {
    Int, Long
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum FloatOperation {
    Divide, Add, Subtract, Multiply, Remainder, Negate
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum IntOperation {
    Divide, Add, Subtract, Multiply, Remainder, Negate, ExclusiveOr, Or, And, ShiftLeft, ShiftRight, UnsignedShiftRight
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum MonitorOperation {
    Enter, Exit
}


#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub enum Type {
    Byte, Char, Double, Float, Int, Long, Boolean, Short, Ref(Cow<'static, str>), ArrayRef(u8, Box<Type>),
    /// The Method type. First is the parameter list and second is the return type. If the return type is `None`, it represents a `void` return type.
    ///
    /// It is invalid if any of the parameter types and the return type is a method type.
    Method(Vec<Type>, Option<Box<Type>>)
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
                    while c.as_str().chars().next().unwrap_or(')') != ')' {
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
                        dim = dim.checked_add(1).ok_or(crate::error::Error::ArithmeticOverflow)?;
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
    pub fn method(params: Vec<Type>, ret: Option<Type>) -> Type {
        Type::Method(params, ret.map(Box::new))
    }
    #[inline]
    pub fn reference<S>(str: S) -> Type where S: Into<Cow<'static, str>> {
        Type::Ref(str.into())
    }
    pub fn array(dim: u8, t: Type) -> Type {
        Type::ArrayRef(dim, Box::new(t))
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, ReadWrite)]
#[tag_type(u8)]
pub enum MethodHandleKind {
    GetField = 1, GetStatic, PutField, PutStatic, InvokeVirtual, InvokeStatic, InvokeSpecial, NewInvokeSpecial, InvokeInterface
}

/// Note: dynamic computed constants are syntactically allowed to refer to themselves via the bootstrap method table but it will fail during resolution.
/// Rust ownership rules prevent us from doing so.
#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Dynamic {
    pub bsm: Box<BootstrapMethod>,
    pub name: Cow<'static, str>,
    pub descriptor: Type
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum OrDynamic<T> {
    Dynamic(Dynamic),
    Static(T)
}
impl<T> From<Dynamic> for OrDynamic<T> {
    #[inline]
    fn from(d: Dynamic) -> Self {
        OrDynamic::Dynamic(d)
    }
}

impl From<Constant> for OrDynamic<Constant> {
    #[inline]
    fn from(t: Constant) -> Self {
        OrDynamic::Static(t)
    }
}

/// Abstract tagged union to represent the instruction set.
/// Note that while each valid instruction corresponds to one and only one enum variant,
/// a value may correspond to multiple possibilities of actual operation used in bytecode.
/// Normally, it should choose the option that takes the lowest space.
///
/// Some variants don't actually appear in the code,
/// but instead they represent attributes of the Code attribute.
/// This gives benefits such that when modifying the class it doesn't need to modify the indices of the attributes to remain valid.
///
/// However, StackMap frames will not be a variant because they become quite invalid after modifications made to code, thus, frames should be regenerated every time.
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    /// Push a constant value to the current stack.
    Push(OrDynamic<Constant>),
    /// Duplicate one or two stack values and insert them zero or more values down.
    ///
    /// `Duplicate(One, None)` is equivalent to `DUP`
    ///
    /// `Duplicate(Two, Some(Two))` is equivalent to `DUP2_X2`
    Duplicate(StackValueType, Option<StackValueType>),
    /// Pop one or two values. Use `Pop(Two)` for double/long values.
    Pop(StackValueType),
    Jump(Label),
    CompareLongs,
    CompareFloats(FloatType, NaNBehavior),
    LocalVariable(LoadOrStore, LocalType, u16),
    Array(LoadOrStore, ArrayType),
    IntOperation(IntType, IntOperation),
    FloatOperation(FloatType, FloatOperation),
    Monitor(MonitorOperation),
    /// Conversion of the same types have no effect, it will not result in an instruction.
    Conversion(NumberType, NumberType),
    ConvertInt(BitType),
    Field(GetOrPut, MemberType),
    InvokeExact(Constant),
    LineNumber(u16),
    /// Not real in bytecode, used as a marker of location.
    Label(Label)
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

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct LocalVariable {
    pub start: Label,
    pub end: Label,
    pub name: Cow<'static, str>,
    pub descriptor: Type,
    pub signature: Option<FieldSignature>,
    pub index: u16
}

#[derive(Clone, PartialEq, Debug)]
pub enum CodeAttribute {
    LocalVarTable(Vec<LocalVariable>),
    TypeAnnotation(CodeTypeAnnotation),
    Frames(Vec<Frame>)
}

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub struct Catch {
    pub start: Label,
    pub end: Label,
    pub handler: Label,
    pub catch: Option<Cow<'static, str>>
}

impl ConstantPoolReadWrite for Catch {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self, Error> {
        try_cp_read!(cp, reader, get_catch)
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> Result<(), Error> {
        match cp.catch(self) {
            Some(off) => off.write_to(writer),
            // Ignore here because the block was probably removed.
            None => Ok(())
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Code {
    pub max_stack: u16,
    pub max_locals: u16,
    pub code: Vec<Instruction>,
    pub catches: Vec<Catch>,
    pub attrs: Vec<CodeAttribute>
}

/// Completed
#[derive(PartialEq, Debug, Clone)]
pub enum FieldAttribute {
    Deprecated,
    Synthetic,
    Signature(FieldSignature),
    ConstantValue(Constant),
    VisibleAnnotations(Vec<Annotation>),
    InvisibleAnnotations(Vec<Annotation>),
    VisibleTypeAnnotations(Vec<FieldTypeAnnotation>),
    InvisibleTypeAnnotations(Vec<FieldTypeAnnotation>),
    Raw(RawAttribute)
}

#[derive(PartialEq, Debug, Clone)]
pub struct Field {
    pub access: AccessFlags,
    pub name: Cow<'static, str>,
    pub descriptor: Type,
    pub attrs: Vec<FieldAttribute>
}

#[derive(PartialEq, Debug, Clone)]
pub enum MethodAttribute {
    Code(Code),
    Deprecated,
    Synthetic,
    Signature(MethodSignature),
    VisibleAnnotations(Vec<Annotation>),
    InvisibleAnnotations(Vec<Annotation>),
    VisibleTypeAnnotations(Vec<MethodTypeAnnotation>),
    InvisibleTypeAnnotations(Vec<MethodTypeAnnotation>),
    VisibleParameterAnnotations(Vec<Vec<Annotation>>),
    InvisibleParameterAnnotations(Vec<Vec<Annotation>>),
    Raw(RawAttribute),
    Exceptions(Vec<Cow<'static, str>>),
    AnnotationDefault(AnnotationValue),
    MethodParameters(Vec<(Cow<'static, str>, AccessFlags)>)
}

#[derive(PartialEq, Debug, Clone)]
pub struct Method {
    pub access: AccessFlags,
    pub name: Cow<'static, str>,
    pub descriptor: Type,
    pub attributes: Vec<MethodAttribute>
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum VerificationType {
    Top, Int, Float, Null, UninitializedThis, Object(Cow<'static, str>),
    /// Following the label, must be a `NEW` instruction.
    UninitializedVariable(Label), Long, Double
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub enum Chop {
    One, Two, Three
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Frame {
    Same(u16), SameLocalsOneStack(u16, VerificationType),
    /// Chop up to three.
    Chop(u16, u8),
    /// At most three items.
    Append(u16, Vec<VerificationType>),
    /// Locals and then stack values.
    Full(u16, Vec<VerificationType>, Vec<VerificationType>)
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

#[derive(Clone, Eq, PartialEq, Debug, ConstantPoolReadWrite)]
pub struct Export {
    #[str_type(Package)]
    pub package: Cow<'static, str>,
    #[use_normal_rw]
    pub flags: AccessFlags,
    /// not exported; use the function to get the strings.
    #[vec_len_type(u16)]
    pub to: Vec<To>
}
impl Export {
    pub fn new<ToStr: Into<Cow<'static, str>>>(pkg: ToStr, flags: AccessFlags, to: Vec<Cow<'static, str>>) -> Self {
        unsafe {
            Self {
                package: pkg.into(),
                flags,
                to: std::mem::transmute(to)
            }
        }
    }
    pub fn to(&self) -> &Vec<Cow<'static, str>> {
        // SAFETY: `To` is equivalent to a Cow<'static, str>, so it is safe to cast the pointer.
        unsafe { &*(&self.to as *const std::vec::Vec<To> as *const std::vec::Vec<std::borrow::Cow<str>>) }
    }
}

#[derive(Clone, Eq, PartialEq, Debug, ConstantPoolReadWrite)]
pub struct Open {
    #[str_type(Package)]
    pub package: Cow<'static, str>,
    #[use_normal_rw]
    pub flags: AccessFlags,
    #[vec_len_type(u16)]
    pub to: Vec<To>
}
impl Open {
    pub fn new<ToStr: Into<Cow<'static, str>>>(pkg: ToStr, flags: AccessFlags, to: Vec<Cow<'static, str>>) -> Self {
        unsafe {
            Self {
                package: pkg.into(),
                flags,
                to: std::mem::transmute(to)
            }
        }
    }
    pub fn to(&self) -> &Vec<Cow<'static, str>> {
        // SAFETY: `To` is equivalent to a Cow<'static, str>, so it is safe to cast the pointer.
        unsafe { &*(&self.to as *const std::vec::Vec<To> as *const std::vec::Vec<std::borrow::Cow<str>>) }
    }
}

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
    /// constants must not be null. They don't have a corresponding constant pool entry.
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