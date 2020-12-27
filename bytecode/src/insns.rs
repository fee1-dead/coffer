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
use std::hash::{Hash, Hasher};
use std::ops::Range;

#[derive(Clone, PartialEq, Hash)]
pub struct MethodHandle<'a> {
    kind: MethodHandleKind,
    owner: Cow<'a, str>,
    name: Cow<'a, str>,
    descriptor: Type<'a>
}

#[derive(Clone, PartialEq)]
pub enum Constant<'a> {
    Null,
    I32(i32),
    F32(f32),
    I64(i64),
    F64(f64),
    String(Cow<'a, str>),
    Class(Cow<'a, str>),
    Field {
        owner: Cow<'a, str>,
        name: Cow<'a, str>,
        descriptor: Type<'a>
    },
    Method {
        interface: bool,
        owner: Cow<'a, str>,
        name: Cow<'a, str>,
        descriptor: Type<'a>
    },
    MethodType(Type<'a>),
    MethodHandle(MethodHandle<'a>)
}

impl Constant<'static> {
    pub const NULL: Constant<'static> = Constant::Null;
}

impl From<i32> for Constant<'_> {
    fn from(i: i32) -> Self {
        Self::I32(i)
    }
}

impl<'a> Hash for Constant<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Constant::Null => {
                state.write(&[0]);
            }
            Constant::I32(i) => {
                state.write(&[1]);
                i.hash(state);
            }
            Constant::F32(f) => {
                state.write(&[2]);
                (*f).to_bits().hash(state);
            }
            Constant::I64(i) => {
                state.write(&[3]);
                i.hash(state);
            }
            Constant::F64(f) => {
                state.write(&[4]);
                (*f).to_bits().hash(state);
            }
            Constant::String(s) => {
                state.write(&[5]);
                s.hash(state);
            }
            Constant::Class(s) => {
                state.write(&[6]);
                s.hash(state);
            }
            Constant::MethodType(s) => {
                state.write(&[7]);
                s.hash(state);
            }
            Constant::MethodHandle(m) => {
                state.write(&[8]);
                m.hash(state)
            }
            Constant::Field { owner, name, descriptor } => {
                state.write(&[9]);
                owner.hash(state);
                name.hash(state);
                descriptor.hash(state);
            }
            Constant::Method { interface, owner, name, descriptor } => {
                state.write(&[10]);
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
    Load,
    Store
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
    Enter,
    Exit
}


#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub enum Type<'a> {
    Byte, Char, Double, Float, Int, Long, Ref(Cow<'a, str>), ArrayRef(u8, Cow<'a, str>),
    /// The Method type. First is the parameter list and second is the return type. If the return type is `None`, it represents a `void` return type.
    ///
    /// It is invalid if any of the parameter types and the return type is a method type.
    Method(Vec<Type<'a>>, Option<Box<Type<'a>>>)
}

impl Type<'static> {
    pub const BYTE: Type<'static> = Type::Byte;
    pub const CHAR: Type<'static> = Type::Char;
    pub const DOUBLE: Type<'static> = Type::Double;
    pub const FLOAT: Type<'static> = Type::Float;
    pub const INT: Type<'static> = Type::Int;
    pub const LONG: Type<'static> = Type::Long;
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum MethodHandleKind {
    GetField = 1, GetStatic, PutField, PutStatic, InvokeVirtual, InvokeStatic, InvokeSpecial, NewInvokeSpecial, InvokeInterface
}


pub struct Dynamic<'a> {
    pub bsm: MethodHandle<'a>,
    pub args: Vec<Constant<'a>>,
    pub name: Cow<'a, str>,
    pub descriptor: Type<'a>
}

pub trait CanBeDynamic {}
impl CanBeDynamic for Constant<'_> {}
// todo determine more types or eliminate to just Constant

pub enum OrDynamic<'a, T: CanBeDynamic> {
    Dynamic(Dynamic<'a>),
    Static(T)
}
impl<'a, T: CanBeDynamic> From<Dynamic<'a>> for OrDynamic<'a, T> {
    #[inline]
    fn from(d: Dynamic<'a>) -> Self {
        OrDynamic::Dynamic(d)
    }
}
impl<'a, T: CanBeDynamic> From<T> for OrDynamic<'a, T> {
    #[inline]
    fn from(t: T) -> Self {
        OrDynamic::Static(t)
    }
}

/// Abstract tagged union to represent the instruction set.
/// Note that while each valid instruction corresponds to one and only one enum variant,
/// a value may correspond to multiple possibilities of actual operation used in bytecode.
/// Normally, it should choose the option that takes the lowest space.
pub enum Instruction<'a> {
    /// Push a constant value to the current stack.
    Push(OrDynamic<'a, Constant<'a>>),
    /// Duplicate one or two stack values and insert them zero or more values down.
    ///
    /// `Duplicate(One, None)` is equivalent to `DUP`
    ///
    /// `Duplicate(Two, Some(Two))` is equivalent to `DUP2_X2`
    Duplicate(StackValueType, Option<StackValueType>),
    /// Pop one or two values. Use `Pop(Two)` for double/long values.
    Pop(StackValueType),
    /// Jump to an absolute position in the index of the owner
    Jump(usize),
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
    InvokeExact(Constant<'a>),
    LineNumber(u16),
    Try,
    Catch(Option<Cow<'a, str>>)
}

pub struct RawAttribute<T> {
    /// Whether to keep this attribute upon writing.
    /// Attributes that are related to local variables will default to `false`, whereas newly created attributes will be `true`.
    pub keep: bool,
    pub inner: T
}

/// All `usize` fields represent indexes into the vector of instructions.
///
/// The end is exclusive, as defined by the JVM specification as well as the `Range` struct.
pub struct Exception<'a> {
    pub range: Range<usize>,
    pub handler: usize,
    pub catch_type: Option<Cow<'a, str>>
}

pub struct Code<'a> {
    pub max_stack: u16,
    pub max_locals: u16,
    pub code: Vec<Instruction<'a>>,
}

use crate::access::AccessFlags;
use std::collections::HashMap;

/// Some values actually becomes ints in the constant pool.
#[derive(Clone, PartialEq)]
pub enum AnnotationValue<'a> {
    Byte(i8),
    Char(u16),
    Double(f64),
    Float(f32),
    Int(i32),
    Long(i64),
    Short(i16),
    Boolean(bool),
    String(Cow<'a, str>),
    Enum(Type<'a>, Cow<'a, str>),
    Class(Option<Type<'a>>),
    Annotation(Type<'a>, HashMap<Cow<'a, str>, AnnotationValue<'a>>),
    Array(Vec<AnnotationValue<'a>>)
}

/// Incomplete
#[derive(Clone, PartialEq)]
pub enum TypeAnnotationTarget {
    TypeParameter(u8),
    SuperType(u16),
    TypeParameterBound(u8, u8),
    Empty,
    FormalParameter(u8),
    Throws(u16),
    // LocalVar, // Not Yet available before I figure out
    Catch(u16),
    Offset(u16),
    TypeArgument(u16, u8)
}

#[derive(Clone, PartialEq)]
pub struct TypeAnnotation<'a> {
    pub target_b: u8,
    pub target: TypeAnnotationTarget,
    pub type_path: Vec<(u8, u8)>,
    pub annotation_type: Type<'a>,
    pub element_values: HashMap<Cow<'a, str>, AnnotationValue<'a>>
}

#[derive(Clone, PartialEq)]
pub struct Annotation<'a> {
    pub annotation_type: Type<'a>,
    pub element_values: HashMap<Cow<'a, str>, AnnotationValue<'a>>
}

/// Completed
#[derive(Clone, PartialEq)]
pub enum FieldAttribute<'a> {
    Deprecated,
    Synthetic,
    Signature(Cow<'a, str>),
    ConstantValueInt(i32),
    ConstantValueFloat(f32),
    ConstantValueLong(i64),
    ConstantValueDouble(f64),
    ConstantValueString(Cow<'a, str>),
    VisibleAnnotations(Vec<Annotation<'a>>),
    InvisibleAnnotations(Vec<Annotation<'a>>),
    VisibleTypeAnnotations(Vec<TypeAnnotation<'a>>),
    InvisibleTypeAnnotations(Vec<TypeAnnotation<'a>>)
}

pub struct Field<'a> {
    pub access: AccessFlags,
    pub name: Cow<'a, str>,
    pub descriptor: Type<'a>,
    pub attrs: Vec<FieldAttribute<'a>>
}

pub struct Method<'a> {
    pub access: AccessFlags,
    pub name: Cow<'a, str>,
    pub descriptor: Type<'a>
}

pub struct Class {

}