
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
pub use signature::*;
use std::borrow::Cow;
use std::hash::{Hash, Hasher};
use std::ops::Range;

#[derive(Debug, Eq, PartialOrd, PartialEq, Ord, Hash, Copy, Clone)]
pub struct Label(u32);

#[derive(Clone, PartialEq, Hash, Debug)]
pub struct MethodHandle<'a> {
    kind: MethodHandleKind,
    owner: Cow<'a, str>,
    name: Cow<'a, str>,
    descriptor: Type<'a>
}

#[derive(Clone, PartialEq, Debug)]
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
pub enum Type<'a> {
    Byte, Char, Double, Float, Int, Long, Boolean, Ref(Cow<'a, str>), ArrayRef(u8, Box<Type<'a>>),
    /// The Method type. First is the parameter list and second is the return type. If the return type is `None`, it represents a `void` return type.
    ///
    /// It is invalid if any of the parameter types and the return type is a method type.
    Method(Vec<Type<'a>>, Option<Box<Type<'a>>>)
}

impl<'a> Display for Type<'a> {
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
            Type::Ref(s) => {
                write!(f, "L{};", s)
            }
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

impl<'a> Type<'a> {
    #[inline]
    pub fn method(params: Vec<Type<'a>>, ret: Option<Type<'a>>) -> Type<'a> {
        Type::Method(params, ret.map(Box::new))
    }
    #[inline]
    pub fn reference<S>(str: S) -> Type<'a> where S: Into<Cow<'a, str>> {
        Type::Ref(str.into())
    }
    pub fn array(dim: u8, t: Type<'a>) -> Type<'a> {
        Type::ArrayRef(dim, Box::new(t))
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum MethodHandleKind {
    GetField = 1, GetStatic, PutField, PutStatic, InvokeVirtual, InvokeStatic, InvokeSpecial, NewInvokeSpecial, InvokeInterface
}

/// Note: dynamic computed constants are syntactically allowed to refer to themselves via the bootstrap method table but it will fail during resolution.
/// Rust ownership rules prevent us from doing so.
#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Dynamic<'a> {
    pub bsm: Box<BootstrapMethod<'a>>,
    pub name: Cow<'a, str>,
    pub descriptor: Type<'a>
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum OrDynamic<'a, T> {
    Dynamic(Dynamic<'a>),
    Static(T)
}
impl<'a, T> From<Dynamic<'a>> for OrDynamic<'a, T> {
    #[inline]
    fn from(d: Dynamic<'a>) -> Self {
        OrDynamic::Dynamic(d)
    }
}

impl<'a> From<Constant<'a>> for OrDynamic<'a, Constant<'a>> {
    #[inline]
    fn from(t: Constant<'a>) -> Self {
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
    /// None means catch everything
    Catch(Option<Cow<'a, str>>),
    /// Not real in bytecode, used as a marker of location.
    Label(Label)
}

#[derive(Clone, PartialEq, Debug)]
pub struct RawAttribute<'a> {
    /// Whether to keep this attribute upon writing.
    /// Attributes that are related to local variables will default to `false`, whereas newly created attributes will be `true`.
    pub keep: bool,
    pub name: Cow<'a, str>,
    pub inner: Cow<'a, [u8]>
}

/// All `usize` fields represent indexes into the vector of instructions.
///
/// The end is exclusive, as defined by the JVM specification as well as the `Range` struct.
pub struct Exception<'a> {
    pub range: Range<usize>,
    pub handler: usize,
    pub catch_type: Option<Cow<'a, str>>
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct LocalVariable<'a> {
    pub start: Label,
    pub end: Label,
    pub name: Cow<'a, str>,
    pub descriptor: Type<'a>,
    pub signature: Option<FieldSignature<'a>>,
    pub index: u16
}

#[derive(Clone, PartialEq, Debug)]
pub enum CodeAttribute<'a> {
    LocalVarTable(Vec<LocalVariable<'a>>),
    TypeAnnotation(CodeTypeAnnotation<'a>),
    Frames(Vec<Frame<'a>>)
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Catch<'a> {
    pub start: Label,
    pub end: Label,
    pub handler: Label,
    pub catch: Cow<'a, str>
}

#[derive(Clone, PartialEq, Debug)]
pub struct Code<'a> {
    pub max_stack: u16,
    pub max_locals: u16,
    pub code: Vec<Instruction<'a>>,
}

use crate::access::AccessFlags;

use std::fmt::Display;
use std::fmt::Formatter;
use annotation::Annotation;
use crate::full::annotation::{FieldTypeAnnotation, MethodTypeAnnotation, CodeTypeAnnotation, AnnotationValue};
use crate::full::version::JavaVersion;

/// Completed
#[derive(PartialEq, Debug, Clone)]
pub enum FieldAttribute<'a> {
    Deprecated,
    Synthetic,
    Signature(FieldSignature<'a>),
    ConstantValueInt(i32),
    ConstantValueFloat(f32),
    ConstantValueLong(i64),
    ConstantValueDouble(f64),
    ConstantValueString(Cow<'a, str>),
    VisibleAnnotations(Vec<Annotation<'a>>),
    InvisibleAnnotations(Vec<Annotation<'a>>),
    VisibleTypeAnnotations(Vec<FieldTypeAnnotation<'a>>),
    InvisibleTypeAnnotations(Vec<FieldTypeAnnotation<'a>>),
    Raw(RawAttribute<'a>)
}

#[derive(PartialEq, Debug, Clone)]
pub struct Field<'a> {
    pub access: AccessFlags,
    pub name: Cow<'a, str>,
    pub descriptor: Type<'a>,
    pub attrs: Vec<FieldAttribute<'a>>
}

#[derive(PartialEq, Debug, Clone)]
pub enum MethodAttribute<'a> {
    Code(Code<'a>),
    Deprecated,
    Synthetic,
    Signature(MethodSignature<'a>),
    VisibleAnnotations(Vec<Annotation<'a>>),
    InvisibleAnnotations(Vec<Annotation<'a>>),
    VisibleTypeAnnotations(Vec<MethodTypeAnnotation<'a>>),
    InvisibleTypeAnnotations(Vec<MethodTypeAnnotation<'a>>),
    VisibleParameterAnnotations(Vec<Vec<Annotation<'a>>>),
    InvisibleParameterAnnotations(Vec<Vec<Annotation<'a>>>),
    Raw(RawAttribute<'a>),
    Exceptions(Vec<Cow<'a, str>>),
    AnnotationDefault(AnnotationValue<'a>),
    MethodParameters(Vec<(Cow<'a, str>, AccessFlags)>)
}

#[derive(PartialEq, Debug, Clone)]
pub struct Method<'a> {
    pub access: AccessFlags,
    pub name: Cow<'a, str>,
    pub descriptor: Type<'a>,
    pub attributes: Vec<MethodAttribute<'a>>
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum VerificationType<'a> {
    Top, Int, Float, Null, UninitializedThis, Object(Cow<'a, str>),
    /// Following the label, must be a `NEW` instruction.
    UninitializedVariable(Label), Long, Double
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub enum Chop {
    One, Two, Three
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Frame<'a> {
    Same(u16), SameLocalsOneStack(u16, VerificationType<'a>),
    /// Chop up to three.
    Chop(u16, u8),
    /// At most three items.
    Append(u16, Vec<VerificationType<'a>>),
    /// Locals and then stack values.
    Full(u16, Vec<VerificationType<'a>>, Vec<VerificationType<'a>>)
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct InnerClass<'a> {
    pub inner_fqname: Cow<'a, str>,
    pub outer_fqname: Cow<'a, str>,
    /// None if the inner class is an anonymous class.
    pub inner_name: Option<Cow<'a, str>>,
    pub inner_access: AccessFlags
}

#[derive(PartialEq, Debug, Clone)]
pub struct Module<'a> {
    pub name: Cow<'a, str>,
    pub flags: AccessFlags,
    pub version: Option<Cow<'a, str>>,
    /// module name, requires flags, module version
    pub requires: Vec<(Cow<'a, str>, AccessFlags, Option<Cow<'a, str>>)>,
    /// package name, exports flags, export to modules (empty = export to all)
    pub exports: Vec<(Cow<'a, str>, AccessFlags, Vec<Cow<'a, str>>)>,
    pub opens: Vec<(Cow<'a, str>, AccessFlags, Vec<Cow<'a, str>>)>,
    pub uses: Vec<Cow<'a, str>>,
    /// service interface fqname, service impls fqnames
    pub provides: Vec<(Cow<'a, str>, Vec<Cow<'a, str>>)>
}

#[derive(PartialEq, Debug, Clone)]
pub enum ClassAttribute<'a> {
    Signature(ClassSignature<'a>),
    Synthetic, Deprecated, SourceFile(Cow<'a, str>), InnerClasses(Vec<InnerClass<'a>>),
    /// first: fully qualified name of the innermost outer class.
    /// second: name of the method that encloses this inner/anonymous class.
    /// third: descriptor of the method.
    EnclosingMethod(Cow<'a, str>, Cow<'a, str>, Type<'a>), SourceDebugExtension(Cow<'a, str>),
    BootstrapMethods(Vec<BootstrapMethod<'a>>), Module(Module<'a>)
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct BootstrapMethod<'a> {
    pub method: MethodHandle<'a>,
    /// constants must not be null. They don't have a corresponding constant pool entry.
    pub arguments: Vec<OrDynamic<'a, Constant<'a>>>
}

#[derive(PartialEq, Debug, Clone)]
pub struct Class<'a> {
    pub version: JavaVersion,
    pub access: AccessFlags,
    pub name: Cow<'a, str>,
    /// java/lang/Object has no superclass.
    pub super_name: Option<Cow<'a, str>>,
    pub interfaces: Vec<Cow<'a, str>>,
    pub fields: Vec<Field<'a>>,
    pub methods: Vec<Method<'a>>,
    pub attributes: Vec<ClassAttribute<'a>>
}