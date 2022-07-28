use crate::{annotation::CodeTypeAnnotation, prelude::*};
use indexmap::map::IndexMap;

/// Acts as a unique identifier to the code. Labels should be treated carefully because when labels become invalid (i.e. removed from the code array) it will become an error.
#[derive(Debug, Eq, PartialOrd, PartialEq, Ord, Hash, Copy, Clone)]
pub struct Label(pub u32);

impl ConstantPoolReadWrite for Label {
    fn read_from<C: ConstantPoolReader, R: Read>(
        cp: &mut C,
        reader: &mut R,
    ) -> crate::Result<Self> {
        Ok(cp.get_label(u16::read_from(reader)? as _))
    }
    fn write_to<C: ConstantPoolWriter, W: Write>(
        &self,
        cp: &mut C,
        writer: &mut W,
    ) -> crate::Result<()> {
        ReadWrite::write_to(&cp.label(self), writer)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum StackValueType {
    /// Represents A stack value of computational type one. This should not be used when the stack type is a f64 or i64.
    One,
    /// Represents two stack values of computational type one, or one stack value of computational type two.
    Two,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum FloatType {
    Double,
    Float,
}

impl From<FloatType> for StackValueType {
    #[inline]
    fn from(ft: FloatType) -> Self {
        match ft {
            FloatType::Double => StackValueType::Two,
            FloatType::Float => StackValueType::One,
        }
    }
}

impl From<FloatType> for LocalType {
    #[inline]
    fn from(ft: FloatType) -> Self {
        match ft {
            FloatType::Double => LocalType::Double,
            FloatType::Float => LocalType::Float,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum NaNBehavior {
    ReturnsOne,
    ReturnsNegativeOne,
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
    Always,
}

impl From<JumpCondition> for u8 {
    fn from(j: JumpCondition) -> Self {
        use crate::constants::insn::*;
        match j {
            JumpCondition::ReferenceEquals => IFACMPEQ,
            JumpCondition::ReferenceNotEquals => IFACMPNE,
            JumpCondition::IntegerEquals => IFICMPEQ,
            JumpCondition::IntegerNotEquals => IFICMPNE,
            JumpCondition::IntegerLessThan => IFICMPLT,
            JumpCondition::IntegerGreaterThan => IFICMPGT,
            JumpCondition::IntegerLessThanOrEquals => IFICMPLE,
            JumpCondition::IntegerGreaterThanOrEquals => IFICMPGE,
            JumpCondition::IntegerEqualsZero => IFEQ,
            JumpCondition::IntegerNotEqualsZero => IFNE,
            JumpCondition::IntegerLessThanZero => IFLT,
            JumpCondition::IntegerGreaterThanZero => IFGT,
            JumpCondition::IntegerLessThanOrEqualsZero => IFLE,
            JumpCondition::IntegerGreaterThanOrEqualsZero => IFGE,
            JumpCondition::IsNull => IFNULL,
            JumpCondition::IsNonNull => IFNONNULL,
            JumpCondition::Always => GOTO,
        }
    }
}

impl std::ops::Neg for JumpCondition {
    type Output = Option<JumpCondition>;

    #[inline]
    fn neg(self) -> Self::Output {
        Some(match self {
            JumpCondition::ReferenceEquals => JumpCondition::ReferenceNotEquals,
            JumpCondition::ReferenceNotEquals => JumpCondition::ReferenceEquals,
            JumpCondition::IntegerNotEquals => JumpCondition::IntegerEquals,
            JumpCondition::IntegerEquals => JumpCondition::IntegerNotEquals,
            JumpCondition::IntegerLessThan => JumpCondition::IntegerGreaterThanOrEquals,
            JumpCondition::IntegerGreaterThan => JumpCondition::IntegerLessThanOrEquals,
            JumpCondition::IntegerLessThanOrEquals => JumpCondition::IntegerGreaterThan,
            JumpCondition::IntegerGreaterThanOrEquals => JumpCondition::IntegerLessThan,
            JumpCondition::IntegerEqualsZero => JumpCondition::IntegerNotEqualsZero,
            JumpCondition::IntegerNotEqualsZero => JumpCondition::IntegerEqualsZero,
            JumpCondition::IntegerLessThanZero => JumpCondition::IntegerGreaterThanOrEqualsZero,
            JumpCondition::IntegerGreaterThanZero => JumpCondition::IntegerLessThanOrEqualsZero,
            JumpCondition::IntegerLessThanOrEqualsZero => JumpCondition::IntegerGreaterThanZero,
            JumpCondition::IntegerGreaterThanOrEqualsZero => JumpCondition::IntegerLessThanZero,
            JumpCondition::IsNull => JumpCondition::IsNonNull,
            JumpCondition::IsNonNull => JumpCondition::IsNull,
            JumpCondition::Always => return None,
        })
    }
}

impl std::ops::Neg for &JumpCondition {
    type Output = Option<JumpCondition>;

    fn neg(self) -> Self::Output {
        (*self).neg()
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum LoadOrStore {
    Load,
    Store,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum GetOrPut {
    Get,
    Put,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum MemberType {
    Static,
    Virtual,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum LocalType {
    Int,
    Long,
    Float,
    Double,
    Reference,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum ArrayType {
    ByteOrBool,
    Short,
    Char,
    Int,
    Long,
    Float,
    Double,
    Reference,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum NumberType {
    Int,
    Long,
    Float,
    Double,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum BitType {
    Byte,
    Short,
    Char,
    Int,
    Long,
    Float,
    Double,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum IntType {
    Int,
    Long,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum FloatOperation {
    Divide,
    Add,
    Subtract,
    Multiply,
    Remainder,
    Negate,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum IntOperation {
    Divide,
    Add,
    Subtract,
    Multiply,
    Remainder,
    Negate,
    ExclusiveOr,
    Or,
    And,
    ShiftLeft,
    ShiftRight,
    UnsignedShiftRight,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum MonitorOperation {
    Enter,
    Exit,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ClassType {
    Object(Cow<'static, str>),
    Array(u8, Type),
}

impl From<ClassType> for Cow<'static, str> {
    fn from(t: ClassType) -> Self {
        match t {
            ClassType::Object(s) => s,
            ClassType::Array(dim, ty) => Cow::Owned(format!("{}{}", "[".repeat(dim as usize), ty)),
        }
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
    NoOp,
    /// Push a null object reference.
    PushNull,
    /// Push a constant value to the current stack.
    Push(OrDynamic<Constant>),
    Dup,
    DupX1,
    DupX2,
    Dup2,
    Dup2X1,
    Dup2X2,
    Pop1,
    Pop2,
    Jump(JumpCondition, Label),
    CompareLongs,
    CompareFloats(FloatType, NaNBehavior),
    LocalVariable(LoadOrStore, LocalType, u16),
    Array(LoadOrStore, ArrayType),
    ArrayLength,
    IntOperation(IntType, IntOperation),
    FloatOperation(FloatType, FloatOperation),
    Throw,
    CheckCast(OrDynamic<ClassType>),
    InstanceOf(OrDynamic<ClassType>),
    NewArray(OrDynamic<Type>, u8),
    Monitor(MonitorOperation),
    New(OrDynamic<Cow<'static, str>>),
    /// Conversion of the same types have no effect, it will not result in an instruction.
    Conversion(NumberType, NumberType),
    ConvertInt(BitType),
    Return(Option<LocalType>),
    Field(GetOrPut, MemberType, OrDynamic<MemberRef>),
    InvokeExact(MemberType, OrDynamic<MemberRef>),
    InvokeSpecial(OrDynamic<MemberRef>),
    InvokeDynamic(Dynamic),
    InvokeInterface(OrDynamic<MemberRef>, u8),
    Jsr(Label),
    Ret(u16),
    Swap,
    IntIncrement(u16, i16),
    LineNumber(u16),
    TableSwitch {
        default: Label,
        low: i32,
        offsets: Vec<Label>,
    },
    LookupSwitch {
        default: Label,
        table: IndexMap<i32, Label>,
    },
    /// Not real in bytecode, used as a marker of location.
    Label(Label),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct LocalVariable {
    pub start: Label,
    pub end: Label,
    pub name: Cow<'static, str>,
    pub descriptor: Option<Type>,
    pub signature: Option<FieldSignature>,
    pub index: u16,
}

/// A line number for debugging, to use this in code, you can use the [`LineNumber`](crate::code::Instruction::LineNumber) variant.
#[derive(Clone, Copy, Eq, PartialEq, ReadWrite, Debug)]
pub struct LineNumber(pub u16, pub u16);

#[derive(Clone, Eq, PartialEq, Debug, ConstantPoolReadWrite)]
pub(super) struct LocalVar {
    #[use_normal_rw]
    pub start: u16,
    #[use_normal_rw]
    pub len: u16,
    pub name: Cow<'static, str>,
    pub descriptor: Type,
    #[use_normal_rw]
    pub index: u16,
}

#[derive(Clone, Eq, PartialEq, Debug, ConstantPoolReadWrite)]
pub struct LocalVarType {
    #[use_normal_rw]
    pub start: u16,
    #[use_normal_rw]
    pub len: u16,
    pub name: Cow<'static, str>,
    pub signature: FieldSignature,
    #[use_normal_rw]
    pub index: u16,
}

#[derive(Clone, PartialEq, Debug, ConstantPoolReadWrite)]
#[attr_enum]
pub(super) enum CodeAttr {
    LineNumberTable(
        #[vec_len_type(u16)]
        #[use_normal_rw]
        Vec<LineNumber>,
    ),
    LocalVariableTable(#[vec_len_type(u16)] Vec<LocalVar>),
    LocalVariableTypeTable(#[vec_len_type(u16)] Vec<LocalVarType>),
    RuntimeInvisibleTypeAnnotations(#[vec_len_type(u16)] Vec<CodeTypeAnnotation>),
    RuntimeVisibleTypeAnnotations(#[vec_len_type(u16)] Vec<CodeTypeAnnotation>),
    StackMapTable(#[vec_len_type(u16)] Vec<RawFrame>),
    #[raw_variant]
    Raw(RawAttribute),
}

#[derive(Clone, PartialEq, Debug)]
pub enum CodeAttribute {
    VisibleTypeAnnotations(Vec<CodeTypeAnnotation>),
    InvisibleTypeAnnotations(Vec<CodeTypeAnnotation>),
    LocalVariables(Vec<LocalVariable>),
    Raw(RawAttribute),
}

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub struct Catch {
    pub start: Label,
    pub end: Label,
    pub handler: Label,
    pub catch: Option<Cow<'static, str>>,
}

impl ConstantPoolReadWrite for Catch {
    fn read_from<C: ConstantPoolReader, R: Read>(
        cp: &mut C,
        reader: &mut R,
    ) -> crate::Result<Self, Error> {
        try_cp_read!(cp, reader, get_catch)
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(
        &self,
        cp: &mut C,
        writer: &mut W,
    ) -> crate::Result<(), Error> {
        match cp.catch(self) {
            Some(off) => off.write_to(writer),
            // Ignore here because the block was probably removed.
            None => Ok(()),
        }
    }
}
