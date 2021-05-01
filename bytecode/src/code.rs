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

//! Structures that represent instructions that will be
//! executed when a method is called.

use std::collections::HashMap;
use std::convert::TryFrom;
use std::hash::Hash;
use std::io::{Cursor, Read, Write};
use std::rc::Rc;
use std::str::FromStr;

use indexmap::map::IndexMap;
use nom::lib::std::borrow::Cow;

use crate::annotation::CodeTypeAnnotation;
use crate::prelude::*;
use crate::{
    read_from, try_cp_read, try_cp_read_idx, ConstantPoolReadWrite, ConstantPoolReader,
    ConstantPoolWriter, Error, ReadWrite,
};

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
struct LocalVar {
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
enum CodeAttr {
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

#[derive(Clone, PartialEq, Debug, Default)]
pub struct Code {
    pub max_stack: u16,
    pub max_locals: u16,
    pub code: Vec<Instruction>,
    pub catches: Vec<Catch>,
    pub attrs: Vec<CodeAttribute>,
}

impl ConstantPoolReadWrite for Code {
    fn read_from<C: ConstantPoolReader, R: Read>(
        cp: &mut C,
        reader: &mut R,
    ) -> crate::Result<Self, Error> {
        use crate::code::GetOrPut::*;
        use crate::code::Instruction::*;
        use crate::code::LoadOrStore::*;
        use crate::code::MemberType::*;
        use crate::code::{
            FloatOperation as FOp, IntOperation as IOp, Label as Lbl, LocalVariable as LocalVar,
        };
        use crate::insn::{Instruction as I, SwitchEntry, TableSwitch as TblS, Wide};
        use std::io::{Seek, SeekFrom};

        struct Labeler<'a, T: ConstantPoolReader> {
            inner: &'a mut T,
            labels: HashMap<u32, Lbl>,
            catches: &'a [Catch],
        }
        impl<'a, T: ConstantPoolReader> ConstantPoolReader for Labeler<'a, T> {
            fn read_raw(&mut self, idx: u16) -> Option<RawConstantEntry> {
                self.inner.read_raw(idx)
            }

            fn resolve_later(&mut self, bsm_idx: u16, bsm: Rc<LazyBsm>) {
                self.inner.resolve_later(bsm_idx, bsm)
            }

            fn bootstrap_methods(&mut self, bsms: &[BootstrapMethod]) -> Result<()> {
                self.inner.bootstrap_methods(bsms)
            }

            fn get_label(&mut self, idx: u32) -> Lbl {
                if let Some(v) = self.labels.get(&idx) {
                    *v
                } else {
                    let l = Lbl(self.labels.len() as u32);
                    self.labels.insert(idx, l);
                    l
                }
            }

            fn get_catch(&mut self, idx: u16) -> Option<Catch> {
                self.catches.get(idx as usize).cloned()
            }
        }
        let max_stack = u16::read_from(reader)?;
        let max_locals = u16::read_from(reader)?;
        let mut code = vec![0; u32::read_from(reader)? as usize];
        reader.read_exact(&mut code)?;
        let mut labeler = Labeler {
            inner: cp,
            labels: HashMap::new(),
            catches: &[],
        };
        let len = code.len();
        let mut code_reader = Cursor::new(code);
        let mut instructions = Vec::new();
        let mut pos2idx = HashMap::new();
        while (code_reader.position() as usize) < len {
            let curpos = code_reader.position();
            pos2idx.insert(curpos as u32, instructions.len());
            let opcode = code_reader.get_ref()[curpos as usize];
            let insn = match opcode {
                crate::constants::insn::TABLESWITCH | crate::constants::insn::LOOKUPSWITCH => {
                    // pad 0-3 bytes to align properly
                    code_reader.seek(SeekFrom::Current((4 - (curpos & 3)) as i64))?;
                    let op = [opcode];
                    let mut temp_read = (&op).chain(&mut code_reader);
                    crate::insn::Instruction::read_from(&mut temp_read)?
                }
                _ => crate::insn::Instruction::read_from(&mut code_reader)?,
            };
            macro_rules! lbl {
                ($off:expr) => {{
                    labeler.get_label((curpos as i64 + $off as i64) as u32)
                }};
            }
            #[inline]
            fn push<C: Into<OrDynamic<Constant>>>(c: C) -> Instruction {
                Push(c.into())
            }
            let insn = match insn {
                I::AThrow => Throw,
                I::Nop => NoOp,
                I::AConstNull => PushNull,
                I::Swap => Swap,
                I::LCmp => CompareLongs,
                I::ArrayLength => ArrayLength,
                I::MonitorEnter => Monitor(MonitorOperation::Enter),
                I::MonitorExit => Monitor(MonitorOperation::Exit),

                I::IInc(idx, val) => IntIncrement(idx as u16, val as i16),
                I::Wide(Wide::IInc(idx, val)) => IntIncrement(idx, val),

                I::Pop => Pop1,
                I::Pop2 => Pop2,

                I::Dup => Dup,
                I::Dup2 => Dup2,
                I::Dupx1 => DupX1,
                I::Dupx2 => DupX2,
                I::Dup2x1 => Dup2X1,
                I::Dup2x2 => Dup2X2,

                I::IConstM1 => push(Constant::I32(-1)),
                I::IConst0 => push(Constant::I32(0)),
                I::IConst1 => push(Constant::I32(1)),
                I::IConst2 => push(Constant::I32(2)),
                I::IConst3 => push(Constant::I32(3)),
                I::IConst4 => push(Constant::I32(4)),
                I::IConst5 => push(Constant::I32(5)),

                I::FConst0 => push(Constant::F32(0.0)),
                I::FConst1 => push(Constant::F32(1.0)),
                I::FConst2 => push(Constant::F32(2.0)),

                I::DConst0 => push(Constant::F64(0.0)),
                I::DConst1 => push(Constant::F64(1.0)),

                I::LConst0 => push(Constant::I64(0)),
                I::LConst1 => push(Constant::I64(1)),

                I::Bipush(b) => push(Constant::I32(b as i32)),
                I::Sipush(s) => push(Constant::I32(s as i32)),

                I::Ldc(b) => push(try_cp_read_idx!(labeler, b as u16, read_constant)?),
                I::LdcW(i) | I::Ldc2W(i) => push(try_cp_read_idx!(labeler, i, read_constant)?),

                I::IALoad => Array(Load, ArrayType::Int),
                I::LALoad => Array(Load, ArrayType::Long),
                I::FALoad => Array(Load, ArrayType::Float),
                I::DALoad => Array(Load, ArrayType::Double),
                I::CALoad => Array(Load, ArrayType::Char),
                I::SALoad => Array(Load, ArrayType::Short),
                I::BALoad => Array(Load, ArrayType::ByteOrBool),
                I::AALoad => Array(Load, ArrayType::Reference),

                I::IAStore => Array(Store, ArrayType::Int),
                I::LAStore => Array(Store, ArrayType::Long),
                I::FAStore => Array(Store, ArrayType::Float),
                I::DAStore => Array(Store, ArrayType::Double),
                I::CAStore => Array(Store, ArrayType::Char),
                I::SAStore => Array(Store, ArrayType::Short),
                I::BAStore => Array(Store, ArrayType::ByteOrBool),
                I::AAStore => Array(Store, ArrayType::Reference),

                I::ALoad0 => LocalVariable(Load, LocalType::Reference, 0),
                I::ALoad1 => LocalVariable(Load, LocalType::Reference, 1),
                I::ALoad2 => LocalVariable(Load, LocalType::Reference, 2),
                I::ALoad3 => LocalVariable(Load, LocalType::Reference, 3),
                I::ALoad(i) => LocalVariable(Load, LocalType::Reference, i as u16),
                I::Wide(Wide::ALoad(i)) => LocalVariable(Load, LocalType::Reference, i),

                I::ILoad0 => LocalVariable(Load, LocalType::Int, 0),
                I::ILoad1 => LocalVariable(Load, LocalType::Int, 1),
                I::ILoad2 => LocalVariable(Load, LocalType::Int, 2),
                I::ILoad3 => LocalVariable(Load, LocalType::Int, 3),
                I::ILoad(i) => LocalVariable(Load, LocalType::Int, i as u16),
                I::Wide(Wide::ILoad(i)) => LocalVariable(Load, LocalType::Int, i),

                I::LLoad0 => LocalVariable(Load, LocalType::Long, 0),
                I::LLoad1 => LocalVariable(Load, LocalType::Long, 1),
                I::LLoad2 => LocalVariable(Load, LocalType::Long, 2),
                I::LLoad3 => LocalVariable(Load, LocalType::Long, 3),
                I::LLoad(i) => LocalVariable(Load, LocalType::Long, i as u16),
                I::Wide(Wide::LLoad(i)) => LocalVariable(Load, LocalType::Long, i),

                I::FLoad0 => LocalVariable(Load, LocalType::Float, 0),
                I::FLoad1 => LocalVariable(Load, LocalType::Float, 1),
                I::FLoad2 => LocalVariable(Load, LocalType::Float, 2),
                I::FLoad3 => LocalVariable(Load, LocalType::Float, 3),
                I::FLoad(i) => LocalVariable(Load, LocalType::Float, i as u16),
                I::Wide(Wide::FLoad(i)) => LocalVariable(Load, LocalType::Float, i),

                I::DLoad0 => LocalVariable(Load, LocalType::Double, 0),
                I::DLoad1 => LocalVariable(Load, LocalType::Double, 1),
                I::DLoad2 => LocalVariable(Load, LocalType::Double, 2),
                I::DLoad3 => LocalVariable(Load, LocalType::Double, 3),
                I::DLoad(i) => LocalVariable(Load, LocalType::Double, i as u16),
                I::Wide(Wide::DLoad(i)) => LocalVariable(Load, LocalType::Double, i),

                I::AStore0 => LocalVariable(Store, LocalType::Reference, 0),
                I::AStore1 => LocalVariable(Store, LocalType::Reference, 1),
                I::AStore2 => LocalVariable(Store, LocalType::Reference, 2),
                I::AStore3 => LocalVariable(Store, LocalType::Reference, 3),
                I::AStore(i) => LocalVariable(Store, LocalType::Reference, i as u16),
                I::Wide(Wide::AStore(i)) => LocalVariable(Store, LocalType::Reference, i),

                I::IStore0 => LocalVariable(Store, LocalType::Int, 0),
                I::IStore1 => LocalVariable(Store, LocalType::Int, 1),
                I::IStore2 => LocalVariable(Store, LocalType::Int, 2),
                I::IStore3 => LocalVariable(Store, LocalType::Int, 3),
                I::IStore(i) => LocalVariable(Store, LocalType::Int, i as u16),
                I::Wide(Wide::IStore(i)) => LocalVariable(Store, LocalType::Int, i),

                I::LStore0 => LocalVariable(Store, LocalType::Long, 0),
                I::LStore1 => LocalVariable(Store, LocalType::Long, 1),
                I::LStore2 => LocalVariable(Store, LocalType::Long, 2),
                I::LStore3 => LocalVariable(Store, LocalType::Long, 3),
                I::LStore(i) => LocalVariable(Store, LocalType::Long, i as u16),
                I::Wide(Wide::LStore(i)) => LocalVariable(Store, LocalType::Long, i),

                I::FStore0 => LocalVariable(Store, LocalType::Float, 0),
                I::FStore1 => LocalVariable(Store, LocalType::Float, 1),
                I::FStore2 => LocalVariable(Store, LocalType::Float, 2),
                I::FStore3 => LocalVariable(Store, LocalType::Float, 3),
                I::FStore(i) => LocalVariable(Store, LocalType::Float, i as u16),
                I::Wide(Wide::FStore(i)) => LocalVariable(Store, LocalType::Float, i),

                I::DStore0 => LocalVariable(Store, LocalType::Double, 0),
                I::DStore1 => LocalVariable(Store, LocalType::Double, 1),
                I::DStore2 => LocalVariable(Store, LocalType::Double, 2),
                I::DStore3 => LocalVariable(Store, LocalType::Double, 3),
                I::DStore(i) => LocalVariable(Store, LocalType::Double, i as u16),
                I::Wide(Wide::DStore(i)) => LocalVariable(Store, LocalType::Double, i),

                I::IAdd => IntOperation(IntType::Int, IOp::Add),
                I::IAnd => IntOperation(IntType::Int, IOp::And),
                I::INeg => IntOperation(IntType::Int, IOp::Negate),
                I::IXor => IntOperation(IntType::Int, IOp::ExclusiveOr),
                I::IOr => IntOperation(IntType::Int, IOp::Or),
                I::ISub => IntOperation(IntType::Int, IOp::Subtract),
                I::IMul => IntOperation(IntType::Int, IOp::Multiply),
                I::IDiv => IntOperation(IntType::Int, IOp::Divide),
                I::IShr => IntOperation(IntType::Int, IOp::ShiftRight),
                I::IShl => IntOperation(IntType::Int, IOp::ShiftLeft),
                I::IUshr => IntOperation(IntType::Int, IOp::UnsignedShiftRight),
                I::IRem => IntOperation(IntType::Int, IOp::Remainder),

                I::LAdd => IntOperation(IntType::Long, IOp::Add),
                I::LAnd => IntOperation(IntType::Long, IOp::And),
                I::LNeg => IntOperation(IntType::Long, IOp::Negate),
                I::LXor => IntOperation(IntType::Long, IOp::ExclusiveOr),
                I::LOr => IntOperation(IntType::Long, IOp::Or),
                I::LSub => IntOperation(IntType::Long, IOp::Subtract),
                I::LMul => IntOperation(IntType::Long, IOp::Multiply),
                I::LDiv => IntOperation(IntType::Long, IOp::Divide),
                I::LShr => IntOperation(IntType::Long, IOp::ShiftRight),
                I::LShl => IntOperation(IntType::Long, IOp::ShiftLeft),
                I::LUshr => IntOperation(IntType::Long, IOp::UnsignedShiftRight),
                I::LRem => IntOperation(IntType::Long, IOp::Remainder),

                I::FAdd => FloatOperation(FloatType::Float, FOp::Add),
                I::FNeg => FloatOperation(FloatType::Float, FOp::Negate),
                I::FSub => FloatOperation(FloatType::Float, FOp::Subtract),
                I::FMul => FloatOperation(FloatType::Float, FOp::Multiply),
                I::FDiv => FloatOperation(FloatType::Float, FOp::Divide),
                I::FRem => FloatOperation(FloatType::Float, FOp::Remainder),

                I::DAdd => FloatOperation(FloatType::Double, FOp::Add),
                I::DNeg => FloatOperation(FloatType::Double, FOp::Negate),
                I::DSub => FloatOperation(FloatType::Double, FOp::Subtract),
                I::DMul => FloatOperation(FloatType::Double, FOp::Multiply),
                I::DDiv => FloatOperation(FloatType::Double, FOp::Divide),
                I::DRem => FloatOperation(FloatType::Double, FOp::Remainder),

                I::I2B => ConvertInt(BitType::Byte),
                I::I2C => ConvertInt(BitType::Char),
                I::I2S => ConvertInt(BitType::Short),
                I::I2F => ConvertInt(BitType::Float),
                I::I2L => ConvertInt(BitType::Long),
                I::I2D => ConvertInt(BitType::Double),

                I::L2I => Conversion(NumberType::Long, NumberType::Int),
                I::L2D => Conversion(NumberType::Long, NumberType::Double),
                I::L2F => Conversion(NumberType::Long, NumberType::Float),

                I::F2I => Conversion(NumberType::Float, NumberType::Int),
                I::F2D => Conversion(NumberType::Float, NumberType::Double),
                I::F2L => Conversion(NumberType::Float, NumberType::Long),

                I::D2I => Conversion(NumberType::Double, NumberType::Int),
                I::D2L => Conversion(NumberType::Double, NumberType::Long),
                I::D2F => Conversion(NumberType::Double, NumberType::Float),

                I::FCmpG => CompareFloats(FloatType::Float, NaNBehavior::ReturnsOne),
                I::FCmpL => CompareFloats(FloatType::Float, NaNBehavior::ReturnsNegativeOne),
                I::DCmpG => CompareFloats(FloatType::Double, NaNBehavior::ReturnsOne),
                I::DCmpL => CompareFloats(FloatType::Double, NaNBehavior::ReturnsNegativeOne),

                I::Goto(off) => Jump(JumpCondition::Always, lbl!(off)),
                I::GotoW(off) => Jump(JumpCondition::Always, lbl!(off)),
                I::IfEq(off) => Jump(JumpCondition::IntegerEqualsZero, lbl!(off)),
                I::IfNe(off) => Jump(JumpCondition::IntegerNotEqualsZero, lbl!(off)),
                I::IfGt(off) => Jump(JumpCondition::IntegerGreaterThanZero, lbl!(off)),
                I::IfGe(off) => Jump(JumpCondition::IntegerGreaterThanOrEqualsZero, lbl!(off)),
                I::IfLt(off) => Jump(JumpCondition::IntegerLessThanZero, lbl!(off)),
                I::IfLe(off) => Jump(JumpCondition::IntegerLessThanOrEqualsZero, lbl!(off)),
                I::IfNull(off) => Jump(JumpCondition::IsNull, lbl!(off)),
                I::IfNonNull(off) => Jump(JumpCondition::IsNonNull, lbl!(off)),
                I::IfACmpEq(off) => Jump(JumpCondition::ReferenceEquals, lbl!(off)),
                I::IfACmpNe(off) => Jump(JumpCondition::ReferenceNotEquals, lbl!(off)),
                I::IfICmpEq(off) => Jump(JumpCondition::IntegerEquals, lbl!(off)),
                I::IfICmpNe(off) => Jump(JumpCondition::IntegerNotEquals, lbl!(off)),
                I::IfICmpGt(off) => Jump(JumpCondition::IntegerGreaterThan, lbl!(off)),
                I::IfICmpGe(off) => Jump(JumpCondition::IntegerGreaterThanOrEquals, lbl!(off)),
                I::IfICmpLt(off) => Jump(JumpCondition::IntegerLessThan, lbl!(off)),
                I::IfICmpLe(off) => Jump(JumpCondition::IntegerLessThanOrEquals, lbl!(off)),

                I::Jsr(off) => Jsr(lbl!(off)),
                I::JsrW(off) => Jsr(lbl!(off)),

                I::Ret(l) => Ret(l as u16),
                I::Wide(Wide::Ret(l)) => Ret(l),

                I::AReturn => Return(Some(LocalType::Reference)),
                I::IReturn => Return(Some(LocalType::Int)),
                I::LReturn => Return(Some(LocalType::Long)),
                I::DReturn => Return(Some(LocalType::Double)),
                I::FReturn => Return(Some(LocalType::Float)),
                I::Return => Return(None),

                I::TableSwitch(dflt, TblS { low, offsets, .. }) => TableSwitch {
                    default: lbl!(dflt),
                    low,
                    offsets: offsets.into_iter().map(|i| lbl!(i)).collect(),
                },
                I::LookupSwitch(dflt, switches) => LookupSwitch {
                    default: lbl!(dflt),
                    table: switches
                        .into_iter()
                        .map(|SwitchEntry(i, to)| (i, lbl!(to)))
                        .collect(),
                },

                I::GetStatic(field) => Field(
                    Get,
                    Static,
                    try_cp_read!(
                        field,
                        labeler.read_or_dynamic(field, ConstantPoolReader::read_member)
                    )?,
                ),
                I::PutStatic(field) => Field(
                    Put,
                    Static,
                    try_cp_read!(
                        field,
                        labeler.read_or_dynamic(field, ConstantPoolReader::read_member)
                    )?,
                ),
                I::GetField(field) => Field(
                    Get,
                    Virtual,
                    try_cp_read!(
                        field,
                        labeler.read_or_dynamic(field, ConstantPoolReader::read_member)
                    )?,
                ),
                I::PutField(field) => Field(
                    Put,
                    Virtual,
                    try_cp_read!(
                        field,
                        labeler.read_or_dynamic(field, ConstantPoolReader::read_member)
                    )?,
                ),

                I::InvokeStatic(m) => InvokeExact(
                    Static,
                    try_cp_read!(
                        m,
                        labeler.read_or_dynamic(m, ConstantPoolReader::read_member)
                    )?,
                ),
                I::InvokeVirtual(m) => InvokeExact(
                    Virtual,
                    try_cp_read!(
                        m,
                        labeler.read_or_dynamic(m, ConstantPoolReader::read_member)
                    )?,
                ),

                I::InvokeSpecial(m) => InvokeSpecial(try_cp_read!(
                    m,
                    labeler.read_or_dynamic(m, ConstantPoolReader::read_member)
                )?),
                I::InvokeInterface(m, c, _) => InvokeInterface(
                    try_cp_read!(
                        m,
                        labeler.read_or_dynamic(m, ConstantPoolReader::read_member)
                    )?,
                    c,
                ),
                I::InvokeDynamic(d, _) => {
                    InvokeDynamic(try_cp_read_idx!(labeler, d, read_invokedynamic)?)
                }

                I::New(n) => New(try_cp_read!(
                    n,
                    labeler.read_or_dynamic(n, ConstantPoolReader::read_class)
                )?),

                I::NewArray(4) => NewArray(OrDynamic::Static(Type::Boolean), 1),
                I::NewArray(5) => NewArray(OrDynamic::Static(Type::Char), 1),
                I::NewArray(6) => NewArray(OrDynamic::Static(Type::Float), 1),
                I::NewArray(7) => NewArray(OrDynamic::Static(Type::Double), 1),
                I::NewArray(8) => NewArray(OrDynamic::Static(Type::Byte), 1),
                I::NewArray(9) => NewArray(OrDynamic::Static(Type::Short), 1),
                I::NewArray(10) => NewArray(OrDynamic::Static(Type::Int), 1),
                I::NewArray(11) => NewArray(OrDynamic::Static(Type::Long), 1),
                I::NewArray(n) => {
                    return Err(Error::Invalid("NewArray type", n.to_string().into()))
                }

                I::ANewArray(r) => NewArray(
                    try_cp_read!(
                        r,
                        labeler.read_or_dynamic(r, ConstantPoolReader::read_class)
                    )?
                    .map_static(|c| c.parse().unwrap_or(Type::Ref(c))),
                    1,
                ),
                I::MultiANewArray(r, dim) => NewArray(
                    try_cp_read!(
                        r,
                        labeler.read_or_dynamic(r, ConstantPoolReader::read_class)
                    )?
                    .map_static(|c| c.parse().unwrap_or(Type::Ref(c))),
                    dim,
                ),
                I::CheckCast(r) => CheckCast(
                    try_cp_read!(
                        r,
                        labeler.read_or_dynamic(r, ConstantPoolReader::read_class)
                    )
                    .and_then(|t| match t {
                        OrDynamic::Static(c) => Ok(OrDynamic::Static(if c.starts_with('[') {
                            if let Type::ArrayRef(dim, ty) = Type::from_str(c.as_ref())? {
                                ClassType::Array(dim, *ty)
                            } else {
                                unsafe { std::hint::unreachable_unchecked() }
                            }
                        } else {
                            ClassType::Object(c)
                        })),
                        OrDynamic::Dynamic(d) => Ok(OrDynamic::Dynamic(d)),
                    })?,
                ),
                I::InstanceOf(r) => InstanceOf(
                    try_cp_read!(
                        r,
                        labeler.read_or_dynamic(r, ConstantPoolReader::read_class)
                    )
                    .and_then(|t| match t {
                        OrDynamic::Static(c) => Ok(OrDynamic::Static(if c.starts_with('[') {
                            if let Type::ArrayRef(dim, ty) = Type::from_str(c.as_ref())? {
                                ClassType::Array(dim, *ty)
                            } else {
                                unsafe { std::hint::unreachable_unchecked() }
                            }
                        } else {
                            ClassType::Object(c)
                        })),
                        OrDynamic::Dynamic(d) => Ok(OrDynamic::Dynamic(d)),
                    })?,
                ),
            };
            instructions.push(insn);
        }
        pos2idx.insert(code_reader.get_ref().len() as u32, instructions.len()); // the last position that is still valid but will not be covered in the loop

        let exceptions = u16::read_from(reader)?;
        let mut catches = Vec::with_capacity(exceptions as usize);
        for _ in 0..exceptions {
            let start = read_from!(&mut labeler, reader)?;
            let end = read_from!(&mut labeler, reader)?;
            let handler = read_from!(&mut labeler, reader)?;
            let ty = {
                let idx = u16::read_from(reader)?;
                if idx == 0 {
                    None
                } else {
                    Some(try_cp_read_idx!(labeler, idx, read_class)?)
                }
            };
            catches.push(Catch {
                start,
                end,
                handler,
                catch: ty,
            });
        }
        labeler.catches = &catches;

        let numattrs = u16::read_from(reader)?;
        let mut attrs = Vec::with_capacity(numattrs as usize);
        let mut to_insert: std::collections::BTreeMap<usize, Vec<Instruction>> =
            std::collections::BTreeMap::new();
        let mut local_vars: HashMap<LocalVarKey, LocalVar> = HashMap::new();
        #[derive(Hash, Eq, PartialEq)]
        struct LocalVarKey(Lbl, Lbl, u16, Cow<'static, str>);
        for _ in 0..numattrs {
            match CodeAttr::read_from(&mut labeler, reader)? {
                CodeAttr::LineNumberTable(ln) => {
                    for self::LineNumber(off, line) in ln {
                        to_insert.insert(pos2idx[&(off as u32)], vec![LineNumber(line)]);
                    }
                }
                CodeAttr::LocalVariableTable(localvar) => {
                    for l in localvar {
                        let start = labeler.get_label(l.start as u32);
                        let end = labeler.get_label((l.start + l.len) as u32);

                        let key = LocalVarKey(start, end, l.index, l.name.clone());
                        if let Some(v) = local_vars.get_mut(&key) {
                            v.descriptor = Some(l.descriptor);
                        } else {
                            local_vars.insert(
                                key,
                                LocalVar {
                                    start,
                                    end,
                                    name: l.name,
                                    descriptor: Some(l.descriptor),
                                    signature: None,
                                    index: l.index,
                                },
                            );
                        }
                    }
                }
                CodeAttr::LocalVariableTypeTable(vartypes) => {
                    for l in vartypes {
                        let start = labeler.get_label(l.start as u32);
                        let end = labeler.get_label((l.start + l.len) as u32);
                        let key = LocalVarKey(start, end, l.index, l.name.clone());
                        if let Some(v) = local_vars.get_mut(&key) {
                            v.signature = Some(l.signature);
                        } else {
                            local_vars.insert(
                                key,
                                LocalVar {
                                    start,
                                    end,
                                    name: l.name,
                                    descriptor: None,
                                    signature: Some(l.signature),
                                    index: l.index,
                                },
                            );
                        }
                    }
                }
                CodeAttr::Raw(r) => attrs.push(CodeAttribute::Raw(r)),
                CodeAttr::StackMapTable(_) => {} // stack map will be regenerated
                CodeAttr::RuntimeInvisibleTypeAnnotations(an) => {
                    attrs.push(CodeAttribute::InvisibleTypeAnnotations(an))
                }
                CodeAttr::RuntimeVisibleTypeAnnotations(an) => {
                    attrs.push(CodeAttribute::VisibleTypeAnnotations(an))
                }
            }
        }
        if !local_vars.is_empty() {
            attrs.push(CodeAttribute::LocalVariables(
                local_vars.into_iter().map(|(_, l)| l).collect(),
            ));
        }
        instructions.reserve(to_insert.len());
        for (k, v) in labeler.labels {
            to_insert.entry(pos2idx[&k]).or_default().push(Label(v));
        }
        for (k, v) in to_insert.into_iter().rev() {
            for i in v {
                instructions.insert(k, i)
            }
        }
        Ok(Code {
            max_stack,
            max_locals,
            code: instructions,
            catches,
            attrs,
        })
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(
        &self,
        cp: &mut C,
        writer: &mut W,
    ) -> crate::Result<(), Error> {
        use crate::constants::insn::*;

        self.max_stack.write_to(writer)?;
        self.max_locals.write_to(writer)?;
        let mut buf: Vec<Vec<u8>> = Vec::new();
        let mut jumps: Vec<&Instruction> = Vec::new();
        let mut cursor: Cursor<Vec<u8>> = Cursor::new(Vec::new());
        let mut line_numbers: HashMap<usize, u16> = HashMap::new();
        let mut labels: HashMap<Label, (usize, usize)> = HashMap::new();
        macro_rules! get_label {
            ($label: expr) => {
                *labels.get($label).ok_or_else(|| {
                    Error::Invalid("referenced label", $label.0.to_string().into())
                })?
            };
        }
        macro_rules! wide_or_normal {
            ($op: expr, $($ext: ident => $ty: ident),+) => ({
                use std::convert::TryFrom;
                #[allow(unused_parens)]
                if let ($(Ok($ext)),*) = ($($ty::try_from(*$ext)),*) {
                    $op.write_to(&mut cursor)?;
                    $(
                        $ext.write_to(&mut cursor)?;
                    )*
                } else {
                    WIDE.write_to(&mut cursor)?;
                    $op.write_to(&mut cursor)?;
                    $(
                        $ext.write_to(&mut cursor)?;
                    )*
                }
            });
        }
        for insn in self.code.iter() {
            match insn {
                Instruction::NoOp => NOP.write_to(&mut cursor)?,
                Instruction::PushNull => ACONST_NULL.write_to(&mut cursor)?,
                Instruction::Push(OrDynamic::Static(Constant::I32(0))) => {
                    ICONST_0.write_to(&mut cursor)?
                }
                Instruction::Push(OrDynamic::Static(Constant::I32(1))) => {
                    ICONST_1.write_to(&mut cursor)?
                }
                Instruction::Push(OrDynamic::Static(Constant::I32(2))) => {
                    ICONST_2.write_to(&mut cursor)?
                }
                Instruction::Push(OrDynamic::Static(Constant::I32(3))) => {
                    ICONST_3.write_to(&mut cursor)?
                }
                Instruction::Push(OrDynamic::Static(Constant::I32(4))) => {
                    ICONST_4.write_to(&mut cursor)?
                }
                Instruction::Push(OrDynamic::Static(Constant::I32(5))) => {
                    ICONST_5.write_to(&mut cursor)?
                }
                Instruction::Push(OrDynamic::Static(Constant::I32(-1))) => {
                    ICONST_M1.write_to(&mut cursor)?
                }
                Instruction::Push(OrDynamic::Static(Constant::I64(0))) => {
                    LCONST_0.write_to(&mut cursor)?
                }
                Instruction::Push(OrDynamic::Static(Constant::I64(1))) => {
                    LCONST_1.write_to(&mut cursor)?
                }
                Instruction::Push(OrDynamic::Static(Constant::I32(i @ -128..=127))) => {
                    BIPUSH.write_to(&mut cursor)?;
                    (*i as i8).write_to(&mut cursor)?;
                }
                Instruction::Push(OrDynamic::Static(Constant::I32(i @ -32768..=32767))) => {
                    SIPUSH.write_to(&mut cursor)?;
                    (*i as i16).write_to(&mut cursor)?;
                }

                Instruction::Push(OrDynamic::Static(Constant::F32(f))) if f.eq(&0.0) => {
                    FCONST_0.write_to(&mut cursor)?
                }
                Instruction::Push(OrDynamic::Static(Constant::F32(f))) if f.eq(&1.0) => {
                    FCONST_1.write_to(&mut cursor)?
                }
                Instruction::Push(OrDynamic::Static(Constant::F32(f))) if f.eq(&2.0) => {
                    FCONST_2.write_to(&mut cursor)?
                }
                Instruction::Push(OrDynamic::Static(Constant::F64(f))) if f.eq(&0.0) => {
                    DCONST_0.write_to(&mut cursor)?
                }
                Instruction::Push(OrDynamic::Static(Constant::F64(f))) if f.eq(&1.0) => {
                    DCONST_1.write_to(&mut cursor)?
                }
                Instruction::Push(c) => {
                    let idx = cp.insert_ordynamic(c.clone(), C::insert_constant);
                    let wide = match c {
                        OrDynamic::Dynamic(d) => d.descriptor.is_wide(),
                        OrDynamic::Static(c) => c.is_wide(),
                    };
                    if wide {
                        LDC2_W.write_to(&mut cursor)?;
                        idx.write_to(&mut cursor)?;
                    } else if let Ok(idx) = u8::try_from(idx) {
                        LDC.write_to(&mut cursor)?;
                        idx.write_to(&mut cursor)?;
                    } else {
                        LDC_W.write_to(&mut cursor)?;
                        idx.write_to(&mut cursor)?;
                    }
                }

                Instruction::Dup => DUP.write_to(&mut cursor)?,
                Instruction::DupX1 => DUP_X1.write_to(&mut cursor)?,
                Instruction::DupX2 => DUP_X2.write_to(&mut cursor)?,
                Instruction::Dup2 => DUP2.write_to(&mut cursor)?,
                Instruction::Dup2X1 => DUP2_X1.write_to(&mut cursor)?,
                Instruction::Dup2X2 => DUP2_X2.write_to(&mut cursor)?,
                Instruction::Pop1 => POP.write_to(&mut cursor)?,
                Instruction::Pop2 => POP2.write_to(&mut cursor)?,
                Instruction::CompareLongs => LCMP.write_to(&mut cursor)?,
                Instruction::CompareFloats(FloatType::Float, NaNBehavior::ReturnsOne) => {
                    FCMPG.write_to(&mut cursor)?
                }
                Instruction::CompareFloats(FloatType::Float, NaNBehavior::ReturnsNegativeOne) => {
                    FCMPL.write_to(&mut cursor)?
                }
                Instruction::CompareFloats(FloatType::Double, NaNBehavior::ReturnsOne) => {
                    DCMPG.write_to(&mut cursor)?
                }
                Instruction::CompareFloats(FloatType::Double, NaNBehavior::ReturnsNegativeOne) => {
                    DCMPL.write_to(&mut cursor)?
                }
                Instruction::LocalVariable(t, ty, l) => {
                    let op = match (t, ty) {
                        (LoadOrStore::Load, LocalType::Int) => ILOAD,
                        (LoadOrStore::Load, LocalType::Float) => FLOAD,
                        (LoadOrStore::Load, LocalType::Double) => DLOAD,
                        (LoadOrStore::Load, LocalType::Long) => LLOAD,
                        (LoadOrStore::Load, LocalType::Reference) => ALOAD,
                        (LoadOrStore::Store, LocalType::Int) => ISTORE,
                        (LoadOrStore::Store, LocalType::Float) => FSTORE,
                        (LoadOrStore::Store, LocalType::Double) => DSTORE,
                        (LoadOrStore::Store, LocalType::Long) => LSTORE,
                        (LoadOrStore::Store, LocalType::Reference) => ASTORE,
                    };
                    wide_or_normal!(op, l => u8);
                }
                Instruction::Array(LoadOrStore::Load, ArrayType::Int) => {
                    IALOAD.write_to(&mut cursor)?
                }
                Instruction::Array(LoadOrStore::Load, ArrayType::ByteOrBool) => {
                    BALOAD.write_to(&mut cursor)?
                }
                Instruction::Array(LoadOrStore::Load, ArrayType::Short) => {
                    SALOAD.write_to(&mut cursor)?
                }
                Instruction::Array(LoadOrStore::Load, ArrayType::Char) => {
                    CALOAD.write_to(&mut cursor)?
                }
                Instruction::Array(LoadOrStore::Load, ArrayType::Float) => {
                    FALOAD.write_to(&mut cursor)?
                }
                Instruction::Array(LoadOrStore::Load, ArrayType::Long) => {
                    LALOAD.write_to(&mut cursor)?
                }
                Instruction::Array(LoadOrStore::Load, ArrayType::Double) => {
                    DALOAD.write_to(&mut cursor)?
                }
                Instruction::Array(LoadOrStore::Load, ArrayType::Reference) => {
                    AALOAD.write_to(&mut cursor)?
                }
                Instruction::Array(LoadOrStore::Store, ArrayType::Int) => {
                    IASTORE.write_to(&mut cursor)?
                }
                Instruction::Array(LoadOrStore::Store, ArrayType::ByteOrBool) => {
                    BASTORE.write_to(&mut cursor)?
                }
                Instruction::Array(LoadOrStore::Store, ArrayType::Short) => {
                    SASTORE.write_to(&mut cursor)?
                }
                Instruction::Array(LoadOrStore::Store, ArrayType::Char) => {
                    CASTORE.write_to(&mut cursor)?
                }
                Instruction::Array(LoadOrStore::Store, ArrayType::Float) => {
                    FASTORE.write_to(&mut cursor)?
                }
                Instruction::Array(LoadOrStore::Store, ArrayType::Long) => {
                    LASTORE.write_to(&mut cursor)?
                }
                Instruction::Array(LoadOrStore::Store, ArrayType::Double) => {
                    DASTORE.write_to(&mut cursor)?
                }
                Instruction::Array(LoadOrStore::Store, ArrayType::Reference) => {
                    AASTORE.write_to(&mut cursor)?
                }

                Instruction::ArrayLength => ARRAYLENGTH.write_to(&mut cursor)?,
                Instruction::IntOperation(IntType::Int, IntOperation::Subtract) => {
                    ISUB.write_to(&mut cursor)?
                }
                Instruction::IntOperation(IntType::Int, IntOperation::Add) => {
                    IADD.write_to(&mut cursor)?
                }
                Instruction::IntOperation(IntType::Int, IntOperation::Multiply) => {
                    IMUL.write_to(&mut cursor)?
                }
                Instruction::IntOperation(IntType::Int, IntOperation::Divide) => {
                    IDIV.write_to(&mut cursor)?
                }
                Instruction::IntOperation(IntType::Int, IntOperation::Remainder) => {
                    IREM.write_to(&mut cursor)?
                }
                Instruction::IntOperation(IntType::Int, IntOperation::Negate) => {
                    INEG.write_to(&mut cursor)?
                }
                Instruction::IntOperation(IntType::Int, IntOperation::ShiftRight) => {
                    ISHR.write_to(&mut cursor)?
                }
                Instruction::IntOperation(IntType::Int, IntOperation::ShiftLeft) => {
                    ISHL.write_to(&mut cursor)?
                }
                Instruction::IntOperation(IntType::Int, IntOperation::UnsignedShiftRight) => {
                    IUSHR.write_to(&mut cursor)?
                }
                Instruction::IntOperation(IntType::Int, IntOperation::Or) => {
                    IOR.write_to(&mut cursor)?
                }
                Instruction::IntOperation(IntType::Int, IntOperation::ExclusiveOr) => {
                    IXOR.write_to(&mut cursor)?
                }
                Instruction::IntOperation(IntType::Int, IntOperation::And) => {
                    IAND.write_to(&mut cursor)?
                }
                Instruction::IntOperation(IntType::Long, IntOperation::Subtract) => {
                    LSUB.write_to(&mut cursor)?
                }
                Instruction::IntOperation(IntType::Long, IntOperation::Add) => {
                    LADD.write_to(&mut cursor)?
                }
                Instruction::IntOperation(IntType::Long, IntOperation::Multiply) => {
                    LMUL.write_to(&mut cursor)?
                }
                Instruction::IntOperation(IntType::Long, IntOperation::Divide) => {
                    LDIV.write_to(&mut cursor)?
                }
                Instruction::IntOperation(IntType::Long, IntOperation::Remainder) => {
                    LREM.write_to(&mut cursor)?
                }
                Instruction::IntOperation(IntType::Long, IntOperation::Negate) => {
                    LNEG.write_to(&mut cursor)?
                }
                Instruction::IntOperation(IntType::Long, IntOperation::ShiftRight) => {
                    LSHR.write_to(&mut cursor)?
                }
                Instruction::IntOperation(IntType::Long, IntOperation::ShiftLeft) => {
                    LSHL.write_to(&mut cursor)?
                }
                Instruction::IntOperation(IntType::Long, IntOperation::UnsignedShiftRight) => {
                    LUSHR.write_to(&mut cursor)?
                }
                Instruction::IntOperation(IntType::Long, IntOperation::Or) => {
                    LOR.write_to(&mut cursor)?
                }
                Instruction::IntOperation(IntType::Long, IntOperation::ExclusiveOr) => {
                    LXOR.write_to(&mut cursor)?
                }
                Instruction::IntOperation(IntType::Long, IntOperation::And) => {
                    LAND.write_to(&mut cursor)?
                }

                Instruction::FloatOperation(FloatType::Float, FloatOperation::Subtract) => {
                    FSUB.write_to(&mut cursor)?
                }
                Instruction::FloatOperation(FloatType::Float, FloatOperation::Negate) => {
                    FNEG.write_to(&mut cursor)?
                }
                Instruction::FloatOperation(FloatType::Float, FloatOperation::Add) => {
                    FADD.write_to(&mut cursor)?
                }
                Instruction::FloatOperation(FloatType::Float, FloatOperation::Multiply) => {
                    FMUL.write_to(&mut cursor)?
                }
                Instruction::FloatOperation(FloatType::Float, FloatOperation::Divide) => {
                    FDIV.write_to(&mut cursor)?
                }
                Instruction::FloatOperation(FloatType::Float, FloatOperation::Remainder) => {
                    FREM.write_to(&mut cursor)?
                }
                Instruction::FloatOperation(FloatType::Double, FloatOperation::Subtract) => {
                    DSUB.write_to(&mut cursor)?
                }
                Instruction::FloatOperation(FloatType::Double, FloatOperation::Negate) => {
                    DNEG.write_to(&mut cursor)?
                }
                Instruction::FloatOperation(FloatType::Double, FloatOperation::Add) => {
                    DADD.write_to(&mut cursor)?
                }
                Instruction::FloatOperation(FloatType::Double, FloatOperation::Multiply) => {
                    DMUL.write_to(&mut cursor)?
                }
                Instruction::FloatOperation(FloatType::Double, FloatOperation::Divide) => {
                    DDIV.write_to(&mut cursor)?
                }
                Instruction::FloatOperation(FloatType::Double, FloatOperation::Remainder) => {
                    DREM.write_to(&mut cursor)?
                }
                Instruction::Throw => ATHROW.write_to(&mut cursor)?,
                Instruction::InstanceOf(ty) => {
                    INSTANCEOF.write_to(&mut cursor)?;
                    cp.insert_ordynamic(ty.clone(), C::insert_class)
                        .write_to(&mut cursor)?;
                }
                Instruction::CheckCast(ty) => {
                    CHECKCAST.write_to(&mut cursor)?;
                    cp.insert_ordynamic(ty.clone(), C::insert_class)
                        .write_to(&mut cursor)?;
                }
                Instruction::New(ty) => {
                    NEW.write_to(&mut cursor)?;
                    cp.insert_ordynamic(ty.clone(), C::insert_class)
                        .write_to(&mut cursor)?;
                }
                Instruction::NewArray(_, _) => {}
                Instruction::Monitor(MonitorOperation::Enter) => {
                    MONITORENTER.write_to(&mut cursor)?
                }
                Instruction::Monitor(MonitorOperation::Exit) => {
                    MONITOREXIT.write_to(&mut cursor)?
                }
                Instruction::Conversion(NumberType::Long, NumberType::Int) => {
                    L2I.write_to(&mut cursor)?
                }
                Instruction::Conversion(NumberType::Long, NumberType::Float) => {
                    L2F.write_to(&mut cursor)?
                }
                Instruction::Conversion(NumberType::Long, NumberType::Double) => {
                    L2D.write_to(&mut cursor)?
                }
                Instruction::Conversion(NumberType::Double, NumberType::Float) => {
                    D2F.write_to(&mut cursor)?
                }
                Instruction::Conversion(NumberType::Double, NumberType::Int) => {
                    D2I.write_to(&mut cursor)?
                }
                Instruction::Conversion(NumberType::Double, NumberType::Long) => {
                    D2L.write_to(&mut cursor)?
                }
                Instruction::Conversion(NumberType::Int, NumberType::Double) => {
                    I2D.write_to(&mut cursor)?
                }
                Instruction::Conversion(NumberType::Int, NumberType::Float) => {
                    I2F.write_to(&mut cursor)?
                }
                Instruction::Conversion(NumberType::Int, NumberType::Long) => {
                    I2L.write_to(&mut cursor)?
                }
                Instruction::Conversion(NumberType::Float, NumberType::Double) => {
                    F2D.write_to(&mut cursor)?
                }
                Instruction::Conversion(NumberType::Float, NumberType::Int) => {
                    F2I.write_to(&mut cursor)?
                }
                Instruction::Conversion(NumberType::Float, NumberType::Long) => {
                    F2L.write_to(&mut cursor)?
                }

                Instruction::Conversion(_, _) => {}
                Instruction::ConvertInt(BitType::Int) => {} // Redundant conversions

                Instruction::ConvertInt(BitType::Long) => I2L.write_to(&mut cursor)?,
                Instruction::ConvertInt(BitType::Short) => I2S.write_to(&mut cursor)?,
                Instruction::ConvertInt(BitType::Byte) => I2B.write_to(&mut cursor)?,
                Instruction::ConvertInt(BitType::Char) => I2C.write_to(&mut cursor)?,
                Instruction::ConvertInt(BitType::Double) => I2D.write_to(&mut cursor)?,
                Instruction::ConvertInt(BitType::Float) => I2F.write_to(&mut cursor)?,
                Instruction::Return(None) => RETURN.write_to(&mut cursor)?,
                Instruction::Return(Some(LocalType::Reference)) => ARETURN.write_to(&mut cursor)?,
                Instruction::Return(Some(LocalType::Long)) => LRETURN.write_to(&mut cursor)?,
                Instruction::Return(Some(LocalType::Int)) => IRETURN.write_to(&mut cursor)?,
                Instruction::Return(Some(LocalType::Double)) => DRETURN.write_to(&mut cursor)?,
                Instruction::Return(Some(LocalType::Float)) => FRETURN.write_to(&mut cursor)?,
                Instruction::Field(op, memty, mem) => {
                    match (op, memty) {
                        (GetOrPut::Get, MemberType::Virtual) => GETFIELD,
                        (GetOrPut::Put, MemberType::Virtual) => PUTFIELD,
                        (GetOrPut::Get, MemberType::Static) => GETSTATIC,
                        (GetOrPut::Put, MemberType::Static) => GETSTATIC,
                    }
                    .write_to(&mut cursor)?;
                    cp.insert_ordynamic(mem.clone(), C::insert_member)
                        .write_to(&mut cursor)?;
                }
                Instruction::InvokeExact(memty, mem) => {
                    match memty {
                        MemberType::Static => INVOKESTATIC,
                        MemberType::Virtual => INVOKEVIRTUAL,
                    }
                    .write_to(&mut cursor)?;
                    cp.insert_ordynamic(mem.clone(), C::insert_member)
                        .write_to(&mut cursor)?;
                }
                Instruction::InvokeSpecial(mem) => {
                    INVOKESPECIAL.write_to(&mut cursor)?;
                    cp.insert_ordynamic(mem.clone(), C::insert_member)
                        .write_to(&mut cursor)?;
                }
                Instruction::InvokeDynamic(dy) => {
                    INVOKEDYNAMIC.write_to(&mut cursor)?;
                    cp.insert_dynamic(dy.clone()).write_to(&mut cursor)?;
                    cursor.write_all(&[0, 0])?;
                }
                Instruction::InvokeInterface(mem, count) => {
                    INVOKEINTERFACE.write_to(&mut cursor)?;
                    cp.insert_ordynamic(mem.clone(), C::insert_member)
                        .write_to(&mut cursor)?;
                    count.write_to(&mut cursor)?;
                    cursor.write_all(&[0])?;
                }

                Instruction::Ret(i) => wide_or_normal!(RET, i => u8),
                Instruction::Swap => SWAP.write_to(&mut cursor)?,
                Instruction::IntIncrement(l, inc) => wide_or_normal!(RET, l => u8, inc => i8),
                Instruction::LineNumber(ln) => {
                    line_numbers.insert(cursor.position() as usize, *ln);
                }
                Instruction::LookupSwitch { .. }
                | Instruction::TableSwitch { .. }
                | Instruction::Jump(_, _)
                | Instruction::Jsr(_) => {
                    buf.push(cursor.into_inner());
                    cursor = Cursor::new(Vec::new());
                    jumps.push(insn);
                }
                Instruction::Label(l) => {
                    labels.insert(*l, (buf.len(), cursor.position() as usize));
                }
            }
        }
        buf.push(cursor.into_inner());
        let mut index_hints = Vec::new();
        let mut last_max_index = 0usize;
        let mut buf_iter = buf.iter();
        // Get minimum/maximum starting index of the next buffer, that is: index_hints[n] is max of buf[n + 1] resulting index.
        for j in &jumps {
            let this_size_max = 1 + match *j {
                Instruction::LookupSwitch { default: _, table } => 11 + table.len() * 8,
                Instruction::TableSwitch {
                    default: _,
                    low: _,
                    offsets,
                } => 15 + offsets.len() * 4, // +3 alignment
                Instruction::Jsr(_) | Instruction::Jump(JumpCondition::Always, _) => 4, // goto_w/jsr_w i32
                Instruction::Jump(_, _) => 7, // conditional jumps can't be wide, so there must be a conversion.
                // SAFETY: other variants are not inserted
                _ => unsafe { std::hint::unreachable_unchecked() },
            };
            last_max_index += this_size_max + buf_iter.next().unwrap().len();
            index_hints.push(last_max_index);
        }
        /*
        determine the actual size by partially resolving the label through index_hints
            Optimize for possible non-wide jumps, and convert conditional jumps to make it wide-compatible:
            such as:
                ifnull far_away
                ops
            gets converted to
                ifnonnull cont
                goto_w far_away
                cont:
                    ops
        */
        let mut actual_indices = Vec::new();
        let mut last_idx = 0;
        buf_iter = buf.iter();
        let mut actual_sizes = Vec::new();
        for j in &jumps {
            last_idx += buf_iter.next().unwrap().len();
            let actual_size = 1 + match *j {
                Instruction::LookupSwitch { default: _, table } => {
                    ((4 - ((last_idx + 2) & 3)) & 3) + 8 + table.len() * 8
                }
                Instruction::TableSwitch {
                    default: _,
                    low: _,
                    offsets,
                } => ((4 - ((last_idx + 2) & 3)) & 3) + 12 + offsets.len() * 4,
                Instruction::Jsr(target) | Instruction::Jump(JumpCondition::Always, target) => {
                    let (buf_idx, buf_off) = get_label!(target);
                    let target_off = if buf_idx != 0 {
                        index_hints[buf_idx - 1]
                    } else {
                        0
                    } + buf_off;
                    if target_off <= 65535 {
                        2
                    } else {
                        4
                    }
                }
                Instruction::Jump(_, target) => {
                    let (buf_idx, buf_off) = get_label!(target);
                    let target_off = if buf_idx != 0 {
                        index_hints[buf_idx - 1]
                    } else {
                        0
                    } + buf_off;
                    if target_off <= 65535 {
                        2
                    } else {
                        7
                    }
                }
                // SAFETY: other variants are not inserted
                _ => unsafe { std::hint::unreachable_unchecked() },
            };
            last_idx += actual_size;
            actual_sizes.push(actual_size);
            actual_indices.push(last_idx);
        }
        let code_len = (buf_iter.next().unwrap().len() + last_idx) as u32;
        code_len.write_to(writer)?;
        let mut jumps_iter = jumps.into_iter();
        let mut buf_iter = buf.into_iter();
        writer.write_all(&buf_iter.next().unwrap())?;
        for ((i, bytes), (idx, act)) in buf_iter
            .enumerate()
            .zip(actual_indices.iter().zip(actual_sizes.iter()))
        {
            macro_rules! resolve_label {
                ($label: expr) => {{
                    let (buf_off, inner_off) = get_label!($label);
                    let that_off = (if buf_off == 0 {
                        0
                    } else {
                        actual_indices[buf_off - 1] as u32
                    }) + (inner_off as u32);
                    (that_off as i32).wrapping_sub(*idx as i32) + *act as i32
                }};
            }

            macro_rules! wide {
                ($label: ident, $off: ident => $non_wide: expr, $wide: expr) => {{
                    let $off = resolve_label!($label);
                    if let Ok($off) = u16::try_from($off) {
                        $non_wide
                    } else {
                        $wide
                    }
                }};
            }
            let jump = jumps_iter.next().unwrap();
            match jump {
                Instruction::LookupSwitch { default, table } => {
                    LOOKUPSWITCH.write_to(writer)?;
                    writer.write_all(&vec![0; (4 - (actual_indices[i] & 3)) & 3])?; // proper 4 byte alignment
                    write_to!(&resolve_label!(default), writer)?;

                    (table.len() as u32).write_to(writer)?;
                    let mut tbl = table.clone();
                    tbl.sort_keys(); // lookup switch must be sorted
                    for (val, off) in tbl {
                        write_to!(&val, writer)?;
                        write_to!(&resolve_label!(&off), writer)?;
                    }
                }
                Instruction::TableSwitch {
                    default,
                    low,
                    offsets,
                } => {
                    TABLESWITCH.write_to(writer)?;
                    writer.write_all(&vec![0; (4 - (actual_indices[i] & 3)) & 3])?; // proper 4 byte alignment
                    write_to!(&resolve_label!(default), writer)?;
                    write_to!(low, writer)?;
                    write_to!(&(low + (offsets.len() - 1) as i32), writer)?;
                    for l in offsets {
                        write_to!(&resolve_label!(l), writer)?;
                    }
                }
                Instruction::Jsr(target) => {
                    wide!(target, off => {
                        JSR.write_to(writer)?;
                        write_to!(&off, writer)?;
                    }, {
                        JSR_W.write_to(writer)?;
                        write_to!(&off, writer)?;
                    })
                }
                Instruction::Jump(JumpCondition::Always, target) => {
                    wide!(target, off => {
                        GOTO.write_to(writer)?;
                        write_to!(&off, writer)?;
                    }, {
                        GOTO_W.write_to(writer)?;
                        write_to!(&off, writer)?;
                    })
                }
                Instruction::Jump(cond, target) => {
                    wide!(target, off => {
                        u8::write_to(&(*cond).into(), writer)?;
                        write_to!(&off, writer)?;
                    }, {
                        u8::write_to(&(-cond).unwrap_or_else(|| unsafe { std::hint::unreachable_unchecked() }).into(), writer)?;
                        write_to!(&5i32, writer)?;
                        GOTO_W.write_to(writer)?;
                        write_to!(&off, writer)?;
                    })
                }
                // SAFETY: other variants are not inserted
                _ => unsafe { std::hint::unreachable_unchecked() },
            }
            writer.write_all(&bytes)?;
        }

        struct Labeler<'a, T: ConstantPoolWriter>(
            &'a Vec<usize>,
            &'a HashMap<Label, (usize, usize)>,
            &'a mut T,
            &'a Vec<Catch>,
        );

        impl<'a, T: ConstantPoolWriter> ConstantPoolWriter for Labeler<'a, T> {
            #[inline]
            fn insert_raw(&mut self, value: RawConstantEntry) -> u16 {
                self.2.insert_raw(value)
            }

            #[inline]
            fn insert_bsm(&mut self, bsm: BootstrapMethod) -> u16 {
                self.2.insert_bsm(bsm)
            }

            fn label(&mut self, lbl: &Label) -> u16 {
                let (buf_off, inner_off) = *self.1.get(lbl).unwrap();
                (if buf_off == 0 {
                    0
                } else {
                    self.0[buf_off - 1] as u16
                }) + (inner_off as u16)
            }

            fn catch(&mut self, catch: &Catch) -> Option<u16> {
                self.3.iter().position(|c| c == catch).map(|n| n as u16)
            }
        }

        (self.catches.len() as u16).write_to(writer)?;
        let mut labeler = Labeler(&actual_indices, &labels, cp, &self.catches);
        for Catch {
            start,
            end,
            handler,
            catch,
        } in &self.catches
        {
            labeler.label(start).write_to(writer)?;
            labeler.label(end).write_to(writer)?;
            labeler.label(handler).write_to(writer)?;
            if let Some(s) = catch {
                labeler.insert_class(s.clone())
            } else {
                0
            }
            .write_to(writer)?;
        }
        (self.attrs.len() as u16).write_to(writer)?;
        for a in &self.attrs {
            match a {
                CodeAttribute::VisibleTypeAnnotations(a) => {
                    CodeAttr::RuntimeVisibleTypeAnnotations(a.clone())
                }
                CodeAttribute::InvisibleTypeAnnotations(a) => {
                    CodeAttr::RuntimeInvisibleTypeAnnotations(a.clone())
                }
                CodeAttribute::LocalVariables(l) => {
                    let mut ty: Vec<LocalVarType> = vec![];
                    let mut var: Vec<LocalVar> = vec![];
                    for lc in l {
                        if let Some(ref desc) = lc.descriptor {
                            let start = labeler.label(&lc.start);
                            let len = labeler.label(&lc.end) - start;
                            var.push(LocalVar {
                                start,
                                len,
                                name: lc.name.clone(),
                                descriptor: desc.clone(),
                                index: lc.index,
                            })
                        }
                        if let Some(ref sig) = lc.signature {
                            let start = labeler.label(&lc.start);
                            let len = labeler.label(&lc.end) - start;
                            ty.push(LocalVarType {
                                start,
                                len,
                                name: lc.name.clone(),
                                signature: sig.clone(),
                                index: lc.index,
                            })
                        }
                    }
                    match (ty.is_empty(), var.is_empty()) {
                        (true, true) => {
                            return Err(Error::Invalid(
                                "local variables",
                                "no localvariable type or descriptor present".into(),
                            ))
                        }
                        (false, true) => CodeAttr::LocalVariableTypeTable(ty),
                        (true, false) => CodeAttr::LocalVariableTable(var),
                        (false, false) => {
                            CodeAttr::LocalVariableTable(var).write_to(&mut labeler, writer)?;
                            CodeAttr::LocalVariableTypeTable(ty)
                        }
                    }
                }
                CodeAttribute::Raw(r) => CodeAttr::Raw(r.clone()),
            }
            .write_to(&mut labeler, writer)?;
        }
        Ok(())
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, ConstantPoolReadWrite)]
#[tag_type(u8)]
pub enum VerificationType {
    Top,
    Int,
    Float,
    Long,
    Double,
    Null,
    UninitializedThis,
    Object(#[str_type(Class)] Cow<'static, str>),
    /// Following the label, must be a `NEW` instruction.
    UninitializedVariable(Label),
}

impl VerificationType {
    pub const fn is_wide(&self) -> bool {
        matches!(self, VerificationType::Double | VerificationType::Long)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum RawFrame {
    Same(u16),
    SameLocalsOneStack(u16, VerificationType),
    /// Chop up to three.
    Chop(u16, u8),
    /// At most three items.
    Append(u16, Vec<VerificationType>),
    /// Locals and then stack values.
    Full(u16, Vec<VerificationType>, Vec<VerificationType>),
}

impl ConstantPoolReadWrite for RawFrame {
    fn read_from<C: ConstantPoolReader, R: Read>(
        cp: &mut C,
        reader: &mut R,
    ) -> crate::Result<Self> {
        let tag = u8::read_from(reader)?;
        Ok(match tag {
            0..=63 => RawFrame::Same(tag as u16),
            64..=127 => RawFrame::SameLocalsOneStack(
                (tag - 64) as u16,
                VerificationType::read_from(cp, reader)?,
            ),
            128..=246 => {
                return Err(Error::Invalid(
                    "tag (is reserved for future use)",
                    tag.to_string().into(),
                ))
            }
            247 => RawFrame::SameLocalsOneStack(
                u16::read_from(reader)?,
                VerificationType::read_from(cp, reader)?,
            ),
            248..=250 => RawFrame::Chop(u16::read_from(reader)?, 251 - tag),
            251 => RawFrame::Same(u16::read_from(reader)?),
            252..=254 => RawFrame::Append(u16::read_from(reader)?, {
                let mut vec = Vec::with_capacity((tag - 251) as usize);
                for _ in 251..tag {
                    vec.push(VerificationType::read_from(cp, reader)?)
                }
                vec
            }),
            _ => RawFrame::Full(
                u16::read_from(reader)?,
                {
                    let locals = u16::read_from(reader)?;
                    let mut local = Vec::with_capacity(locals as usize);
                    for _ in 0..locals {
                        local.push(VerificationType::read_from(cp, reader)?);
                    }
                    local
                },
                {
                    let stacks = u16::read_from(reader)?;
                    let mut stack = Vec::with_capacity(stacks as usize);
                    for _ in 0..stacks {
                        stack.push(VerificationType::read_from(cp, reader)?);
                    }
                    stack
                },
            ),
        })
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(
        &self,
        cp: &mut C,
        writer: &mut W,
    ) -> crate::Result<()> {
        match self {
            RawFrame::Same(off @ 0..=63) => (*off as u8).write_to(writer)?,
            RawFrame::Same(off) => {
                251u8.write_to(writer)?;
                off.write_to(writer)?;
            }
            RawFrame::SameLocalsOneStack(off @ 0..=63, veri) => {
                (*off as u8 + 64).write_to(writer)?;
                veri.write_to(cp, writer)?;
            }
            RawFrame::SameLocalsOneStack(off, veri) => {
                247u8.write_to(writer)?;
                off.write_to(writer)?;
                veri.write_to(cp, writer)?;
            }
            RawFrame::Chop(off, chop @ 1..=3) => {
                (251 - *chop as u8).write_to(writer)?;
                off.write_to(writer)?;
            }
            RawFrame::Chop(_, c) => return Err(Error::Invalid("Chop value", c.to_string().into())),
            RawFrame::Append(off, locals) if locals.len() <= 3 => {
                (locals.len() as u8 + 251).write_to(writer)?;
                off.write_to(writer)?;
                for local in locals {
                    local.write_to(cp, writer)?;
                }
            }
            RawFrame::Append(_, _) => {
                return Err(Error::Invalid("Append", "locals length > 3".into()))
            }
            RawFrame::Full(off, locals, stack) => {
                255u8.write_to(writer)?;
                off.write_to(writer)?;
                (locals.len() as u16).write_to(writer)?;
                for local in locals {
                    local.write_to(cp, writer)?;
                }
                (stack.len() as u16).write_to(writer)?;
                for s in stack {
                    s.write_to(cp, writer)?;
                }
            }
        }
        Ok(())
    }
}
