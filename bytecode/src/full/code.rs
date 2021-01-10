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

use std::io::{Read, Write};

use indexmap::map::IndexMap;
use nom::lib::std::borrow::Cow;

use crate::{ConstantPoolReader, ConstantPoolReadWrite, ConstantPoolWriter, Error, ReadWrite, try_cp_read, try_cp_read_idx, read_from};
use crate::access::AccessFlags;
use crate::full::{BootstrapMethod, Constant, FieldSignature, MemberRef, RawAttribute, To, Type, VerificationType};
use crate::full::annotation::{CodeTypeAnnotation};
use std::str::FromStr;

/// Acts as a unique identifier to the code. Labels should be treated carefully because when labels become invalid (i.e. removed from the code array) it will become an error.
#[derive(Debug, Eq, PartialOrd, PartialEq, Ord, Hash, Copy, Clone)]
pub struct Label(pub u32);

impl ConstantPoolReadWrite for Label {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> crate::Result<Self> {
        Ok(cp.get_label(u16::read_from(reader)? as _))
    }
    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> crate::Result<()> {
        ReadWrite::write_to(&(cp.label(*self) as u16), writer)
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
    Byte, Bool, Short, Char, Int, Long, Float, Double, Reference
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

impl<T> OrDynamic<T> {
    fn map<F, N>(self, f: F) -> OrDynamic<N> where F: FnOnce(T) -> N {
        match self {
            Self::Dynamic(d) => OrDynamic::Dynamic(d),
            Self::Static(t) => OrDynamic::Static(f(t))
        }
    }
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ClassType {
    Object(Cow<'static, str>),
    Array(u8, Type)
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
    /// Duplicate one or two stack values and insert them zero or more values down.
    ///
    /// `Duplicate(One, None)` is equivalent to `DUP`
    ///
    /// `Duplicate(Two, Some(Two))` is equivalent to `DUP2_X2`
    Duplicate(StackValueType, Option<StackValueType>),
    /// Pop one or two values. Use `Pop(Two)` for double/long values.
    Pop(StackValueType),
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
    InvokeInterface(OrDynamic<MemberRef>),
    Jsr(Label),
    Ret(u16),
    Swap,
    LocalInt(u16, i16),
    LineNumber(u16),
    TableSwitch {
        default: Label,
        low: i32,
        offsets: Vec<Label>
    },
    LookupSwitch {
        default: Label,
        table: IndexMap<i32, Label>,
    },
    /// Not real in bytecode, used as a marker of location.
    Label(Label)
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct LocalVariable {
    pub start: Label,
    pub end: Label,
    pub name: Cow<'static, str>,
    pub descriptor: Option<Type>,
    pub signature: Option<FieldSignature>,
    pub index: u16
}

#[derive(Clone, Copy, Eq, PartialEq, ReadWrite, Debug)]
pub struct LineNumber(u16, u16);

#[derive(Clone, Eq, PartialEq, Debug, ConstantPoolReadWrite)]
struct LocalVar {
    #[use_normal_rw]
    pub start: u16,
    #[use_normal_rw]
    pub len: u16,
    pub name: Cow<'static, str>,
    pub descriptor: Type,
    #[use_normal_rw]
    pub index: u16
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
    pub index: u16
}

#[derive(Clone, PartialEq, Debug, ConstantPoolReadWrite)]
#[attr_enum]
enum CodeAttr {
    LineNumberTable(#[vec_len_type(u16)] #[use_normal_rw] Vec<LineNumber>),
    LocalVariableTable(#[vec_len_type(u16)] Vec<LocalVar>),
    LocalVariableTypeTable(#[vec_len_type(u16)] Vec<LocalVarType>),
    RuntimeInvisibleTypeAnnotations(#[vec_len_type(u16)] Vec<CodeTypeAnnotation>),
    RuntimeVisibleTypeAnnotations(#[vec_len_type(u16)] Vec<CodeTypeAnnotation>),
    StackMapTable(#[vec_len_type(u16)] Vec<RawFrame>),
    #[raw_variant]
    Raw(RawAttribute)
}

#[derive(Clone, PartialEq, Debug)]
pub enum CodeAttribute {
    VisibleTypeAnnotations(Vec<CodeTypeAnnotation>),
    InvisibleTypeAnnotations(Vec<CodeTypeAnnotation>),
    LocalVariables(Vec<LocalVariable>),
    Raw(RawAttribute)
}

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub struct Catch {
    pub start: Label,
    pub end: Label,
    pub handler: Label,
    pub catch: Option<Cow<'static, str>>
}

impl ConstantPoolReadWrite for Catch {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> crate::Result<Self, Error> {
        try_cp_read!(cp, reader, get_catch)
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> crate::Result<(), Error> {
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

impl ConstantPoolReadWrite for Code {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> crate::Result<Self, Error> {
        use crate::insn::{Instruction as I, Wide, SwitchEntry, TableSwitch as TblS};
        use crate::full::Instruction::*;
        use crate::full::LoadOrStore::*;
        use crate::full::GetOrPut::*;
        use crate::full::StackValueType::*;
        use crate::full::MemberType::*;
        use crate::full::{IntOperation as IOp, FloatOperation as FOp};
        use std::collections::HashMap;
        use crate::full::cp::RawConstantEntry;
        use std::io::{Cursor, SeekFrom, Seek};

        struct Labeler<'a, T: ConstantPoolReader> {
            inner: &'a mut T,
            labels: HashMap<u32, crate::full::Label>,
            catches: &'a [Catch]
        }
        impl<'a, T: ConstantPoolReader> ConstantPoolReader for Labeler<'a, T> {
            fn read_raw(&mut self, idx: u16) -> Option<RawConstantEntry> {
                self.inner.read_raw(idx)
            }

            fn resolve_later(&mut self, bsm_idx: u16, ptr: &mut Box<BootstrapMethod>) {
                self.inner.resolve_later(bsm_idx, ptr)
            }

            fn get_label(&mut self, idx: u32) -> crate::full::Label {
                if let Some(v) = self.labels.get(&idx) {
                    *v
                } else {
                    let l = crate::full::Label(self.labels.len() as u32);
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
        let mut labeler = Labeler { inner: cp, labels: HashMap::new(), catches: &[] };
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
                    code_reader.seek(SeekFrom::Current((4 - ((curpos % 4) & 3)) as i64))?;
                    let op = [opcode];
                    let mut temp_read = (&op).chain(&mut code_reader);
                    crate::insn::Instruction::read_from(&mut temp_read)?
                }
                _ => {
                    crate::insn::Instruction::read_from(&mut code_reader)?
                }
            };
            macro_rules! lbl {
                ($off:expr) => ({labeler.get_label((curpos as i64 + $off as i64) as u32)});
            }
            #[inline]
            fn push<C: Into<OrDynamic<Constant>>>(c: C) -> Instruction { Push(c.into()) }
            let insn = match insn {
                I::AThrow => Throw,
                I::Nop => NoOp,
                I::AConstNull => PushNull,
                I::Swap => Swap,
                I::LCmp => CompareLongs,
                I::ArrayLength => ArrayLength,
                I::MonitorEnter => Monitor(MonitorOperation::Enter),
                I::MonitorExit => Monitor(MonitorOperation::Exit),

                I::IInc(idx, val) => LocalInt(idx as u16, val as i16),
                I::Wide(Wide::IInc(idx, val))  => LocalInt(idx, val),

                I::Pop => Pop(One),
                I::Pop2 => Pop(Two),

                I::Dup => Duplicate(One, None),
                I::Dup2 => Duplicate(Two, None),
                I::Dupx1 => Duplicate(One, Some(One)),
                I::Dupx2 => Duplicate(One, Some(Two)),
                I::Dup2x1 => Duplicate(Two, Some(One)),
                I::Dup2x2 => Duplicate(Two, Some(Two)),

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

                I::LDC(b) => push(try_cp_read_idx!(labeler, b as u16, read_constant)?),
                I::LDCW(i) | I::LDC2W(i) => push(try_cp_read_idx!(labeler, i, read_constant)?),

                I::IALoad => Array(Load, ArrayType::Int),
                I::LALoad => Array(Load, ArrayType::Long),
                I::FALoad => Array(Load, ArrayType::Float),
                I::DALoad => Array(Load, ArrayType::Double),
                I::CALoad => Array(Load, ArrayType::Char),
                I::SALoad => Array(Load, ArrayType::Short),
                I::BALoad => Array(Load, ArrayType::Byte),
                I::AALoad => Array(Load, ArrayType::Reference),

                I::IAStore => Array(Store, ArrayType::Int),
                I::LAStore => Array(Store, ArrayType::Long),
                I::FAStore => Array(Store, ArrayType::Float),
                I::DAStore => Array(Store, ArrayType::Double),
                I::CAStore => Array(Store, ArrayType::Char),
                I::SAStore => Array(Store, ArrayType::Short),
                I::BAStore => Array(Store, ArrayType::Byte),
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

                I::TableSwitch(dflt, TblS { low, offsets, .. }) => TableSwitch { default: lbl!(dflt), low, offsets: offsets.into_iter().map(|i| lbl!(i)).collect() },
                I::LookupSwitch(dflt, switches) => LookupSwitch { default: lbl!(dflt), table: switches.into_iter().map(|SwitchEntry(i,to)| (i, lbl!(to))).collect() },

                I::GetStatic(field) => Field(Get, Static, try_cp_read!(field, labeler.read_or_dynamic(field, ConstantPoolReader::read_member))?),
                I::PutStatic(field) => Field(Put, Static, try_cp_read!(field, labeler.read_or_dynamic(field, ConstantPoolReader::read_member))?),
                I::GetField(field) => Field(Get, Virtual, try_cp_read!(field, labeler.read_or_dynamic(field, ConstantPoolReader::read_member))?),
                I::PutField(field) => Field(Put, Virtual, try_cp_read!(field, labeler.read_or_dynamic(field, ConstantPoolReader::read_member))?),

                I::InvokeStatic(m) => InvokeExact(Static, try_cp_read!(m, labeler.read_or_dynamic(m, ConstantPoolReader::read_member))?),
                I::InvokeVirtual(m) => InvokeExact(Virtual, try_cp_read!(m, labeler.read_or_dynamic(m, ConstantPoolReader::read_member))?),

                I::InvokeSpecial(m) => InvokeSpecial(try_cp_read!(m, labeler.read_or_dynamic(m, ConstantPoolReader::read_member))?),
                I::InvokeInterface(m, _, _) => InvokeInterface(try_cp_read!(m, labeler.read_or_dynamic(m, ConstantPoolReader::read_member))?),
                I::InvokeDynamic(d, _) => InvokeDynamic(try_cp_read_idx!(labeler, d, read_invokedynamic)?),

                I::New(n) => New(try_cp_read!(n, labeler.read_or_dynamic(n, ConstantPoolReader::read_class))?),

                I::NewArray(4) => NewArray(OrDynamic::Static(Type::Boolean), 1),
                I::NewArray(5) => NewArray(OrDynamic::Static(Type::Char), 1),
                I::NewArray(6) => NewArray(OrDynamic::Static(Type::Float), 1),
                I::NewArray(7) => NewArray(OrDynamic::Static(Type::Double), 1),
                I::NewArray(8) => NewArray(OrDynamic::Static(Type::Byte), 1),
                I::NewArray(9) => NewArray(OrDynamic::Static(Type::Short), 1),
                I::NewArray(10) => NewArray(OrDynamic::Static(Type::Int), 1),
                I::NewArray(11) => NewArray(OrDynamic::Static(Type::Long), 1),
                I::NewArray(n) => return Err(Error::Invalid("NewArray type", n.to_string().into())),

                I::ANewArray(r) => NewArray(try_cp_read!(r, labeler.read_or_dynamic(r, ConstantPoolReader::read_class))?.map(|c| c.parse().unwrap_or(Type::Ref(c))), 1),
                I::MultiANewArray(r, dim) => NewArray(try_cp_read!(r, labeler.read_or_dynamic(r, ConstantPoolReader::read_class))?.map(|c| c.parse().unwrap_or(Type::Ref(c))), dim),
                I::CheckCast(r) => CheckCast(try_cp_read!(r, labeler.read_or_dynamic(r, ConstantPoolReader::read_class)).and_then(|t|
                    match t {
                        OrDynamic::Static(c) => Ok(OrDynamic::Static(
                            if c.starts_with('[') {
                                if let Type::ArrayRef(dim, ty) = Type::from_str(c.as_ref())? { ClassType::Array(dim, *ty) } else { unsafe { std::hint::unreachable_unchecked() } }
                            } else { ClassType::Object(c) }
                        )),
                        OrDynamic::Dynamic(d) => Ok(OrDynamic::Dynamic(d))
                    }
                )?),
                I::InstanceOf(r) => InstanceOf(try_cp_read!(r, labeler.read_or_dynamic(r, ConstantPoolReader::read_class)).and_then(|t|
                    match t {
                        OrDynamic::Static(c) => Ok(OrDynamic::Static(
                            if c.starts_with('[') {
                                if let Type::ArrayRef(dim, ty) = Type::from_str(c.as_ref())? { ClassType::Array(dim, *ty) } else { unsafe { std::hint::unreachable_unchecked() } }
                            } else { ClassType::Object(c) }
                        )),
                        OrDynamic::Dynamic(d) => Ok(OrDynamic::Dynamic(d))
                    }
                )?),
            };
            instructions.push(insn);
        }

        let exceptions = u16::read_from(reader)?;
        let mut catches = Vec::with_capacity(exceptions as usize);
        for _ in 0..exceptions {
            let start = read_from!(&mut labeler, reader)?;
            let end = read_from!(&mut labeler, reader)?;
            let handler = read_from!(&mut labeler, reader)?;
            let ty = { let idx = u16::read_from(reader)?; if idx == 0 { None } else { Some(try_cp_read_idx!(labeler, idx, read_utf8)?) } };
            catches.push(Catch {
                start, end, handler, catch: ty
            });
        }
        labeler.catches = &catches;

        let numattrs = u16::read_from(reader)?;
        let mut attrs = Vec::with_capacity(numattrs as usize);
        let mut to_insert: std::collections::BTreeMap<usize, Vec<Instruction>> = std::collections::BTreeMap::new();
        let mut local_vars: HashMap<LocalVarKey, crate::full::LocalVariable> = HashMap::new();
        #[derive(Hash, Eq, PartialEq)]
        struct LocalVarKey(crate::full::Label, crate::full::Label, u16, Cow<'static, str>);
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

                        let key = LocalVarKey ( start, end, l.index, l.name.clone() );
                        if let Some(v) = local_vars.get_mut(&key) {
                            v.descriptor = Some(l.descriptor);
                        } else {
                            local_vars.insert(key, crate::full::LocalVariable {
                                start,
                                end,
                                name: l.name,
                                descriptor: Some(l.descriptor),
                                signature: None,
                                index: l.index
                            });
                        }
                    }
                }
                CodeAttr::LocalVariableTypeTable(vartypes) => {
                    for l in vartypes {
                        let start = labeler.get_label(l.start as u32);
                        let end = labeler.get_label((l.start + l.len) as u32);
                        let key = LocalVarKey ( start, end, l.index, l.name.clone() );
                        if let Some(v) = local_vars.get_mut(&key) {
                            v.signature = Some(l.signature);
                        } else {
                            local_vars.insert(key, crate::full::LocalVariable {
                                start,
                                end,
                                name: l.name,
                                descriptor: None,
                                signature: Some(l.signature),
                                index: l.index
                            });
                        }
                    }
                }
                CodeAttr::Raw(r) => attrs.push(CodeAttribute::Raw(r)),
                CodeAttr::StackMapTable(_) => {}
                CodeAttr::RuntimeInvisibleTypeAnnotations(an) => attrs.push(CodeAttribute::InvisibleTypeAnnotations(an)),
                CodeAttr::RuntimeVisibleTypeAnnotations(an) => attrs.push(CodeAttribute::VisibleTypeAnnotations(an))
            }
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
            max_stack, max_locals, code: instructions, catches, attrs
        })
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> crate::Result<(), Error> {
        unimplemented!()
    }
}


#[derive(Debug, Eq, PartialEq, Clone)]
pub enum RawFrame {
    Same(u16), SameLocalsOneStack(u16, VerificationType),
    /// Chop up to three.
    Chop(u16, u8),
    /// At most three items.
    Append(u16, Vec<VerificationType>),
    /// Locals and then stack values.
    Full(u16, Vec<VerificationType>, Vec<VerificationType>)
}

impl ConstantPoolReadWrite for RawFrame {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> crate::Result<Self> {
        let tag = u8::read_from(reader)?;
        Ok(match tag {
            0..=63 => RawFrame::Same(tag as u16),
            64..=127 => RawFrame::SameLocalsOneStack((tag - 64) as u16, VerificationType::read_from(cp, reader)?),
            128..=246 => return Err(Error::Invalid("tag (is reserved for future use)", tag.to_string().into())),
            247 => RawFrame::SameLocalsOneStack(u16::read_from(reader)?, VerificationType::read_from(cp, reader)?),
            248..=250 => RawFrame::Chop(u16::read_from(reader)?, 251 - tag),
            251 => RawFrame::Same(u16::read_from(reader)?),
            252..=254 => RawFrame::Append(u16::read_from(reader)?, {
                let mut vec = Vec::with_capacity((tag - 251) as usize);
                let mut i = 251;
                while i < tag {
                    let veri = VerificationType::read_from(cp, reader)?;
                    i += if let VerificationType::Double | VerificationType::Long = veri {
                        2
                    } else {
                        1
                    };
                    vec.push(veri);
                }
                vec }),
            _ => RawFrame::Full(u16::read_from(reader)?, {
                let locals = u16::read_from(reader)?;
                let mut local = Vec::with_capacity((tag - 251) as usize);
                let mut i = 0;
                while i < locals {
                    let veri = VerificationType::read_from(cp, reader)?;
                    i += if let VerificationType::Double | VerificationType::Long = veri {
                        2
                    } else {
                        1
                    };
                    local.push(veri);
                }
                local
            }, {
                let stacks = u16::read_from(reader)?;
                let mut stack = Vec::with_capacity((tag - 251) as usize);
                let mut i = 0;
                while i < stacks {
                    let veri = VerificationType::read_from(cp, reader)?;
                    i += if let VerificationType::Double | VerificationType::Long = veri {
                        2
                    } else {
                        1
                    };
                    stack.push(veri);
                }
                stack
            })
        })
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> crate::Result<()> {
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
            RawFrame::Append(_, _) => return Err(Error::Invalid("Append", "locals length > 3".into())),
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

