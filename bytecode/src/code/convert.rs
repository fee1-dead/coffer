use std::str::FromStr;

use super::{Instruction, Instruction::*};
use crate::{
    insn::{Instruction as I, SwitchEntry, TableSwitch as TblS, Wide},
    prelude::{
        FloatOperation as FOp, GetOrPut::*, IntOperation as IOp, LoadOrStore::*, MemberType::*, *,
    },
    try_cp_read_idx,
};

pub struct Conv;

impl Conv {
    pub(crate) fn convert_direct_instruction<T: ConstantPoolReader>(
        insn: I,
        labeler: &mut T,
        curpos: i64
    ) -> Result<Instruction> {
        macro_rules! lbl {
            ($off:expr) => {{
                labeler.get_label((curpos + $off as i64) as u32)
            }};
        }
        #[inline]
        fn push<C: Into<OrDynamic<Constant>>>(c: C) -> Instruction {
            Push(c.into())
        }
        Ok(match insn {
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
            I::NewArray(n) => return Err(Error::Invalid("NewArray type", n.to_string().into())),

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
                            // SAFETY: Must be array because string starts with '['.
                            unsafe { std::hint::unreachable_unchecked() }
                        }
                    } else {
                        ClassType::Object(c)
                    })),
                    OrDynamic::Dynamic(d) => Ok(OrDynamic::Dynamic(d)),
                })?,
            ),
        })
    }
}
