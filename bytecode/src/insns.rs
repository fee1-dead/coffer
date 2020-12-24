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
use std::sync::Arc;
use std::rc::Rc;
use std::collections::{VecDeque, LinkedList, BTreeMap};
use std::ptr::NonNull;
use std::marker::PhantomData;
use std::mem;

pub type ClassRef = Box<String>;

#[derive(Clone, Eq, PartialEq)]
pub enum Constant {
    I32(i32),
    F32(f32),
    I64(i64),
    F64(f64),
    String(Box<String>),
    Class(Box<String>),
    MethodType(Box<String>),
    Null
}
pub enum StackValueType {
    /// Represents A stack value of computational type one. This should not be used when the stack type is a f64 or i64.
    One,
    /// Represents two stack values of computational type one, or one stack value of computational type two.
    Two
}
pub enum FloatType {
    Double,
    Float
}
impl From<FloatType> for StackValueType {
    fn from(ft: FloatType) -> Self {
        match ft {
            FloatType::Double => StackValueType::Two,
            FloatType::Float => StackValueType::One
        }
    }
}
impl From<FloatType> for LocalType {
    fn from(ft: FloatType) -> Self {
        match ft {
            FloatType::Double => LocalType::Double,
            FloatType::Float => LocalType::Float
        }
    }
}
pub enum NaNBehavior {
    ReturnsOne,
    ReturnsNegativeOne
}
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
pub enum LoadOrStore {
    Load,
    Store
}
pub enum GetOrPut {
    Get, Put
}
pub enum MemberType {
    Static, Virtual
}
#[derive(Copy, Clone, Eq, PartialEq)]
pub enum LocalType {
    Int, Long, Float, Double, Reference
}
pub enum ArrayType {
    ByteOrBool, Short, Char, Int, Long, Float, Double, Reference
}
pub enum NumberType {
    Int, Long, Float, Double
}
pub enum BitType {
    Byte, Short, Char, Int, Long, Float, Double
}
pub enum IntType {
    Int, Long
}
pub enum FloatOperation {
    Divide, Add, Subtract, Multiply, Remainder, Negate
}
pub enum IntOperation {
    Divide, Add, Subtract, Multiply, Remainder, Negate, ExclusiveOr, Or, And, ShiftLeft, ShiftRight, UnsignedShiftRight
}
pub enum MonitorOperation {
    Enter,
    Exit
}
/// Abstract tagged union to represent the instruction set.
/// Note that while each valid instruction corresponds to one and only one enum variant,
/// a value may correspond to multiple possibilities of actual operation used in bytecode.
/// Normally, it should choose the option that takes the lowest space.
pub enum Instruction {
    /// Push a constant value to the current stack.
    Push(Constant),
    /// Duplicate one or two stack values and insert them one or more values down.
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
    InvokeExact(Constant),
}
enum InsnListMod {
    Remove,
    RemoveMany(usize),
    Insert(Instruction),
    InsertMany(Vec<Instruction>),
    Replace(usize, Instruction),
    ReplaceMany(usize, Vec<Instruction>)
}
struct InstructionListModifier(BTreeMap<usize, InsnListMod>);
impl InstructionListModifier {
    fn apply(&self, list: &mut InstructionList) {

    }
}
#[repr(transparent)]
pub struct InstructionList(Vec<Instruction>);