//! Stack map generation tools.
//!
//! To generate a stack map, we first divide instructions into basic blocks.
//!
//! A basic block is a sequence of instructions that are guaranteed to be executed in order,
//! therefore, if we know the starting frame of a basic block, we can calculate the ending frame
//! by analyzing the instructions in the block.
//!
//! Basic blocks are separated by labels or jump instructions. Labels are at the beginning of a basic block,
//! and jump instructions are at the end of a basic block. A control flow graph represents the execution flow
//! of the basic blocks with edges representing potential branching from one block to other blocks.
//!
//! The algorithm follows a depth first search on the control flow graph, and calculates the starting frame
//! using the method signature. If we ever encounter a block that has been visited before, we check if the
//! starting frame is the same as the one we calculated. If not, we have a problem. After visiting all blocks,
//! we have ensured that frames are consistent with jumps and we can generate the stack map.

use std::collections::HashMap;

use super::{Code, Instruction, Label};
use crate::dynamic::OrDynamic;
use crate::flags::MethodFlags;
use crate::member::{Method, MethodAttribute};
use crate::prelude::{Constant, Type};

/// In the bytecode, we divide code units by labels as they are potential jump targets.
///
/// These units are called "block"s in coffer
pub struct BlockInfo {
    v: BlockVerification,
}

#[derive(Clone, Copy, Debug)]
pub enum VerificationType {
    Top,
    Integer,
    Float,
    Long,
    Double,
    Null,
    UninitializedThis,
    Object,
    Uninitialized,
}

impl VerificationType {
    pub fn is_wide(self) -> bool {
        matches!(self, Self::Long | Self::Double)
    }
}

#[derive(Default, Clone, Debug)]
pub enum BlockVerification {
    #[default]
    Undefined,
    Filled(Frame),
}

#[derive(Clone, Debug, Default)]
pub struct Frame {
    stack: Vec<VerificationType>,
    locals: Vec<VerificationType>,
}

pub fn cut_code(c: &Code) -> Vec<BasicBlock<'_>> {
    let mut v = Vec::new();
    let mut current = BasicBlock::default();

    for ins in &c.code {
        match ins {
            | Instruction::Label(l) => {
                if current.should_start_new_block_for_label() {
                    v.push(current);
                    current = BasicBlock {
                        label: Some(l),
                        ..Default::default()
                    }
                } else {
                    current.label = Some(*l);
                }
            }
            | x @ Instruction::TableSwitch { .. }
            | x @ Instruction::Jump(..)
            | x @ Instruction::LookupSwitch { .. } => {
                current.terminator = Some(x.clone());
                v.push(current);
                current = BasicBlock::default();
            }
            | Instruction::Jsr(_) => {
                unimplemented!("jsr is not implemented");
            }
            | x => {
                current.instructions.push(x);
            }
        }
    }

    v
}

#[derive(Default)]
pub struct BasicBlock<'a> {
    label: Option<Label>,
    start_frame: Option<Frame>,
    end_frame: Option<Frame>,
    instructions: Vec<&'a Instruction>,
    /// if this block does not end with a jump instruction, then this is `None`.
    terminator: Option<&'a Instruction>,
}

impl BasicBlock<'_> {
    pub fn should_start_new_block_for_label(&self) -> bool {
        self.label.is_some() || !self.instructions.is_empty() || self.terminator.is_some()
    }
}

pub struct Analyzer<'a> {
    method: &'a Method,
    /// the code
    blocks: Vec<BasicBlock<'a>>,
    label_to_index: HashMap<Label, usize>,
}

impl<'a> Analyzer<'a> {
    pub fn new(method: &'a Method, blocks: Vec<BasicBlock<'a>>) -> Self {
        let code_idx = method
            .attributes
            .iter()
            .position(|x| matches!(x, MethodAttribute::Code(_)))
            .expect("native/no code method passed to analyzer");
        let code = method.attributes.swap_remove(code_idx);
        let MethodAttribute::Code(code) = code else { unreachable!() };

        let label_to_index = code
            .code
            .iter()
            .enumerate()
            .filter_map(|(i, insn)| {
                if let Instruction::Label(l) = insn {
                    Some((*l, i))
                } else {
                    None
                }
            })
            .collect();

        Self {
            method,
            code,
            label_to_index,
        }
    }

    /// https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.10.1.2
    pub fn ty_to_verificaton(ty: &Type) -> VerificationType {
        match ty {
            Type::Char | Type::Byte | Type::Short | Type::Boolean | Type::Int => {
                VerificationType::Integer
            }
            Type::Double => VerificationType::Double,
            Type::Long => VerificationType::Long,
            Type::Float => VerificationType::Float,
            Type::Ref(_) | Type::ArrayRef(_, _) => VerificationType::Object,
            Type::Method { .. } => unreachable!(),
        }
    }

    pub fn constant_to_verification(c: &Constant) -> VerificationType {
        match c {
            Constant::I32(_) => VerificationType::Integer,
            Constant::I64(_) => VerificationType::Long,
            Constant::F32(_) => VerificationType::Float,
            Constant::F64(_) => VerificationType::Double,
            Constant::String(_)
            | Constant::Class(_)
            | Constant::Member(_)
            | Constant::MethodType(_)
            | Constant::MethodHandle(_) => VerificationType::Object,
        }
    }

    pub fn analyze_code(&mut self) {
        use VerificationType::*;
        // Let us first construct the stack frame.
        // FIXME: this doesn't special case Ljava/lang/Object;, whose constructor's `this` type is Object, and not UninitializedThis.
        let mut frame = Frame::default();
        // if our method is not static..
        let is_static = self.method.access.contains(MethodFlags::ACC_STATIC);
        let is_constructor = &*self.method.name == "<init>";
        match (is_static, is_constructor) {
            (false, false) => frame.locals.push(Object),
            (false, true) => frame.locals.push(UninitializedThis), // `this` is uninitialized
            (true, false) => {}
            (true, true) => panic!(
                "invalid bytecode: method cannot be both static and constructor at the same time."
            ),
        }
        frame.locals.extend(
            self.method
                .descriptor
                .as_method()
                .unwrap()
                .0
                .iter()
                .map(|x| Self::ty_to_verificaton(x)),
        );
        // At this point, we have generated a frame for the starting position.
        // The stack is empty and the locals are populated based on the method.

        // emulate code behavior until we run into a jump or a label..
        for instruction in &self.code.code {
            match instruction {
                Instruction::NoOp | Instruction::LineNumber(_) => {}
                Instruction::Ret(_) | Instruction::Jsr(_) => {
                    unimplemented!("ret and jsr are not supported")
                }
                Instruction::PushNull => frame.stack.push(Null),
                Instruction::Push(OrDynamic::Dynamic(d)) => {
                    frame.stack.push(Self::ty_to_verificaton(&d.descriptor));
                }
                Instruction::Push(OrDynamic::Static(c)) => {
                    frame.stack.push(Self::constant_to_verification(c));
                }
                Instruction::Dup => {
                    if frame.stack.is_empty() {
                        panic!("`dup` on empty stack");
                    }
                    if frame.stack.last().unwrap().is_wide() {
                        panic!("`dup` on wide type");
                    }
                    frame.stack.push(*frame.stack.last().unwrap());
                }
                Instruction::DupX1 => todo!(),
                Instruction::DupX2 => todo!(),
                Instruction::Dup2 => todo!(),
                Instruction::Dup2X1 => todo!(),
                Instruction::Dup2X2 => todo!(),
                Instruction::Pop1 => todo!(),
                Instruction::Pop2 => todo!(),
                Instruction::Jump(_, _) => todo!(),
                Instruction::CompareLongs => todo!(),
                Instruction::CompareFloats(_, _) => todo!(),
                Instruction::LocalVariable(_, _, _) => todo!(),
                Instruction::Array(_, _) => todo!(),
                Instruction::ArrayLength => todo!(),
                Instruction::IntOperation(_, _) => todo!(),
                Instruction::FloatOperation(_, _) => todo!(),
                Instruction::Throw => todo!(),
                Instruction::CheckCast(_) => todo!(),
                Instruction::InstanceOf(_) => todo!(),
                Instruction::NewArray(_, _) => todo!(),
                Instruction::Monitor(_) => todo!(),
                Instruction::New(_) => todo!(),
                Instruction::Conversion(_, _) => todo!(),
                Instruction::ConvertInt(_) => todo!(),
                Instruction::Return(_) => todo!(),
                Instruction::Field(_, _, _) => todo!(),
                Instruction::InvokeExact(_, _) => todo!(),
                Instruction::InvokeSpecial(_) => todo!(),
                Instruction::InvokeDynamic(_) => todo!(),
                Instruction::InvokeInterface(_, _) => todo!(),
                Instruction::Swap => todo!(),
                Instruction::IntIncrement(_, _) => todo!(),
                Instruction::TableSwitch {
                    default,
                    low,
                    offsets,
                } => todo!(),
                Instruction::LookupSwitch { default, table } => todo!(),
                Instruction::Label(_) => todo!(),
            }
            self.code.max_stack = self.code.max_stack.max(frame.stack.len() as u16);
            self.code.max_locals = self.code.max_locals.max(frame.locals.len() as u16);
        }
        todo!()
    }
    pub fn analyze(mut self) -> Method {
        self.analyze_code();
        self.method
            .attributes
            .push(MethodAttribute::Code(self.code));
        self.method
    }
}
