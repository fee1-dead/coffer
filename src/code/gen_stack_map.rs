//! Stack map generation tools.

use std::collections::HashMap;

use super::{Code, Instruction, Label};
use crate::{member::{Method, MethodAttribute}, flags::MethodFlags, prelude::Type};

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

impl BlockInfo {}

pub struct Analyzer {
    /// the method
    method: Method,
    /// the code
    code: Code,
    label_to_index: HashMap<Label, usize>,
}

impl Analyzer {
    pub fn new(mut method: Method) -> Self {
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
            | Type::Char
            | Type::Byte
            | Type::Short
            | Type::Boolean
            | Type::Int => VerificationType::Integer,
            | Type::Double => VerificationType::Double,
            | Type::Long => VerificationType::Long,
            | Type::Float => VerificationType::Float,
            | Type::Ref(_)
            | Type::ArrayRef(_, _) => VerificationType::Object,
            | Type::Method { .. } => unreachable!(),
        }
    }
    pub fn analyze_code(&mut self) {
        use VerificationType::*;
        // Let us first construct the stack frame.
        // FIXME: this doesn't special case Ljava/lang/Object;, whose constructor's `this` type is Object, and not UninitializedThis.
        let mut frame = Frame::default();
        // if our method is not static..
        let is_static = self.method.access.contains(MethodFlags::ACC_STATIC);
        let is_constructor = self.method.name == "<init>";
        match (is_static, is_constructor) {
            (false, false) => frame.locals.push(Object),
            (false, true) => frame.locals.push(UninitializedThis), // `this` is uninitialized
            (true, false) => {}
            (true, true) => panic!("invalid bytecode: method cannot be both static and constructor at the same time."),
        }
        frame.locals.extend(self.method.descriptor.as_method().unwrap().0.iter().map(|x| Self::ty_to_verificaton(x)));
        // At this point, we have generated a frame for the starting position.
        // The stack is empty and the locals are populated based on the method.

        // emulate code behavior until we run into a jump or a label..
        for instruction in &self.code.code {
            match instruction {
                Instruction::NoOp => {}
                Instruction::PushNull => frame.stack.push(Null),
                Instruction::Push(_) => todo!(),
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
                Instruction::Jsr(_) => todo!(),
                Instruction::Ret(_) => todo!(),
                Instruction::Swap => todo!(),
                Instruction::IntIncrement(_, _) => todo!(),
                Instruction::LineNumber(_) => todo!(),
                Instruction::TableSwitch { default, low, offsets } => todo!(),
                Instruction::LookupSwitch { default, table } => todo!(),
                Instruction::Label(_) => todo!(),
            }
        }
        todo!()
    }
    pub fn analyze(mut self) -> Method {
        self.analyze_code();
        self.method.attributes.push(MethodAttribute::Code(self.code));
        self.method
    }
}
