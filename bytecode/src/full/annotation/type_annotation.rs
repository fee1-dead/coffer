use std::borrow::Cow;
use std::collections::HashMap;

use crate::{ConstantPoolReadWrite, ReadWrite};
use crate::full::code::{Catch, Label};

use super::AnnotationValue;
use super::super::Type;

#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ClassTypeAnnotationTarget {
    GenericTypeParameter(u8) = 0,
    /// `u16::MAX` is `extends`, others are indices of `implements`
    ExtendsImplementsClause(u16) = 0x10,
    /// Type parameter index, and then bound index
    GenericTypeParameterBound(u8, u8) = 0x11,
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum MethodTypeAnnotationTarget {
    GenericTypeParameter(u8) = 1,
    GenericTypeParameterBound(u8, u8) = 0x12,
    Return = 0x14,
    Reciever = 0x15,
    FormalParameter(u8) = 0x16,
    Throws(u16) = 0x17
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct LocalVarTarget {
    pub start: Label,
    pub end: Label,
    pub idx: u16
}

impl ConstantPoolReadWrite for LocalVarTarget {
    fn read_from<C: crate::ConstantPoolReader, R: std::io::Read>(cp: &mut C, reader: &mut R) -> crate::Result<Self> {
        let start_idx = u16::read_from(reader)?;
        let start = cp.get_label(start_idx as _);
        let end = cp.get_label(start_idx as u32 + (u16::read_from(reader)? - 1) as u32);
        Ok(LocalVarTarget {
            start, end, idx: crate::read_from!(reader)?
        })
    }

    fn write_to<C: crate::ConstantPoolWriter, W: std::io::Write>(&self, cp: &mut C, writer: &mut W) -> crate::Result<()> {
        let start = cp.label(self.start);
        let end = cp.label(self.end);
        start.write_to(writer)?;
        (end - start + 1).write_to(writer)?;
        self.idx.write_to(writer)?;
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, ConstantPoolReadWrite)]
#[tag_type(u8)]
pub enum CodeTypeAnnotationTarget {
    #[tag(0x40)]
    /// Example `@Foo A a = bar();`
    LocalVariable(#[vec_len_type(u16)] Vec<LocalVarTarget>),
    /// Example `try (@Foo A a = bar()) {}`
    ResourceVariable(#[vec_len_type(u16)] Vec<LocalVarTarget>),
    /// Example `try { } catch (@Foo A a) { }`
    CatchParameter(Catch),
    /// Example `a instanceof @Foo B`
    InstanceOf(Label),
    /// Example `new @Foo Bar()`
    Constructor(Label),
    /// Example `@Baz Qux::new`
    ConstructorRef(Label),
    /// Example `@Baz Qux::method`
    MethodRef(Label),
    /// Example `(@Foo A & @Bar B) o` has two annotations with the same label, but different for the second field.
    Cast(Label, #[use_normal_rw] u8),
    /// Example `new Foo<@Bar Baz, @Bar Qux>`
    GenericConstructor(Label, #[use_normal_rw] u8),
    /// Example `foo.<@Bar Baz>qux()`
    GenericMethod(Label, #[use_normal_rw] u8),
    /// Example `Foo::<@Bar Baz>new`
    GenericConstructorRef(Label, #[use_normal_rw] u8),
    /// Example `Foo::<@Bar Baz>method`
    GenericMethodRef(Label, #[use_normal_rw] u8),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, ReadWrite)]
#[tag_type(u8)]
pub enum TypePath {
    Array(u8), Nested(u8), TypeBound(u8), TypeArgument(u8)
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassTypeAnnotation {
    pub target: ClassTypeAnnotationTarget,
    pub type_path: Vec<(u8, u8)>,
    pub annotation_type: Type,
    pub element_values: HashMap<Cow<'static, str>, AnnotationValue>
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodTypeAnnotation {
    pub target: MethodTypeAnnotationTarget,
    pub type_path: Vec<(u8, u8)>,
    pub annotation_type: Type,
    pub element_values: HashMap<Cow<'static, str>, AnnotationValue>
}

#[derive(Debug, Clone, PartialEq, ReadWrite, Copy, Eq)]
#[tag_type(u8)]
pub enum FieldTarget { #[tag(0x13)] Field }

#[derive(Debug, Clone, PartialEq, ConstantPoolReadWrite)]
pub struct FieldTypeAnnotation {
    #[use_normal_rw]
    pub target_type: FieldTarget,
    #[vec_len_type(u8)]
    #[use_normal_rw]
    pub type_path: Vec<TypePath>,
    pub annotation_type: Type,
    pub element_values: HashMap<Cow<'static, str>, AnnotationValue>
}


#[derive(Debug, Clone, PartialEq, ConstantPoolReadWrite)]
pub struct CodeTypeAnnotation {
    pub target: CodeTypeAnnotationTarget,
    #[vec_len_type(u8)]
    #[use_normal_rw]
    pub type_path: Vec<TypePath>,
    pub annotation_type: Type,
    pub element_values: HashMap<Cow<'static, str>, AnnotationValue>
}