use std::borrow::Cow;
use std::collections::HashMap;

use super::super::Type;
use super::AnnotationValue;
use crate::code::{Catch, Label};
use crate::{ConstantPoolReadWrite, ReadWrite};

/// Represents where a type annotation is annotated in a class.
#[derive(Copy, Clone, Debug, Eq, PartialEq, ReadWrite)]
#[tag_type(u8)]
pub enum ClassTypeAnnotationTarget {
    /// The type annotation is before a generic type parameter at an index.
    #[tag(0)]
    GenericTypeParameter(u8),
    /// The type annotation is before the type after an `extends` or `implements` clause.
    ///
    /// `u16::MAX` is `extends`, others are indices of `implements`
    #[tag(0x10)]
    ExtendsImplementsClause(u16),
    /// The type annotation is applied to the bound on a generic type parameter.
    ///
    /// Type parameter index, and then bound index
    #[tag(0x11)]
    GenericTypeParameterBound(u8, u8),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, ReadWrite)]
#[tag_type(u8)]
pub enum MethodTypeAnnotationTarget {
    #[tag(0x1)]
    GenericTypeParameter(u8),
    #[tag(0x12)]
    GenericTypeParameterBound(u8, u8),
    #[tag(0x14)]
    Return,
    #[tag(0x15)]
    Reciever,
    #[tag(0x16)]
    FormalParameter(u8),
    #[tag(0x17)]
    Throws(u16),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct LocalVarTarget {
    pub start: Label,
    pub end: Label,
    pub idx: u16,
}

impl ConstantPoolReadWrite for LocalVarTarget {
    fn read_from<C: crate::ConstantPoolReader, R: std::io::Read>(
        cp: &mut C,
        reader: &mut R,
    ) -> crate::Result<Self> {
        let start_idx = u16::read_from(reader)?;
        let start = cp.get_label(start_idx as _);
        let end = cp.get_label(start_idx as u32 + (u16::read_from(reader)? - 1) as u32);
        Ok(LocalVarTarget {
            start,
            end,
            idx: crate::read_from!(reader)?,
        })
    }

    fn write_to<C: crate::ConstantPoolWriter, W: std::io::Write>(
        &self,
        cp: &mut C,
        writer: &mut W,
    ) -> crate::Result<()> {
        let start = cp.label(&self.start);
        let end = cp.label(&self.end);
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
    Array(u8),
    Nested(u8),
    TypeBound(u8),
    TypeArgument(u8),
}

#[derive(Debug, Clone, PartialEq, ConstantPoolReadWrite)]
pub struct ClassTypeAnnotation {
    #[use_normal_rw]
    pub target: ClassTypeAnnotationTarget,
    #[vec_len_type(u8)]
    #[use_normal_rw]
    pub type_path: Vec<TypePath>,
    pub annotation_type: Type,
    pub element_values: HashMap<Cow<'static, str>, AnnotationValue>,
}

#[derive(Debug, Clone, PartialEq, ConstantPoolReadWrite)]
pub struct MethodTypeAnnotation {
    #[use_normal_rw]
    pub target: MethodTypeAnnotationTarget,
    #[vec_len_type(u8)]
    #[use_normal_rw]
    pub type_path: Vec<TypePath>,
    pub annotation_type: Type,
    pub element_values: HashMap<Cow<'static, str>, AnnotationValue>,
}

#[derive(Debug, Clone, PartialEq, ReadWrite, Copy, Eq)]
#[tag_type(u8)]
pub enum FieldTarget {
    #[tag(0x13)]
    Field,
}

#[derive(Debug, Clone, PartialEq, ConstantPoolReadWrite)]
pub struct FieldTypeAnnotation {
    #[use_normal_rw]
    pub target_type: FieldTarget,
    #[vec_len_type(u8)]
    #[use_normal_rw]
    pub type_path: Vec<TypePath>,
    pub annotation_type: Type,
    pub element_values: HashMap<Cow<'static, str>, AnnotationValue>,
}

#[derive(Debug, Clone, PartialEq, ConstantPoolReadWrite)]
pub struct CodeTypeAnnotation {
    pub target: CodeTypeAnnotationTarget,
    #[vec_len_type(u8)]
    #[use_normal_rw]
    pub type_path: Vec<TypePath>,
    pub annotation_type: Type,
    pub element_values: HashMap<Cow<'static, str>, AnnotationValue>,
}
