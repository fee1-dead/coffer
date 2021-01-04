use super::AnnotationValue;
use super::super::{Type, Catch};
use std::collections::HashMap;
use std::borrow::Cow;
use crate::full::Label;

#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ClassTypeAnnotationTarget {
    GenericTypeParameter(u8) = 0,
    /// `u16::MAX` is `extends`, others are indices of `implements`
    ExtendsImplementsClause(u16) = 0x10,
    /// Type parameter index, and then bound index
    GenericTypeParameterBound(u8, u8) = 0x11,
}

/// tag: 0x13
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct FieldTypeAnnotationTarget;

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
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CodeTypeAnnotationTarget {
    /// Example `@Foo A a = bar();`
    LocalVariable(Label, Label),
    /// Example `try (@Foo A a = bar()) {}`
    ResourceVariable(Label, Label),
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
    Cast(Label, u8),
    /// Example `new Foo<@Bar Baz, @Bar Qux>`
    GenericConstructor(Label, u8),
    /// Example `foo.<@Bar Baz>qux()`
    GenericMethod(Label, u8),
    /// Example `Foo::<@Bar Baz>new`
    GenericConstructorRef(Label, u8),
    /// Example `Foo::<@Bar Baz>method`
    GenericMethodRef(Label, u8),
}



#[derive(Clone, PartialEq, Debug)]
pub enum TypeAnnotationTarget {
    TypeParameter(u8),
    SuperType(u16),
    TypeParameterBound(u8, u8),
    Empty,
    FormalParameter(u8),
    Throws(u16),
    LocalVar(),
    Catch(u16),
    Offset(u16),
    TypeArgument(u16, u8)
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

#[derive(Debug, Clone, PartialEq)]
pub struct FieldTypeAnnotation {
    pub type_path: Vec<(u8, u8)>,
    pub annotation_type: Type,
    pub element_values: HashMap<Cow<'static, str>, AnnotationValue>
}

#[derive(Debug, Clone, PartialEq)]
pub struct CodeTypeAnnotation {
    pub target: CodeTypeAnnotationTarget,
    pub type_path: Vec<(u8, u8)>,
    pub annotation_type: Type,
    pub element_values: HashMap<Cow<'static, str>, AnnotationValue>
}