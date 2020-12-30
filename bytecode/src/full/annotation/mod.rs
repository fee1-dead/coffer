pub(crate) mod type_annotation;
pub use type_annotation::*;

use std::borrow::Cow;
use std::collections::HashMap;
use super::Type;

/// Some values actually becomes ints in the constant pool.
#[derive(Clone, PartialEq, Debug)]
pub enum AnnotationValue<'a> {
    Byte(i8),
    Char(u16),
    Double(f64),
    Float(f32),
    Int(i32),
    Long(i64),
    Short(i16),
    Boolean(bool),
    String(Cow<'a, str>),
    Enum(Type<'a>, Cow<'a, str>),
    Class(Option<Type<'a>>),
    Annotation(Type<'a>, HashMap<Cow<'a, str>, AnnotationValue<'a>>),
    Array(Vec<AnnotationValue<'a>>)
}

#[derive(Clone, PartialEq, Debug)]
pub struct Annotation<'a> {
    pub annotation_type: Type<'a>,
    pub element_values: HashMap<Cow<'a, str>, AnnotationValue<'a>>
}