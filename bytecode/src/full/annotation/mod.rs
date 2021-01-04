pub(crate) mod type_annotation;
pub use type_annotation::*;

use std::borrow::Cow;
use std::collections::HashMap;
use super::Type;

/// Some values actually becomes ints in the constant pool.
#[derive(Clone, PartialEq, Debug)]
pub enum AnnotationValue {
    Byte(i8),
    Char(u16),
    Double(f64),
    Float(f32),
    Int(i32),
    Long(i64),
    Short(i16),
    Boolean(bool),
    String(Cow<'static, str>),
    Enum(Type, Cow<'static, str>),
    Class(Option<Type>),
    Annotation(Type, HashMap<Cow<'static, str>, AnnotationValue>),
    Array(Vec<AnnotationValue>)
}

#[derive(Clone, PartialEq, Debug)]
pub struct Annotation {
    pub annotation_type: Type,
    pub element_values: HashMap<Cow<'static, str>, AnnotationValue>
}