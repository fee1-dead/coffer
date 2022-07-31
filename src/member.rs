//! Members of a java class
//!
//! They can be fields or methods.
use wtf_8::Wtf8Str;

use crate::annotation::{
    Annotation, AnnotationValue, FieldTypeAnnotation, MethodTypeAnnotation, ParameterAnnotations,
};
use crate::code::Code;
use crate::prelude::*;

/// A reference to a member.
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct MemberRef {
    /// The class holding this member.
    pub owner: Cow<'static, Wtf8Str>,
    /// The name of this member.
    pub name: Cow<'static, Wtf8Str>,
    /// The descriptor of this member.
    ///
    /// Can be a field descriptor or a method descriptor.
    ///
    /// This field is used to determine whether this member is a field or a method.
    pub descriptor: Type,
    /// Determines whether the owner is an interface.
    ///
    /// This field is only applicable to methods, and if this member is a field this field will be ignored.
    pub itfs: bool,
}

impl ConstantPoolReadWrite for MemberRef {
    fn read_from<C: ConstantPoolReader, R: Read>(
        cp: &mut C,
        reader: &mut R,
    ) -> Result<Self, Error> {
        try_cp_read!(cp, reader, read_member)
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(
        &self,
        cp: &mut C,
        writer: &mut W,
    ) -> Result<(), Error> {
        cp.insert_member(self.clone()).write_to(writer)
    }
}

/// Completed
#[derive(PartialEq, Debug, Clone, ConstantPoolReadWrite)]
#[attr_enum]
pub enum FieldAttribute {
    Deprecated,
    Synthetic,
    Signature(FieldSignature),
    ConstantValue(Constant),
    RuntimeVisibleAnnotations(#[vec_len_type(u16)] Vec<Annotation>),
    RuntimeInvisibleAnnotations(#[vec_len_type(u16)] Vec<Annotation>),
    RuntimeVisibleTypeAnnotations(#[vec_len_type(u16)] Vec<FieldTypeAnnotation>),
    RuntimeInvisibleTypeAnnotations(#[vec_len_type(u16)] Vec<FieldTypeAnnotation>),
    #[raw_variant]
    Raw(RawAttribute),
}

#[derive(PartialEq, Debug, Clone, ConstantPoolReadWrite)]
pub struct Field {
    #[use_normal_rw]
    pub access: FieldFlags,
    pub name: Cow<'static, str>,
    pub descriptor: Type,
    #[vec_len_type(u16)]
    pub attrs: Vec<FieldAttribute>,
}

#[derive(PartialEq, Debug, Clone, ConstantPoolReadWrite)]
pub struct MethodParameter {
    #[str_optional]
    name: Option<Cow<'static, str>>,
    #[use_normal_rw]
    access: MethodParameterFlags,
}

#[derive(PartialEq, Debug, Clone, ConstantPoolReadWrite)]
#[attr_enum]
pub enum MethodAttribute {
    Code(Code),
    Deprecated,
    Synthetic,
    Signature(MethodSignature),
    RuntimeVisibleAnnotations(#[vec_len_type(u16)] Vec<Annotation>),
    RuntimeInvisibleAnnotations(#[vec_len_type(u16)] Vec<Annotation>),
    RuntimeVisibleTypeAnnotations(#[vec_len_type(u16)] Vec<MethodTypeAnnotation>),
    RuntimeInvisibleTypeAnnotations(#[vec_len_type(u16)] Vec<MethodTypeAnnotation>),
    RuntimeVisibleParameterAnnotations(#[vec_len_type(u8)] Vec<ParameterAnnotations>),
    RuntimeInvisibleParameterAnnotations(#[vec_len_type(u8)] Vec<ParameterAnnotations>),
    Exceptions(
        #[vec_len_type(u16)]
        #[str_type(Class)]
        Vec<Cow<'static, str>>,
    ),
    AnnotationDefault(AnnotationValue),
    MethodParameters(#[vec_len_type(u8)] Vec<MethodParameter>),
    #[raw_variant]
    Raw(RawAttribute),
}

#[derive(PartialEq, Debug, Clone, ConstantPoolReadWrite)]
pub struct Method {
    #[use_normal_rw]
    pub access: MethodFlags,
    pub name: Cow<'static, Wtf8Str>,
    pub descriptor: Type,
    #[vec_len_type(u16)]
    pub attributes: Vec<MethodAttribute>,
}
