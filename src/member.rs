//! Members of a java class
//!
//! They can be fields or methods.
use wtf_8::Wtf8Str;

/* 
use crate::annotation::{
    Annotation, AnnotationValue, FieldTypeAnnotation, MethodTypeAnnotation, ParameterAnnotations,
};*/
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
#[derive(PartialEq, Debug, Clone, AttributeEnum)]
pub enum FieldAttribute {
    Deprecated,
    Synthetic,
    Signature(FieldSignature),
    ConstantValue(Constant),
    /*RuntimeVisibleAnnotations(#[coffer(as = "h::Vec16")] Vec<Annotation>),
    RuntimeInvisibleAnnotations(#[coffer(as = "h::Vec16")] Vec<Annotation>),
    RuntimeVisibleTypeAnnotations(#[coffer(as = "h::Vec16")] Vec<FieldTypeAnnotation>),
    RuntimeInvisibleTypeAnnotations(#[coffer(as = "h::Vec16")] Vec<FieldTypeAnnotation>),*/
    #[coffer(raw_variant)]
    Raw(RawAttribute),
}

#[derive(PartialEq, Debug, Clone, ConstantPoolReadWrite)]
pub struct Field {
    #[coffer(as = "h::Normal")]
    pub access: FieldFlags,
    pub name: Cow<'static, Wtf8Str>,
    pub descriptor: Type,
    #[coffer(as = "h::Vec16")]
    pub attrs: Vec<FieldAttribute>,
}

#[derive(PartialEq, Debug, Clone, ConstantPoolReadWrite)]
pub struct MethodParameter {
    name: Option<Cow<'static, Wtf8Str>>,
    #[coffer(as = "h::Normal")]
    access: MethodParameterFlags,
}

#[derive(PartialEq, Debug, Clone, AttributeEnum)]
pub enum MethodAttribute {
    Code(Code),
    Deprecated,
    Synthetic,
    Signature(MethodSignature),
    /*RuntimeVisibleAnnotations(#[coffer(as = "h::Vec16")] Vec<Annotation>),
    RuntimeInvisibleAnnotations(#[coffer(as = "h::Vec16")] Vec<Annotation>),
    RuntimeVisibleTypeAnnotations(#[coffer(as = "h::Vec16")] Vec<MethodTypeAnnotation>),
    RuntimeInvisibleTypeAnnotations(#[coffer(as = "h::Vec16")] Vec<MethodTypeAnnotation>),
    RuntimeVisibleParameterAnnotations(#[coffer(as = "h::Vec8")] Vec<ParameterAnnotations>),
    RuntimeInvisibleParameterAnnotations(#[coffer(as = "h::Vec8")] Vec<ParameterAnnotations>),*/
    Exceptions(#[coffer(as = "h::Vec16<h::Class>")] Vec<Cow<'static, Wtf8Str>>),
    // AnnotationDefault(AnnotationValue),
    MethodParameters(#[coffer(as = "h::Vec8")] Vec<MethodParameter>),
    #[coffer(raw_variant)]
    Raw(RawAttribute),
}

#[derive(PartialEq, Debug, Clone, ConstantPoolReadWrite)]
pub struct Method {
    #[coffer(as = "h::Normal")]
    pub access: MethodFlags,
    pub name: Cow<'static, Wtf8Str>,
    pub descriptor: Type,
    #[coffer(as = "h::Vec16")]
    pub attributes: Vec<MethodAttribute>,
}
