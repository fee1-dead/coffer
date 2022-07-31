use wtf_8::Wtf8Str;

use crate::annotation::{Annotation, ClassTypeAnnotation};
use crate::mod_utf8::{modified_utf8_to_string, string_to_modified_utf8};
use crate::module::Module;
use crate::prelude::*;

/// An unrecognized, unknown raw attribute.
#[derive(Clone, PartialEq, Debug)]
pub struct RawAttribute {
    /// Whether to keep this attribute upon writing.
    ///
    /// Attributes that are related to local variables will default to `false`, whereas newly created attributes will be `true`.
    pub(crate) keep: bool,
    /// The name of this attribute.
    pub name: Cow<'static, Wtf8Str>,
    /// The inner data of this attribute.
    pub inner: Cow<'static, [u8]>,
}

impl RawAttribute {
    /// Creates a new raw attribute with name and inner data.
    ///
    /// `String` and `Vec<u8>`, string literal and array literal are all accepted because this uses a Cow.
    pub fn new<S: Into<Cow<'static, Wtf8Str>>, B: Into<Cow<'static, [u8]>>>(name: S, inner: B) -> Self {
        Self {
            keep: true,
            name: name.into(),
            inner: inner.into(),
        }
    }
    /// Used by the procedural macro.
    pub(crate) fn __new(name: Cow<'static, Wtf8Str>, inner: Vec<u8>) -> Self {
        Self {
            keep: false,
            name,
            inner: Cow::Owned(inner),
        }
    }
}

impl ConstantPoolReadWrite for Option<(Cow<'static, Wtf8Str>, Type)> {
    fn read_from<C: ConstantPoolReader, R: Read>(
        cp: &mut C,
        reader: &mut R,
    ) -> Result<Self, Error> {
        let idx = u16::read_from(reader)?;
        if idx == 0 {
            Ok(None)
        } else {
            try_cp_read!(idx, cp.read_nameandtype(idx)).map(Some)
        }
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(
        &self,
        cp: &mut C,
        writer: &mut W,
    ) -> Result<(), Error> {
        if let Some((n, t)) = self {
            cp.insert_nameandtype(n.clone(), t.to_string())
                .write_to(writer)
        } else {
            0u16.write_to(writer)
        }
    }
}

/// A raw string that holds extended debugging information that has no effect on the JVM.
///
/// The [`ReadWrite`] implementation for this just comsumes the whole reader.
/// This works because the macro generates inner readers for attributes which are safe to consume.
#[derive(Eq, PartialEq, Debug, Clone)]
pub struct SourceDebugExtension(pub Cow<'static, str>);

impl ReadWrite for SourceDebugExtension {
    fn read_from<T: Read>(reader: &mut T) -> Result<Self> {
        let mut buf = vec![];
        reader.read_to_end(&mut buf)?;
        Ok(SourceDebugExtension(modified_utf8_to_string(&buf)?.into()))
    }

    fn write_to<T: Write>(&self, writer: &mut T) -> Result<()> {
        writer.write_all(&string_to_modified_utf8(&self.0))?;
        Ok(())
    }
}

#[derive(Eq, PartialEq, Debug, Clone, ConstantPoolReadWrite)]
pub struct InnerClass {
    #[str_type(Class)]
    pub inner_fqname: Cow<'static, str>,
    #[str_optional]
    #[str_type(Class)]
    pub outer_fqname: Option<Cow<'static, str>>,
    /// None if the inner class is an anonymous class.
    #[str_optional]
    pub inner_name: Option<Cow<'static, str>>,
    #[use_normal_rw]
    pub inner_access: InnerClassFlags,
}

#[derive(PartialEq, Debug, Clone, ConstantPoolReadWrite)]
#[attr_enum]
pub enum ClassAttribute {
    Signature(ClassSignature),
    Synthetic,
    Deprecated,
    SourceFile(Cow<'static, Wtf8Str>),
    InnerClasses(#[vec_len_type(u16)] Vec<InnerClass>),
    EnclosingMethod(
        #[str_type(Class)] Cow<'static, Wtf8Str>,
        Option<(Cow<'static, Wtf8Str>, Type)>,
    ),
    SourceDebugExtension(#[use_normal_rw] SourceDebugExtension),
    BootstrapMethods(#[vec_len_type(u16)] Vec<BootstrapMethod>),
    Module(Module),
    ModulePackages(
        #[vec_len_type(u16)]
        #[str_type(Package)]
        Vec<Cow<'static, Wtf8Str>>,
    ),
    ModuleMainClass(#[str_type(Class)] Cow<'static, Wtf8Str>),
    NestHost(#[str_type(Class)] Cow<'static, Wtf8Str>),
    NestMembers(
        #[vec_len_type(u16)]
        #[str_type(Class)]
        Vec<Cow<'static, Wtf8Str>>,
    ),
    RuntimeVisibleAnnotations(#[vec_len_type(u16)] Vec<Annotation>),
    RuntimeInvisibleAnnotations(#[vec_len_type(u16)] Vec<Annotation>),
    RuntimeVisibleTypeAnnotations(#[vec_len_type(u16)] Vec<ClassTypeAnnotation>),
    RuntimeInvisibleTypeAnnotations(#[vec_len_type(u16)] Vec<ClassTypeAnnotation>),
    #[raw_variant]
    Raw(RawAttribute),
}
