
/*
    This file is part of Coffer.

    Coffer is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Coffer is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Coffer. (LICENSE.md)  If not, see <https://www.gnu.org/licenses/>.
*/
use std::borrow::Cow;
use std::hash::{Hash, Hasher};
use std::io::{Read, Write};

use annotation::Annotation;
use crate::prelude::*;
pub use code::*;

use crate::{ConstantPoolReader, ConstantPoolReadWrite, ConstantPoolWriter, Error, read_from, ReadWrite, Result, MemberRef};
use crate::flags::{FieldFlags, MethodParameterFlags, MethodFlags, InnerClassFlags, RequireFlags, ModuleFlags, ClassFlags};
use crate::full::annotation::{AnnotationValue, FieldTypeAnnotation, MethodTypeAnnotation};
use crate::full::version::JavaVersion;

pub mod annotation;
pub mod version;

pub mod cp;
mod code;



#[derive(Clone, PartialEq, Debug)]
pub struct RawAttribute {
    /// Whether to keep this attribute upon writing.
    /// Attributes that are related to local variables will default to `false`, whereas newly created attributes will be `true`.
    keep: bool,
    pub name: Cow<'static, str>,
    pub inner: Cow<'static, [u8]>
}

impl RawAttribute {
    pub fn new<S: Into<Cow<'static, str>>, B: Into<Cow<'static, [u8]>>>(name: S, inner: B) -> Self {
        Self {
            keep: true,
            name: name.into(),
            inner: inner.into()
        }
    }
    /// Used by the procedural macro.
    fn __new(name: Cow<'static, str>, inner: Vec<u8>) -> Self {
        Self {
            keep: false,
            name,
            inner: Cow::Owned(inner)
        }
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
    RuntimeInvisibleAnnotations(#[vec_len_type(u16)]Vec<Annotation>),
    RuntimeVisibleTypeAnnotations(#[vec_len_type(u16)] Vec<FieldTypeAnnotation>),
    RuntimeInvisibleTypeAnnotations(#[vec_len_type(u16)] Vec<FieldTypeAnnotation>),
    #[raw_variant]
    Raw(RawAttribute)
}

#[derive(PartialEq, Debug, Clone)]
pub struct Field {
    pub access: FieldFlags,
    pub name: Cow<'static, str>,
    pub descriptor: Type,
    pub attrs: Vec<FieldAttribute>
}

#[derive(PartialEq, Debug, Clone, ConstantPoolReadWrite)]
pub struct ParameterAnnotations(#[vec_len_type(u16)] Vec<Annotation>);

impl std::ops::Deref for ParameterAnnotations {
    type Target = Vec<Annotation>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for ParameterAnnotations {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(PartialEq, Debug, Clone, ConstantPoolReadWrite)]
pub struct MethodParameter {
    #[str_optional]
    name: Option<Cow<'static, str>>,
    #[use_normal_rw]
    access: MethodParameterFlags
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
    Exceptions(#[vec_len_type(u8)] Vec<Clazz>),
    AnnotationDefault(AnnotationValue),
    MethodParameters(#[vec_len_type(u8)] Vec<MethodParameter>),
    #[raw_variant]
    Raw(RawAttribute),
}

#[derive(PartialEq, Debug, Clone)]
pub struct Method {
    pub access: MethodFlags,
    pub name: Cow<'static, str>,
    pub descriptor: Type,
    pub attributes: Vec<MethodAttribute>
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, ConstantPoolReadWrite)]
#[tag_type(u8)]
pub enum VerificationType {
    Top, Int, Float, Long, Double, Null, UninitializedThis, Object(#[str_type(Class)] Cow<'static, str>),
    /// Following the label, must be a `NEW` instruction.
    UninitializedVariable(Label)
}

impl VerificationType {
    pub const fn is_wide(&self) -> bool {
        matches!(self, VerificationType::Double | VerificationType::Long)
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
    pub inner_access: InnerClassFlags
}

#[derive(Clone, Eq, PartialEq, Debug, ConstantPoolReadWrite)]
pub struct Require {
    #[str_type(Module)]
    pub module: Cow<'static, str>,
    #[use_normal_rw]
    pub flags: RequireFlags,
    #[str_optional]
    pub version: Option<Cow<'static, str>>
}

#[repr(transparent)]
#[derive(Clone, Eq, PartialEq, Debug, ConstantPoolReadWrite)]
pub struct To(#[str_type(Module)] Cow<'static, str>);

#[repr(transparent)]
#[derive(Clone, Eq, PartialEq, Debug, ConstantPoolReadWrite)]
pub struct Clazz(#[str_type(Class)] Cow<'static, str>);

#[derive(Clone, Eq, PartialEq, Debug, ConstantPoolReadWrite)]
pub struct Provide {
    #[str_type(Class)]
    pub class: Cow<'static, str>,
    #[vec_len_type(u16)]
    pub with: Vec<Clazz>
}

#[derive(Eq, PartialEq, Debug, Clone, ConstantPoolReadWrite)]
pub struct Module {
    #[str_type(Module)]
    pub name: Cow<'static, str>,
    #[use_normal_rw]
    pub flags: ModuleFlags,
    #[str_optional]
    pub version: Option<Cow<'static, str>>,
    #[vec_len_type(u16)]
    pub requires: Vec<Require>,
    #[vec_len_type(u16)]
    pub exports: Vec<Export>,
    #[vec_len_type(u16)]
    pub opens: Vec<Open>,
    #[vec_len_type(u16)]
    pub uses: Vec<Clazz>,
    #[vec_len_type(u16)]
    pub provides: Vec<Provide>
}

#[derive(PartialEq, Debug, Clone)]
pub enum ClassAttribute {
    Signature(ClassSignature),
    Synthetic, Deprecated, SourceFile(Cow<'static, str>), InnerClasses(Vec<InnerClass>),
    /// first: fully qualified name of the innermost outer class.
    /// second: name of the method that encloses this inner/anonymous class.
    /// third: descriptor of the method.
    EnclosingMethod(Cow<'static, str>, Cow<'static, str>, Type), SourceDebugExtension(Cow<'static, str>),
    BootstrapMethods(Vec<BootstrapMethod>), Module(Module), ModulePackages(Vec<Cow<'static, str>>), ModuleMainClass(Cow<'static, str>),
    NestHost(Cow<'static, str>), NestMembers(Vec<Cow<'static, str>>), Raw(RawAttribute)
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct BootstrapMethod {
    pub method: MethodHandle,
    pub arguments: Vec<OrDynamic<Constant>>
}

#[derive(PartialEq, Debug, Clone)]
pub struct Class {
    pub version: JavaVersion,
    pub access: ClassFlags,
    pub name: Cow<'static, str>,
    /// java/lang/Object has no superclass.
    pub super_name: Option<Cow<'static, str>>,
    pub interfaces: Vec<Cow<'static, str>>,
    pub fields: Vec<Field>,
    pub methods: Vec<Method>,
    pub attributes: Vec<ClassAttribute>
}