
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
use std::hash::Hash;
use std::io::{Read, Write};

use annotation::Annotation;
use crate::prelude::*;
pub use code::*;

use crate::{ConstantPoolReader, ConstantPoolReadWrite, ConstantPoolWriter, Error, ReadWrite, Result};
use crate::flags::{FieldFlags, MethodParameterFlags, MethodFlags, InnerClassFlags, RequireFlags, ModuleFlags, ClassFlags};
use crate::full::annotation::{AnnotationValue, FieldTypeAnnotation, MethodTypeAnnotation};
use crate::version::JavaVersion;

pub mod annotation;

pub mod cp;
mod code;

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

#[derive(PartialEq, Debug, Clone, ConstantPoolReadWrite)]
pub struct Field {
    #[use_normal_rw]
    pub access: FieldFlags,
    pub name: Cow<'static, str>,
    pub descriptor: Type,
    #[vec_len_type(u16)]
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

#[derive(PartialEq, Debug, Clone, ConstantPoolReadWrite)]
pub struct Method {
    #[use_normal_rw]
    pub access: MethodFlags,
    pub name: Cow<'static, str>,
    pub descriptor: Type,
    #[vec_len_type(u16)]
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

#[repr(transparent)]
#[derive(Eq, PartialEq, Debug, Clone, ConstantPoolReadWrite)]
pub struct Package(#[str_type(Package)] pub Cow<'static, str>);

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct NameAndType(pub Cow<'static, str>, pub Type);

impl ConstantPoolReadWrite for Option<(Cow<'static,str>, Type)> {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self, Error> {
        let idx = u16::read_from(reader)?;
        if idx == 0 {
            Ok(None)
        } else {
            try_cp_read!(idx, cp.read_nameandtype(idx)).map(Some)
        }
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> Result<(), Error> {
        if let Some((n, t)) = self {
            cp.insert_nameandtype(n.clone(), t.to_string()).write_to(writer)
        } else {
            0u16.write_to(writer)
        }
    }
}

#[derive(ConstantPoolReadWrite)]
#[attr_enum]
enum RawClassAttribute {
    Signature(ClassSignature),
    Synthetic, Deprecated, SourceFile(Cow<'static, str>), InnerClasses(#[vec_len_type(u16)] Vec<InnerClass>),
    EnclosingMethod(Clazz, Option<(Cow<'static,str>, Type)>), // SourceDebugExtension(Cow<'static, str>),
    BootstrapMethods(#[vec_len_type(u16)] Vec<BootstrapMethod>), Module(Module), ModulePackages(#[vec_len_type(u16)] Vec<Package>), ModuleMainClass(Clazz),
    NestHost(Clazz), NestMembers(#[vec_len_type(u16)] Vec<Clazz>),
    #[raw_variant]
    Raw(RawAttribute)
}

#[derive(PartialEq, Debug, Clone)]
pub enum ClassAttribute {
    Signature(ClassSignature),
    Synthetic, Deprecated, SourceFile(Cow<'static, str>), InnerClasses(Vec<InnerClass>),
    EnclosingMethod(Cow<'static, str>, Option<(Cow<'static, str>, Type)>), SourceDebugExtension(Cow<'static, str>),
    BootstrapMethods(Vec<BootstrapMethod>), Module(Module), ModulePackages(Vec<Cow<'static, str>>), ModuleMainClass(Cow<'static, str>),
    NestHost(Cow<'static, str>), NestMembers(Vec<Cow<'static, str>>), Raw(RawAttribute)
}
impl ConstantPoolReadWrite for ClassAttribute {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self, Error> {
        let raw = RawClassAttribute::read_from(cp, reader)?;
        macro_rules! generate_matches {
            (($($i:ident$(($($p:ident),*))?),*),($($MatchVariant:ident($($MatchField:ident),*) => $MatchBody:expr)*)) => ({
                match raw {
                    $(RawClassAttribute::$i$(($($p),*))? => ClassAttribute::$i$(($($p),*))?,)*
                    $(RawClassAttribute::$MatchVariant($($MatchField),*) => $MatchBody,)*
                }
            });
        }
        Ok(generate_matches!((Signature(c), Synthetic, Deprecated, SourceFile(s), Module(m), InnerClasses(i), BootstrapMethods(b)), (
            Raw(r) =>
                if r.name == "SourceDebugExtension" {
                    ClassAttribute::SourceDebugExtension(crate::mod_utf8::modified_utf8_to_string(r.inner.as_ref())?.into())
                } else {
                    ClassAttribute::Raw(r)
                }
            EnclosingMethod(c, nt) => ClassAttribute::EnclosingMethod(c.0, nt)
            ModulePackages(p) => ClassAttribute::ModulePackages(unsafe { std::mem::transmute(p) })
            NestMembers(n) => ClassAttribute::NestMembers(unsafe { std::mem::transmute(n) })
            ModuleMainClass(m) => ClassAttribute::ModuleMainClass(m.0)
            NestHost(n) => ClassAttribute::NestHost(n.0)
            )))
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> Result<(), Error> {
        macro_rules! generate_matches {
            ($($Variant:ident$(($($Field:ident),*))? => {
                [$($ReadWriteExpressions:expr),*];
                $($Statements:stmt)*
            })*) => {
                match self {
                    $(ClassAttribute::$Variant$(($($Field),*))? => {
                        let mut cursor = std::io::Cursor::new(Vec::new());
                        #[allow(unused)]
                        {
                            let writer = &mut cursor;
                            $(
                                ConstantPoolReadWrite::write_to($ReadWriteExpressions, cp, writer)?;
                            )*
                            $(
                                $Statements
                            )*
                        }
                        cp.insert_utf8(stringify!($Variant)).write_to(writer)?;
                        let vec = cursor.into_inner();
                        write_to!(&(vec.len() as u32), writer)?;
                        writer.write_all(&vec)?;
                    })*
                    ClassAttribute::Raw(r) => {
                        if r.keep {
                            cp.insert_utf8(r.name.clone()).write_to(writer)?;
                            (r.inner.len() as u32).write_to(writer)?;
                            writer.write_all(r.inner.as_ref())?;
                        }
                    }
                }
            };
        }
        generate_matches! {
            Signature(s) => { [s]; }
            Synthetic => { []; }
            Deprecated => { []; }
            SourceFile(s) => { [s]; }
            Module(m) => { [m]; }
            InnerClasses(i) => { [];
                (i.len() as u16).write_to(writer)?
                for c in i.iter() {
                    c.write_to(cp, writer)?;
                }
            }
            EnclosingMethod(c, nt) => {
                [unsafe { &*(c as *const Cow<str> as *const Clazz) }, nt];
            }
            SourceDebugExtension(s) => { [];
                write_to!(s, writer)?;
            }
            BootstrapMethods(b) => { [];
                (b.len() as u16).write_to(writer)?
                for m in b.iter() {
                    m.write_to(cp, writer)?;
                }
            }
            ModulePackages(p) => { [];
                (p.len() as u16).write_to(writer)?;
                for mp in p.iter() {
                    cp.insert_package(mp.clone()).write_to(writer)?;
                }
            }
            ModuleMainClass(m) => { [unsafe { &*(m as *const Cow<str> as *const Clazz) }]; }
            NestHost(n) => { [unsafe { &*(n as *const Cow<str> as *const Clazz) }]; }
            NestMembers(nm) => { [];
                (nm.len() as u16).write_to(writer)?;
                for m in nm.iter() {
                    cp.insert_class(m.clone()).write_to(writer)?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct BootstrapMethod {
    pub handle: MethodHandle,
    pub arguments: Vec<OrDynamic<Constant>>
}

impl ConstantPoolReadWrite for BootstrapMethod {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self, Error> {
        let handle = try_cp_read!(cp, reader, read_method_handle)?;
        let num_arguments = u16::read_from(reader)?;
        let mut arguments = Vec::with_capacity(num_arguments as usize);
        for _ in 0..num_arguments {
            let idx = u16::read_from(reader)?;
            arguments.push(try_cp_read!(idx, cp.read_or_dynamic(idx, ConstantPoolReader::read_constant))?)
        }
        Ok(BootstrapMethod {
            handle, arguments
        })
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> Result<(), Error> {
        cp.insert_method_handle(self.handle.clone()).write_to(writer)?;
        (self.arguments.len() as u16).write_to(writer)?;
        for arg in self.arguments.iter().cloned() {
            cp.insert_ordynamic(arg, ConstantPoolWriter::insert_constant).write_to(writer)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
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

#[derive(ConstantPoolReadWrite)]
struct ClassWrapper {
    #[use_normal_rw]
    pub access: ClassFlags,
    #[str_type(Class)]
    pub name: Cow<'static, str>,
    #[str_optional]
    #[str_type(Class)]
    pub super_name: Option<Cow<'static, str>>,
    #[vec_len_type(u16)]
    pub interfaces: Vec<Clazz>,
    #[vec_len_type(u16)]
    pub fields: Vec<Field>,
    #[vec_len_type(u16)]
    pub methods: Vec<Method>,
    #[vec_len_type(u16)]
    pub attributes: Vec<ClassAttribute>
}

impl ReadWrite for Class {
    fn read_from<T: Read>(reader: &mut T) -> Result<Self, Error> {
        match u32::read_from(reader)? {
            0xCAFEBABE => {
                let ver = JavaVersion::read_from(reader)?;

                unimplemented!()
            }
            n => Err(Error::Invalid("class header", n.to_string().into()))
        }
    }

    fn write_to<T: Write>(&self, writer: &mut T) -> Result<(), Error> {
        unimplemented!()
    }
}