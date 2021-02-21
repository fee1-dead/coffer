/*
 *     This file is part of Coffer.
 *
 *     Coffer is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     Coffer is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with Coffer. (LICENSE.md)  If not, see <https://www.gnu.org/licenses/>.
 */
use crate::full::*;
use crate::module::Module;
use crate::prelude::*;

/// A unrecognized, unknown raw attribute.
#[derive(Clone, PartialEq, Debug)]
pub struct RawAttribute {
    /// Whether to keep this attribute upon writing.
    /// Attributes that are related to local variables will default to `false`, whereas newly created attributes will be `true`.
    pub(crate) keep: bool,
    /// The name of this attribute.
    pub name: Cow<'static, str>,
    /// The inner data of this attribute.
    pub inner: Cow<'static, [u8]>
}

impl RawAttribute {
    /// Creates a new raw attribute with name and inner data.
    ///
    /// `String` and `Vec<u8>`, string literal and array literal are all accepted because this uses a Cow.
    pub fn new<S: Into<Cow<'static, str>>, B: Into<Cow<'static, [u8]>>>(name: S, inner: B) -> Self {
        Self {
            keep: true,
            name: name.into(),
            inner: inner.into()
        }
    }
    /// Used by the procedural macro.
    pub(crate) fn __new(name: Cow<'static, str>, inner: Vec<u8>) -> Self {
        Self {
            keep: false,
            name,
            inner: Cow::Owned(inner)
        }
    }
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