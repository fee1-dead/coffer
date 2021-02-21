
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
pub use code::*;

use crate::{ConstantPoolReader, ConstantPoolReadWrite, ConstantPoolWriter, Error, ReadWrite, Result};
use crate::flags::{FieldFlags, InnerClassFlags, MethodFlags, MethodParameterFlags, ModuleFlags, RequireFlags};
use crate::full::annotation::{AnnotationValue, FieldTypeAnnotation, MethodTypeAnnotation};
use crate::prelude::*;

pub mod annotation;

pub mod cp;
mod code;


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