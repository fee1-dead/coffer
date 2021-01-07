pub(crate) mod type_annotation;
pub use type_annotation::*;

use std::borrow::Cow;
use std::collections::HashMap;
use super::Type;
use crate::{ConstantPoolReadWrite, ConstantPoolReader, ConstantPoolWriter, Read, Write, ReadWrite, Result, read_from, write_to};
use crate::error::Error;
use std::str::FromStr;
use std::convert::TryInto;

/// Some values actually becomes ints in the constant pool.
#[derive(Clone, PartialEq, Debug)]
pub enum AnnotationValue {
    Byte(i8),
    /// This is a UTF-16 codepoint possibly being a surrougate value. When converting from `char` in rust make sure to check if the length in utf-16 can be 2 or more.
    Char(u16),
    Double(f64),
    Float(f32),
    Int(i32),
    Long(i64),
    Short(i16),
    Boolean(bool),
    String(Cow<'static, str>),
    Enum(Type, Cow<'static, str>),
    /// None = void.class
    Class(Option<Type>),
    Annotation(Annotation),
    Array(Vec<AnnotationValue>)
}

impl ConstantPoolReadWrite for AnnotationValue {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self> {
        let tag = u8::read_from(reader)? as char;
        Ok(match tag {
            'B' => AnnotationValue::Byte({
                let val: i32 = read_from!(cp, reader)?;
                val.try_into().map_err(|_| crate::error::Error::Invalid("byte annotation value", val.to_string().into()))?
            }),
            'C' => AnnotationValue::Char({
                let val: i32 = read_from!(cp, reader)?;
                val.try_into().map_err(|_| crate::error::Error::Invalid("char annotation value", val.to_string().into()))?
            }),
            'D' => AnnotationValue::Double(read_from!(cp, reader)?),
            'F' => AnnotationValue::Float(read_from!(cp, reader)?),
            'I' => AnnotationValue::Int(read_from!(cp, reader)?),
            'J' => AnnotationValue::Long(read_from!(cp, reader)?),
            'S' => AnnotationValue::Short({
                let val: i32 = read_from!(cp, reader)?;
                val.try_into().map_err(|_| crate::error::Error::Invalid("short annotation value", val.to_string().into()))?
            }),
            'Z' => AnnotationValue::Boolean(match read_from!(cp, reader)? {
                1 => true,
                0 => false,
                i => return Err(crate::error::Error::Invalid("bool annotation value", i.to_string().into()))
            }),
            's' => AnnotationValue::String(read_from!(cp, reader)?),
            'e' => AnnotationValue::Enum(Type::read_from(cp, reader)?, read_from!(cp, reader)?),
            'c' => {
                let idx = u16::read_from(reader)?;
                let str = cp.read_indirect_str(7, idx).ok_or_else(|| crate::error::Error::Invalid("constant pool entry index", idx.to_string().into()))?;
                AnnotationValue::Class(if str == "V" { None } else { Some(Type::from_str(str.as_ref())?) })
            }
            '@' => AnnotationValue::Annotation(Annotation::read_from(cp, reader)?),
            '[' => {
                let num = u16::read_from(reader)?;
                let mut values = Vec::with_capacity(num as usize);
                for _ in 0..num {
                    values.push(AnnotationValue::read_from(cp, reader)?)
                }
                AnnotationValue::Array(values)
            }
            _ => { return Err(Error::Invalid("Enum value type", tag.to_string().into())) }
        })
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> Result<()> {
        fn cp_int<C: ConstantPoolWriter, W: Write, I: Into<i32>>(cp: &mut C, writer: &mut W, val: I) -> Result<()> {
            write_to!(&val.into(), cp, writer)
        }
        match self {
            AnnotationValue::Byte(val) => {
                b'B'.write_to(writer)?;
                cp_int(cp, writer, *val)
            }
            AnnotationValue::Char(val) => {
                b'C'.write_to(writer)?;
                cp_int(cp, writer, *val)
            }
            AnnotationValue::Double(val) => {
                b'D'.write_to(writer)?;
                write_to!(val, cp, writer)
            }
            AnnotationValue::Float(val) => {
                b'F'.write_to(writer)?;
                write_to!(val, cp, writer)
            }
            AnnotationValue::Int(val) => {
                b'I'.write_to(writer)?;
                write_to!(val, cp, writer)
            }
            AnnotationValue::Long(val) => {
                b'J'.write_to(writer)?;
                write_to!(val, cp, writer)
            }
            AnnotationValue::Short(val) => {
                b'S'.write_to(writer)?;
                cp_int(cp, writer, *val)
            }
            AnnotationValue::Boolean(val) => {
                b'Z'.write_to(writer)?;
                cp_int(cp, writer, *val)
            }
            AnnotationValue::String(s) => {
                b's'.write_to(writer)?;
                write_to!(s, cp, writer)
            }
            AnnotationValue::Enum(ty, var) => {
                b'e'.write_to(writer)?;
                ty.write_to(cp, writer)?;
                write_to!(var, cp, writer)
            }
            AnnotationValue::Class(n) => {
                b'C'.write_to(writer)?;
                write_to!(&n.as_ref().map_or_else(|| Cow::Borrowed("V"), |t| t.to_string().into()), cp, writer)
            }
            AnnotationValue::Annotation(a) => {
                b'@'.write_to(writer)?;
                a.write_to(cp, writer)
            }
            AnnotationValue::Array(v) => {
                b'['.write_to(writer)?;
                (v.len() as u16).write_to(writer)?;
                for e in v {
                    e.write_to(cp, writer)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Annotation {
    pub annotation_type: Type,
    pub element_values: HashMap<Cow<'static, str>, AnnotationValue>
}

impl ConstantPoolReadWrite for Annotation {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self> {
        let ty = Type::read_from(cp, reader)?;
        let pairs = u16::read_from(reader)?;
        let mut element_values = HashMap::with_capacity(pairs as usize);
        for _ in 0..pairs {
            let name = read_from!(cp, reader)?;
            let value = AnnotationValue::read_from(cp, reader)?;
            element_values.insert(name, value);
        }
        Ok(Annotation { annotation_type: ty, element_values })
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> Result<()> {
        self.annotation_type.write_to(cp, writer)?;
        let size = self.element_values.len() as u16;
        size.write_to(writer)?;
        for (k, e) in &self.element_values {
            write_to!(k, cp, writer)?;
            e.write_to(cp, writer)?;
        }
        Ok(())
    }
}