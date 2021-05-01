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
//! A type represents a field descriptor or a method descriptor.

use crate::prelude::*;
use std::fmt::{Display, Formatter};
use std::str::FromStr;

/// A descriptor, field or method.
#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub enum Type {
    /// A field descriptor representing the primitive `byte` (8-bit integer).
    Byte,
    /// A field descriptor representing the primitive `char` (UTF-16 code point).
    Char,
    /// A field descriptor representing the primitive `double` (double-precision floating point number).
    Double,
    /// A field descriptor representing the primitive `float` (single-precision floating point number).
    Float,
    /// A field descriptor representing the primitive `int` (32-bit integer).
    Int,
    /// A field descriptor representing the primitive `long` (64-bit integer).
    Long,
    /// A field descriptor representing the primitive `boolean`.
    Boolean,
    /// A field descriptor representing the primitive `short` (16-bit integer).
    Short,
    /// A field descriptor representing a java type.
    Ref(Cow<'static, str>),
    /// A field descriptor representing an array type.
    ArrayRef(u8, Box<Type>),
    /// The Method type.
    ///
    /// It is invalid if any of the parameter types and the return type is a method type.
    Method {
        /// The parameters of the method type.
        parameters: Vec<Type>,
        /// The return type of the method. If this is `None`, it represents `void`.
        ret: Option<Box<Type>>,
    },
}

impl Type {
    /// returns `true` if this type is `Long` or `Double`.
    #[inline]
    pub fn is_wide(&self) -> bool {
        matches!(self, Type::Long | Type::Double)
    }

    /// returns `true` if this type is a method descriptor.
    #[inline]
    pub fn is_method(&self) -> bool {
        matches!(self, Type::Method { .. })
    }

    /// Creates a new method descriptor with parameters and return type.
    #[inline]
    pub fn method<P: Into<Vec<Type>>>(params: P, ret: Option<Type>) -> Type {
        Type::Method {
            parameters: params.into(),
            ret: ret.map(Box::new),
        }
    }
    /// Creates a new reference type with a full-qualified name to the type.
    #[inline]
    pub fn reference<S>(str: S) -> Type
    where
        S: Into<Cow<'static, str>>,
    {
        Type::Ref(str.into())
    }
    /// Creates a new array field descriptor. If the underlying type is array, then the dimension is added to the original dimension and the original type is returned.
    pub fn array(dim: u8, mut t: Type) -> Type {
        match &mut t {
            Type::ArrayRef(ref mut orig_dim, ..) => {
                *orig_dim += dim;
                t
            }
            _ => Type::ArrayRef(dim, Box::new(t)),
        }
    }
}

impl From<Type> for Cow<'static, str> {
    fn from(t: Type) -> Self {
        match t {
            Type::Byte => "B".into(),
            Type::Char => "C".into(),
            Type::Double => "D".into(),
            Type::Float => "F".into(),
            Type::Int => "I".into(),
            Type::Long => "J".into(),
            Type::Boolean => "Z".into(),
            Type::Short => "S".into(),
            Type::Ref(t) => format!("L{};", t).into(),
            Type::ArrayRef(dim, t) => format!("{}{}", "[".repeat(dim as usize), t).into(),
            Type::Method {
                parameters: ty,
                ret,
            } => {
                let mut s = String::from('(');
                for t in ty {
                    s.push_str(Cow::from(t).as_ref());
                }
                s.push(')');
                if let Some(t) = ret {
                    s.push_str(Cow::from(*t).as_ref());
                } else {
                    s.push('V');
                }
                s.into()
            }
        }
    }
}

impl FromStr for Type {
    type Err = crate::error::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        fn get_type(c: &mut std::str::Chars, st: &str) -> Result<Type, crate::error::Error> {
            let next_char = c.next();
            Ok(match next_char {
                Some('B') => Type::Byte,
                Some('C') => Type::Char,
                Some('D') => Type::Double,
                Some('F') => Type::Float,
                Some('I') => Type::Int,
                Some('J') => Type::Long,
                Some('Z') => Type::Boolean,
                Some('S') => Type::Short,
                Some('L') => {
                    let mut st = String::new();
                    while c.as_str().chars().next().unwrap_or(';') != ';' {
                        st.push(c.next().unwrap())
                    }
                    if c.next().is_none() {
                        return unexpected_end();
                    } else {
                        Type::Ref(Cow::Owned(st))
                    }
                }
                Some('[') => {
                    let mut dim: u8 = 1;
                    while let Some('[') = c.as_str().chars().next() {
                        c.next();
                        dim += 1;
                    }
                    let r = get_type(c, st)?;
                    Type::ArrayRef(dim, Box::new(r))
                }
                Some('(') => {
                    let mut types = Vec::new();
                    while c.as_str().chars().next().unwrap_or(')') != ')' {
                        types.push(get_type(c, st)?)
                    }
                    if c.next().is_none() {
                        return unexpected_end();
                    } else {
                        Type::Method {
                            parameters: types,
                            ret: if let Some('V') = c.as_str().chars().next() {
                                None
                            } else {
                                Some(Box::new(get_type(c, st)?))
                            },
                        }
                    }
                }
                Some(ch) => {
                    return Err(crate::error::Error::Invalid(
                        "type character",
                        ch.to_string().into(),
                    ))
                }
                None => return unexpected_end(),
            })
        }
        get_type(&mut s.chars(), s)
    }
}
impl ConstantPoolReadWrite for Cow<'static, str> {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self> {
        let idx = ReadWrite::read_from(reader)?;
        cp.read_utf8(idx)
            .ok_or_else(|| {
                crate::error::Error::Invalid("constant pool entry index", idx.to_string().into())
            })
            .map(Into::into)
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> Result<()> {
        cp.insert_utf8(self.clone()).write_to(writer)
    }
}
impl ConstantPoolReadWrite for Type {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self> {
        crate::try_cp_read!(cp, reader, read_utf8)?.parse()
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> Result<()> {
        cp.insert_utf8(self.to_string()).write_to(writer)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        use std::fmt::Write;
        match self {
            Type::Byte => f.write_char('B'),
            Type::Char => f.write_char('C'),
            Type::Double => f.write_char('D'),
            Type::Float => f.write_char('F'),
            Type::Int => f.write_char('I'),
            Type::Long => f.write_char('J'),
            Type::Boolean => f.write_char('Z'),
            Type::Short => f.write_char('S'),
            Type::Ref(s) => {
                write!(f, "L{};", s)
            }
            Type::ArrayRef(dim, t) => {
                "[".repeat(*dim as usize).fmt(f)?;
                t.fmt(f)
            }
            Type::Method {
                parameters: params,
                ret,
            } => {
                f.write_char('(')?;
                for t in params {
                    t.fmt(f)?;
                }
                f.write_char(')')?;
                if let Some(ref t) = ret {
                    t.fmt(f)?;
                } else {
                    f.write_char('V')?;
                }
                Ok(())
            }
        }
    }
}
