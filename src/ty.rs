//! A type represents a field descriptor or a method descriptor.

use std::fmt::{Display, Formatter};

use wtf_8::{Wtf8Str, Wtf8String};

use crate::prelude::*;

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
    Ref(Cow<'static, Wtf8Str>),
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
        S: Into<Cow<'static, Wtf8Str>>,
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

pub fn parse_type(s: &Wtf8Str) -> Result<Type> {
    fn get_type(c: &mut wtf_8::Wtf8CodePoints) -> Result<Type, crate::error::Error> {
        let next_codepoint = c.next();
        Ok(match next_codepoint.map(|c| c.to_char_lossy()) {
            Some('B') => Type::Byte,
            Some('C') => Type::Char,
            Some('D') => Type::Double,
            Some('F') => Type::Float,
            Some('I') => Type::Int,
            Some('J') => Type::Long,
            Some('Z') => Type::Boolean,
            Some('S') => Type::Short,
            Some('L') => {
                let mut st = Wtf8String::new();
                while c.as_str().codepoints().next().unwrap_or_else(|| ';'.into()) != ';' {
                    st.push(c.next().unwrap())
                }
                if c.next().is_none() {
                    return Err(unexpected_end());
                } else {
                    Type::Ref(Cow::Owned(st))
                }
            }
            Some('[') => {
                let mut dim: u8 = 1;
                while Some('['.into()) == c.as_str().codepoints().next() {
                    c.next();
                    dim += 1;
                }
                let r = get_type(c)?;
                Type::ArrayRef(dim, Box::new(r))
            }
            Some('(') => {
                let mut types = Vec::new();
                while c.as_str().codepoints().next().unwrap_or_else(|| ')'.into()) != ')' {
                    types.push(get_type(c)?)
                }
                if c.next().is_none() {
                    return Err(unexpected_end());
                } else {
                    Type::Method {
                        parameters: types,
                        ret: if Some('V'.into()) == c.as_str().codepoints().next() {
                            None
                        } else {
                            Some(Box::new(get_type(c)?))
                        },
                    }
                }
            }
            Some(ch) => {
                return Err(crate::error::Error::Invalid(
                    "type character",
                    format!("{ch:?}").into(),
                ))
            }
            None => return Err(unexpected_end()),
        })
    }
    get_type(&mut s.codepoints())
}

impl ConstantPoolReadWrite for Cow<'static, Wtf8Str> {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self> {
        let idx = ReadWrite::read_from(reader)?;
        cp.read_wtf8(idx)
            .ok_or_else(|| {
                crate::error::Error::Invalid("constant pool entry index", idx.to_string().into())
            })
            .map(Into::into)
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> Result<()> {
        cp.insert_wtf8(self.clone()).write_to(writer)
    }
}

impl ConstantPoolReadWrite for Option<Cow<'static, Wtf8Str>> {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self> {
        let idx = ReadWrite::read_from(reader)?;
        if idx == 0 {
            Ok(None)
        } else {
            cp.read_wtf8(idx)
                .ok_or_else(|| {
                    crate::error::Error::Invalid(
                        "constant pool entry index",
                        idx.to_string().into(),
                    )
                })
                .map(Into::into)
        }
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> Result<()> {
        if let Some(this) = self {
            cp.insert_wtf8(this.clone()).write_to(writer)
        } else {
            0u16.write_to(writer)
        }
    }
}

impl ConstantPoolReadWrite for Type {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self> {
        parse_type(&crate::try_cp_read!(cp, reader, read_wtf8)?)
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> Result<()> {
        cp.insert_wtf8(Cow::Owned(self.into())).write_to(writer)
    }
}

impl<'a> From<&'a Type> for Wtf8String {
    fn from(t: &'a Type) -> Self {
        let mut s = Wtf8String::new();
        write_type(t, &mut s);
        s
    }
}

pub fn write_type(t: &Type, s: &mut Wtf8String) {
    match t {
        Type::Byte => s.push_char('B'),
        Type::Char => s.push_char('C'),
        Type::Double => s.push_char('D'),
        Type::Float => s.push_char('F'),
        Type::Int => s.push_char('I'),
        Type::Long => s.push_char('J'),
        Type::Boolean => s.push_char('Z'),
        Type::Short => s.push_char('S'),
        Type::Ref(t) => {
            s.push_char('L');
            s.push_wtf8(t);
            s.push_char(';');
        }
        Type::ArrayRef(dim, t) => {
            for _ in 0..*dim {
                s.push_char('[');
            }
            write_type(t, s);
        }
        Type::Method {
            parameters: params,
            ret,
        } => {
            s.push_char('(');
            for t in params {
                write_type(t, s);
            }
            s.push_char(')');
            if let Some(ref t) = ret {
                write_type(t, s);
            } else {
                s.push_char('V');
            }
        }
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
                write!(f, "L{};", s.to_string_lossy())
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
