//! In java, signatures are additional information encoded for type arguments in classes and methods.
//!
//! It also reports the bounds or the explicit type for type arguments of a field. For example, the signatures of these fields are recorded:
//!
//! ```ignore
//! SomeGenericClass<T> field1 = ...; // referring type parameter of the class holding this field
//! SomeGenericClass<Foo> field2 = ...; // Explicit type parameter
//! SomeGenericClass<? implements AInterface> // type bound on type parameter
//! ```
use std::fmt::{Display, Formatter, Write};
use std::borrow::Cow;
use std::str::FromStr;

/// A type signature represents either a reference type or a primitive type.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeSignature {
    /// A type signature representing the primitive `byte`. In Java, `byte`s are signed 8-bit integers.
    Byte,
    /// A type signature representing the primitive `char`. In Java, `char`s are UTF-16 code points.
    Char,
    /// A type signature representing the primitive `double`. In Java, `double`s are double-precision floating-point numbers (64-bit).
    Double,
    /// A type signature representing the primitive `float`. In Java, `float`s are single-precision floating-point numbers (32-bit).
    Float,
    /// A type signature representing the primitive `int`. In Java, `int`s are signed 32-bit integers.
    Int,
    /// A type signature representing the primitive `long`. In Java, `long`s are signed 64-bit integers.
    Long,
    /// A type signature representing the primitive `boolean`.
    Boolean,
    /// A type signature representing the primitive `short`. In Java, `byte`s are signed 16-bit integers.
    Short,
    /// A type signature representing a reference.
    Ref(RefTypeSignature)
}

/// Signature for a field. It must be a reference type as primitive types do not have type parameters.
#[repr(transparent)]
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FieldSignature(RefTypeSignature);

/// Signature for classes.
///
/// It contains information about its type parameters (bounds), super class type parameters (bounds), interface type parameters (bounds).
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ClassSignature {
    /// The type parameters for this class.
    pub type_parameters: Vec<TypeParameter>,
    /// The signature for the super class.
    pub super_class: ClassTypeSignature,
    /// Signature for interfaces/
    pub interfaces: Vec<ClassTypeSignature>
}

/// Signature for methods.
///
/// It contains information about its type parameters, arguments, return type, and exceptions.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MethodSignature {
    /// Signature for type parameters of this method.
    pub type_parameters: Vec<TypeParameter>,
    /// Information about the parameters/arguments of this method.
    pub parameters: Vec<TypeSignature>,
    /// Information about the return type of this method.
    pub return_type: Option<TypeSignature>,
    /// Information about the exceptions of this method.
    pub throws: Vec<Throws>
}

/// A type parameter for a class.
///
/// A type parameter may have a class bound and some interface bounds.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypeParameter {
    /// The name of the type parameter. Most of the time it is `T`.
    pub name: Cow<'static, str>,
    /// The class bound of the type parameter.
    ///
    /// Specifically, that this type parameter must be a child of the bound. If `None`, then there is no restriction.
    pub class_bound: Option<RefTypeSignature>,
    /// Interface bounds restrict that types must implement `I` for `I` in the list of interface bounds.
    pub interface_bounds: Vec<RefTypeSignature>
}

/// Signature for exception that a method can throw.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Throws {
    /// Throws a type parameter.
    TypeParameter(Cow<'static, str>),
    /// Throws an explicit class.
    Class(ClassTypeSignature)
}

/// A type argument.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeArgument {
    /// Specifies that the type argument is some type extending this type.
    Extends(RefTypeSignature),
    /// Specifies that the type argument is a parent of this type.
    Super(RefTypeSignature),
    /// Specifies that the type argument is exactly this type.
    Exact(RefTypeSignature),
    /// Specifies that the type argument can be any type.
    Any
}

/// A simple class type signature. Has the simple name and type arguments.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SimpleClassTypeSignature {
    /// The simple name of this class.
    pub name: Cow<'static, str>,
    /// The type arguments for this class.
    pub type_arguments: Vec<TypeArgument>
}

/// A class type signature, contains package directives, a simple type signature, and suffixes (inner classes).
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ClassTypeSignature {
    /// The package directives for this class.
    pub package: Vec<Cow<'static, str>>,
    /// The simple type signature of this class.
    pub name: SimpleClassTypeSignature,
    /// Inner classes names with arguments.
    pub suffix: Vec<SimpleClassTypeSignature>
}

/// A reference type.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum RefTypeSignature {
    /// References a type variable with name.
    TypeVariable(Cow<'static, str>),
    /// References an array type.
    ArrayRef(u8, Box<TypeSignature>),
    /// References a class type.
    ClassType(ClassTypeSignature)
}

// Implementations

#[inline]
pub(super) fn unexpected_end<T>() -> crate::Result<T> {
    Err(std::io::Error::new(std::io::ErrorKind::InvalidData, "Unexpected end of string").into())
}

use nom::{take, one_of, IResult, take_until1, many0, char, complete, peek, alt, take_till1, do_parse, terminated, switch, opt};
use crate::ConstantPoolReadWrite;

fn type_sig(i: &str) -> IResult<&str, TypeSignature> {
    let (i, c) = alt!(i, one_of!("BCDFIJSZ") | peek!(one_of!("[TL")))?;
    Ok((i, match c {
        'B' => TypeSignature::Byte,
        'C' => TypeSignature::Char,
        'D' => TypeSignature::Double,
        'F' => TypeSignature::Float,
        'I' => TypeSignature::Int,
        'J' => TypeSignature::Long,
        'S' => TypeSignature::Short,
        'Z' => TypeSignature::Boolean,
        _ => { return ref_type_sig(i).map(|(i, r)| (i, TypeSignature::Ref(r))) }
    }))
}
fn ref_type_sig(i: &str) -> IResult<&str, RefTypeSignature> {
    let (newi, c) = one_of!(i, "[TL")?;
    match c {
        '[' => {
            let (i, o) = many0!(newi, char!('['))?;
            let dim = o.len() + 1;
            let (i, t) = type_sig(i)?;
            Ok((i, RefTypeSignature::ArrayRef(dim as u8, Box::new(t))))
        }
        'T' => {
            let (i, o) = take_until1!(newi, ";")?;
            let (i, _) = take!(i, 1)?;
            Ok((i, RefTypeSignature::TypeVariable(o.to_owned().into())))
        }
        'L' => {
            let (i, sig) = class_type_sig(i)?;
            Ok((i, RefTypeSignature::ClassType(sig)))
        }
        _ => { unsafe { std::hint::unreachable_unchecked() } }
    }
}

fn throws(i: &str) -> IResult<&str, Throws> {
    do_parse!(i,
        char!('^') >>
        res: switch!(peek!(take!(1)),
        "T" => do_parse!(char!('T') >>
            ty: take_until1!(";") >>
            take!(1) >>
            (Throws::TypeParameter(ty.to_owned().into()))) |
        "L" => do_parse!(sig: class_type_sig >> (Throws::Class(sig)))) >>
    (res))
}

// TODO Error when encountering illegal characters
fn class_type_sig(i: &str) -> IResult<&str, ClassTypeSignature> {
    println!("{}", i);
    do_parse!(i,
    char!('L') >>
    package: packages >>
    name: simple_type_sig >>
    suffix: terminated!(many0!(complete!(do_parse!(char!('.') >>
    sig: simple_type_sig >> (sig)))), char!(';')) >>
    (ClassTypeSignature { package, name, suffix }))
}

fn packages(i: &str) -> IResult<&str, Vec<Cow<'static, str>>> {
    let bytes = i.as_bytes();
    let mut vec = vec![];
    let mut n = 0;
    let mut str = Vec::new();
    let mut last = usize::MAX;
    loop {
        match bytes.get(n) {
            Some(b'/') => {
                unsafe { vec.push(Cow::Owned(String::from_utf8_unchecked(str))); } // the parameter is &str, so it must be valid utf-8.
                str = Vec::new();
                last = n;
            }
            Some(b';') | Some(b'<') | Some(b'.') => break if last == usize::MAX {
                    Ok((i, vec))
                } else {
                    Ok((&i[(last + 1)..i.len()], vec))
                },
            Some(b) => str.push(*b),
            None => break Err(nom::Err::Incomplete(nom::Needed::Unknown))
        }
        n += 1;
    }
}

fn type_arg(i: &str) -> IResult<&str, TypeArgument> {
    let (newi, o) = one_of!(i, "-+*TL[")?;
    Ok(match o {
        '*' => { (newi, TypeArgument::Any) }
        '-' => {
            let (i, o) = ref_type_sig(newi)?;
            (i, TypeArgument::Super(o))
        }
        '+' => {
            let (i, o) = ref_type_sig(newi)?;
            (i, TypeArgument::Extends(o))
        }
        _ => {
            let (i, o) = ref_type_sig(i)?;
            (i, TypeArgument::Exact(o))
        }
    })
}

fn type_args(i: &str) -> IResult<&str, Vec<TypeArgument>> {
    let (i, o) = opt!(i, complete!(do_parse!(char!('<') >> res: many0!(complete!(type_arg)) >> char!('>') >> (res))))?;
    Ok((i, o.unwrap_or_default()))
}

fn simple_type_sig(i: &str) -> IResult<&str, SimpleClassTypeSignature> {
    fn type_var_start(c: char) -> bool { c == '<' || c == '.' || c == ';' }
    do_parse!(i, name: take_till1!(type_var_start) >> type_arguments: type_args >> (SimpleClassTypeSignature { name: name.to_owned().into(), type_arguments }))
}

fn type_parameter(i: &str) -> IResult<&str, TypeParameter> {
    do_parse!(i, ident: take_until1!(":") >> char!(':') >> class_bound: opt!(ref_type_sig) >> interface_bounds: many0!(do_parse!(char!(':') >> res: ref_type_sig >> (res))) >> (TypeParameter { name: ident.to_owned().into(), class_bound, interface_bounds }))
}

fn type_parameters(i: &str) -> IResult<&str, Vec<TypeParameter>> {
    do_parse!(i, res: opt!(complete!(do_parse!(char!('<') >> res: many0!(type_parameter) >> char!('>') >> (res)))) >> (res.unwrap_or_default()))
}

fn method_sig(i: &str) -> IResult<&str, MethodSignature> {
    do_parse!(i, type_parameters: type_parameters >> char!('(') >> parameters: many0!(type_sig) >> char!(')') >> return_type: opt!(type_sig) >> throws: many0!(throws) >> (MethodSignature { type_parameters, parameters, return_type, throws }))
}

fn class_sig(i: &str) -> IResult<&str, ClassSignature> {
    do_parse!(i, type_parameters: type_parameters >> super_class: class_type_sig >> interfaces: many0!(class_type_sig) >> (ClassSignature { type_parameters, super_class, interfaces }))
}

macro_rules! convert_result {
    ($s: expr, $i:expr) => {
        match complete!($s, $i) {
            Ok((i, r)) => { if i.is_empty() { Ok(r) } else { unexpected_end() } }
            Err(e) => { Err(Box::<dyn std::error::Error>::from(e.to_string()).into()) }
        }
    };
}

macro_rules! fromstr_impls {
    ($($ty: ty, $fn: expr),*) => {
        $(
            impl FromStr for $ty {
                type Err = crate::error::Error;

                fn from_str(s: &str) -> Result<Self, Self::Err> {
                    convert_result!(s, $fn)
                }
            }
        )*
    };
}

macro_rules! cprw_impls {
    ($($ty: ty), *) => {
        $(
            impl ConstantPoolReadWrite for $ty {
                fn read_from<C: crate::ConstantPoolReader, R: std::io::Read>(cp: &mut C, reader: &mut R) -> crate::Result<Self> {
                    FromStr::from_str(Cow::<'static, str>::read_from(cp, reader)?.as_ref())
                }

                fn write_to<C: crate::ConstantPoolWriter, W: std::io::Write>(&self, cp: &mut C, writer: &mut W) -> crate::Result<()> {
                    Cow::<'static, str>::write_to(&self.to_string().into(), cp, writer)
                }
            }
        )*
    };
}

fromstr_impls!(
    SimpleClassTypeSignature, simple_type_sig,
    RefTypeSignature, ref_type_sig,
    ClassTypeSignature, class_type_sig,
    TypeSignature, type_sig,
    Throws, throws,
    TypeParameter, type_parameter,
    ClassSignature, class_sig,
    MethodSignature, method_sig
);

impl FromStr for FieldSignature {
    type Err = crate::error::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(FromStr::from_str(s)?))
    }
}

cprw_impls!(SimpleClassTypeSignature, RefTypeSignature, ClassTypeSignature, TypeSignature, Throws, TypeParameter, ClassSignature, MethodSignature, FieldSignature);

impl Display for FieldSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl Display for TypeSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeSignature::Byte => { f.write_char('B') }
            TypeSignature::Char => { f.write_char('C') }
            TypeSignature::Double => { f.write_char('D') }
            TypeSignature::Float => { f.write_char('F') }
            TypeSignature::Int => { f.write_char('I') }
            TypeSignature::Long => { f.write_char('J') }
            TypeSignature::Boolean => { f.write_char('Z') }
            TypeSignature::Short => { f.write_char('S') }
            TypeSignature::Ref(r) => { r.fmt(f) }
        }
    }
}

impl Display for TypeArgument {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeArgument::Extends(ref t) => {
                f.write_str("+")?;
                t.fmt(f)
            }
            TypeArgument::Super(ref t) => {
                f.write_str("-")?;
                t.fmt(f)
            }
            TypeArgument::Any => {
                f.write_str("*")
            }
            TypeArgument::Exact(ref t) => {
                t.fmt(f)
            }
        }
    }
}

impl Display for SimpleClassTypeSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.name.fmt(f)?;
        if !self.type_arguments.is_empty() {
            f.write_char('<')?;
            for t in &self.type_arguments {
                t.fmt(f)?;
            }
            f.write_char('>')?;
        }
        Ok(())
    }
}

impl Display for ClassTypeSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("L")?;
        for s in &self.package {
            s.fmt(f)?;
            f.write_str("/")?;
        }
        self.name.fmt(f)?;
        for sig in &self.suffix {
            f.write_str(".")?;
            sig.fmt(f)?;
        }
        f.write_str(";")
    }
}
impl Display for RefTypeSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RefTypeSignature::TypeVariable(name) => {
                write!(f, "T{};", name)
            }
            RefTypeSignature::ArrayRef(dim, t) => {
                "[".repeat(*dim as usize).fmt(f)?;
                t.fmt(f)
            }
            RefTypeSignature::ClassType(c) => {
                c.fmt(f)
            }
        }
    }
}

impl Display for Throws {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_char('^')?;
        match self {
            Throws::TypeParameter(p) => {
                write!(f, "L{};", p)
            }
            Throws::Class(c) => {
                c.fmt(f)
            }
        }
    }
}

impl Display for TypeParameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.name.fmt(f)?;
        f.write_char(':')?;
        if let Some(ref sig) = self.class_bound {
            sig.fmt(f)?;
        }
        for sig in &self.interface_bounds {
            f.write_char(':')?;
            sig.fmt(f)?;
        }
        Ok(())
    }
}

impl Display for MethodSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if !self.type_parameters.is_empty() {
            f.write_char('<')?;
            for par in &self.type_parameters {
                par.fmt(f)?;
            }
            f.write_char('>')?;
        }
        f.write_char('(')?;
        for sig in &self.parameters {
            sig.fmt(f)?;
        }
        f.write_char(')')?;
        if let Some(ref sig) = self.return_type {
            sig.fmt(f)?;
        } else {
            f.write_char('V')?;
        }
        for t in &self.throws {
            t.fmt(f)?;
        }
        Ok(())
    }
}

impl Display for ClassSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if !self.type_parameters.is_empty() {
            f.write_char('<')?;
            for par in &self.type_parameters {
                par.fmt(f)?;
            }
            f.write_char('>')?;
        }
        self.super_class.fmt(f)?;
        for sig in &self.interfaces {
            sig.fmt(f)?;
        }
        Ok(())
    }
}