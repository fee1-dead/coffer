//! In java, signatures are additional information encoded for type arguments in classes and methods.
//!
//! It also reports the bounds or the explicit type for type arguments of a field. For example, the signatures of these fields are recorded:
//!
//! ```java
//! SomeGenericClass<T> field1 = ...;
//! // ^ referring type parameter of the class holding this field
//! SomeGenericClass<Foo> field2 = ...; // Explicit type parameter
//! SomeGenericClass<? implements AInterface> // type bound on type parameter
//! ```
use std::borrow::Cow;
use std::fmt::{Display, Formatter, Write};
use std::str::FromStr;

use crate::Result;

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
    Ref(RefTypeSignature),
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
    pub interfaces: Vec<ClassTypeSignature>,
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
    pub throws: Vec<Throws>,
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
    pub interface_bounds: Vec<RefTypeSignature>,
}

/// Signature for exception that a method can throw.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Throws {
    /// Throws a type parameter.
    TypeParameter(Cow<'static, str>),
    /// Throws an explicit class.
    Class(ClassTypeSignature),
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
    Any,
}

/// A simple class type signature. Has the simple name and type arguments.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SimpleClassTypeSignature {
    /// The simple name of this class.
    pub name: Cow<'static, str>,
    /// The type arguments for this class.
    pub type_arguments: Vec<TypeArgument>,
}

/// A class type signature, contains package directives, a simple type signature, and suffixes (inner classes).
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ClassTypeSignature {
    /// The package directives for this class.
    pub package: Vec<Cow<'static, str>>,
    /// The simple type signature of this class.
    pub name: SimpleClassTypeSignature,
    /// Inner classes names with arguments.
    pub suffix: Vec<SimpleClassTypeSignature>,
}

/// A reference type.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum RefTypeSignature {
    /// References a type variable with name.
    TypeVariable(Cow<'static, str>),
    /// References an array type.
    ArrayRef(u8, Box<TypeSignature>),
    /// References a class type.
    ClassType(ClassTypeSignature),
}

// Implementations

#[inline]
pub(super) fn unexpected_end() -> crate::Error {
    crate::Error::Invalid("data", "unexpected end of string".into())
}

use crate::ConstantPoolReadWrite;

pub type IResult<'a, T> = crate::Result<(&'a str, T)>;

fn invalid(s: &'static str, x: impl Into<String>) -> crate::Error {
    crate::Error::Invalid(s, x.into().into())
}

fn type_sig(i: &str) -> IResult<'_, TypeSignature> {
    let o = match i.chars().next() {
        Some('B') => TypeSignature::Byte,
        Some('C') => TypeSignature::Char,
        Some('D') => TypeSignature::Double,
        Some('F') => TypeSignature::Float,
        Some('I') => TypeSignature::Int,
        Some('J') => TypeSignature::Long,
        Some('S') => TypeSignature::Short,
        Some('Z') => TypeSignature::Boolean,
        Some('T' | 'L' | '[') => return ref_type_sig(i).map(|(i, r)| (i, TypeSignature::Ref(r))),
        Some(c) => return Err(invalid("character for type signature", c)),
        None => return Err(unexpected_end()),
    };

    Ok((&i[1..], o))
}
fn ref_type_sig(i: &str) -> IResult<'_, RefTypeSignature> {
    match i.chars().next() {
        Some('[') => {
            let mut c = i.chars();
            let dim = c.by_ref().take_while(|&x| x == '[').count();
            // take_while takes the first non-`[` character, so we go back one character.
            c.next_back();
            let (i, t) = type_sig(c.as_str())?;
            Ok((i, RefTypeSignature::ArrayRef(dim as u8, Box::new(t))))
        }
        Some('T') => {
            let semi = i.find(';').ok_or_else(unexpected_end)?;
            let type_var = i[1..semi].to_string();
            Ok((
                &i[semi + 1..],
                RefTypeSignature::TypeVariable(type_var.into()),
            ))
        }
        Some('L') => {
            let (i, sig) = class_type_sig(i)?;
            Ok((i, RefTypeSignature::ClassType(sig)))
        }
        Some(c) => Err(invalid("character for referenced tyep signature", c)),
        None => Err(unexpected_end()),
    }
}

fn char<const C: char>(i: &str) -> IResult<'_, ()> {
    match i.chars().next() {
        Some(c) if c == C => Ok((&i[1..], ())),
        Some(other) => Err(invalid("character", format!("expected {C}, found {other}"))),
        None => Err(unexpected_end()),
    }
}

fn throws(i: &str) -> IResult<'_, Throws> {
    let (i, ()) = char::<'^'>(i)?;
    match i.chars().next() {
        Some('L') => {
            let (i, sig) = class_type_sig(i)?;
            Ok((i, Throws::Class(sig)))
        }
        Some('T') => {
            let semi = i.find(';').ok_or_else(unexpected_end)?;
            let type_var = i[1..semi].to_string();
            Ok((&i[semi + 1..], Throws::TypeParameter(type_var.into())))
        }
        Some(c) => Err(invalid("character for thrown type signature", c)),
        None => Err(unexpected_end()),
    }
}

// ClassTypeSignature:
// L [PackageSpecifier] SimpleClassTypeSignature {ClassTypeSignatureSuffix} ;
// ClassTypeSignatureSuffix:
// . SimpleClassTypeSignature
fn class_type_sig(i: &str) -> IResult<'_, ClassTypeSignature> {
    let (i, ()) = char::<'L'>(i)?;
    let (i, package) = packages(i)?;
    let (mut i, name) = simple_type_sig(i)?;
    let mut suffix = vec![];
    while i.as_bytes().get(0) == Some(&b'.') {
        // Safe way to do this because the nom macros looks confusing
        let res = simple_type_sig(&i[1..])?; // Skip the dot here
        i = res.0;
        suffix.push(res.1);
    }
    let (i, _) = char::<';'>(i)?;
    Ok((
        i,
        ClassTypeSignature {
            package,
            name,
            suffix,
        },
    ))
}

// PackageSpecifier:
// Identifier / {PackageSpecifier}
fn packages(i: &str) -> IResult<'_, Vec<Cow<'static, str>>> {
    let bytes = i.as_bytes();
    let mut vec = vec![];
    let mut n = 0;
    let mut str = Vec::new();
    // The index of the last slash.
    let mut last = usize::MAX;
    loop {
        match bytes.get(n) {
            // If there is a slash, it is a package directive. Push to the vec and clear out the last string.
            Some(b'/') => {
                unsafe {
                    vec.push(Cow::Owned(String::from_utf8_unchecked(str)));
                } // the parameter is &str, so it must be valid utf-8.
                str = Vec::new();
                last = n;
            }
            // These characters would mean that there is no more package directive and the string we've just built is used for other functions.
            Some(b';') | Some(b'<') | Some(b'.') => {
                break if last == usize::MAX {
                    // there were no slashes found so we just return the whole input string.
                    Ok((i, vec))
                } else {
                    // Returns everything after the last slash.
                    Ok((&i[(last + 1)..], vec))
                };
            }
            // Push this byte to build the string.
            Some(b) => str.push(*b),
            // The string ended but we are still parsing the packages!
            None => break Err(unexpected_end()),
        }
        n += 1;
    }
}

// TypeArgument:
// [WildcardIndicator] ReferenceTypeSignature
// *
// WildcardIndicator:
// +
// -
fn type_arg(i: &str) -> IResult<'_, TypeArgument> {
    let mut chars = i.chars();
    Ok(match chars.next() {
        Some('*') => (chars.as_str(), TypeArgument::Any),
        Some('-') => {
            let (i, o) = ref_type_sig(chars.as_str())?;
            (i, TypeArgument::Super(o))
        }
        Some('+') => {
            let (i, o) = ref_type_sig(chars.as_str())?;
            (i, TypeArgument::Extends(o))
        }
        // Use the old `i` to parse the exact type.
        Some('T' | 'L' | '[') => {
            let (i, o) = ref_type_sig(i)?;
            (i, TypeArgument::Exact(o))
        }
        Some(c) => {
            return Err(invalid(
                "type argument",
                format!("expected one of '*', '-', '+', 'T', 'L', '[, found {c}"),
            ))
        }
        None => return Err(unexpected_end()),
    })
}

fn type_args(mut i: &str) -> IResult<'_, Vec<TypeArgument>> {
    let mut args = vec![];
    if i.as_bytes().get(0) == Some(&b'<') {
        i = &i[1..];
        while !i.is_empty() && i.as_bytes().get(0) != Some(&b'>') {
            let res = type_arg(i)?;
            i = res.0;
            args.push(res.1);
        }
        let (i, _) = char::<'>'>(i)?;
        Ok((i, args))
    } else {
        Ok((i, Vec::new()))
    }
}

fn simple_type_sig(i: &str) -> IResult<'_, SimpleClassTypeSignature> {
    fn type_var_start(c: char) -> bool {
        c == '<' || c == '.' || c == ';'
    }
    let mut chars = i.chars();
    let name = chars
        .by_ref()
        .take_while(|&x| !type_var_start(x))
        .collect::<String>()
        .into();
    chars.next_back();
    let (i, type_arguments) = type_args(chars.as_str())?;
    Ok((
        i,
        SimpleClassTypeSignature {
            name,
            type_arguments,
        },
    ))
}

fn type_parameter(i: &str) -> IResult<'_, TypeParameter> {
    let mut chars = i.chars();
    let name = chars
        .by_ref()
        .take_while(|&x| x != ':')
        .collect::<String>()
        .into();
    let (mut i, class_bound) = if let Ok((i, class_bound)) = ref_type_sig(i) {
        (i, Some(class_bound))
    } else {
        (i, None)
    };
    let mut interface_bounds = vec![];
    while let Some(':') = chars.next() {
        let (new_i, bound) = ref_type_sig(i)?;
        interface_bounds.push(bound);
        i = new_i;
    }

    Ok((
        i,
        TypeParameter {
            name,
            class_bound,
            interface_bounds,
        },
    ))
}

fn type_parameters(mut i: &str) -> IResult<'_, Vec<TypeParameter>> {
    let mut params = vec![];
    if i.as_bytes().get(0) == Some(&b'<') {
        i = &i[1..];
        while !i.is_empty() && i.as_bytes().get(0) != Some(&b'>') {
            let res = type_parameter(i)?;
            i = res.0;
            params.push(res.1);
        }
        let (i, _) = char::<'>'>(i)?;
        Ok((i, params))
    } else {
        Ok((i, Vec::new()))
    }
}

fn ret(i: &str) -> IResult<'_, Option<TypeSignature>> {
    Ok(if i.as_bytes().get(0) == Some(&b'V') {
        (&i[1..], None)
    } else {
        type_sig(i).map(|(i, r)| (i, Some(r)))?
    })
}

fn method_sig(i: &str) -> IResult<'_, MethodSignature> {
    let (i, type_parameters) = type_parameters(i)?;
    let (mut i, ()) = char::<'('>(i)?;
    let mut parameters = vec![];
    while let Ok((new_i, param)) = type_sig(i) {
        i = new_i;
        parameters.push(param);
    }
    let (mut i, return_type) = ret(i)?;
    let mut t = vec![];
    while let Some('^') = i.chars().next() {
        let (new_i, throw) = throws(&i[1..])?;
        i = new_i;
        t.push(throw);
    }
    Ok((
        i,
        MethodSignature {
            type_parameters,
            parameters,
            return_type,
            throws: t,
        },
    ))
}

fn class_sig(i: &str) -> IResult<'_, ClassSignature> {
    let (i, type_parameters) = type_parameters(i)?;
    let (mut i, super_class) = class_type_sig(i)?;
    let mut interfaces = vec![];
    while let Some('L') = i.chars().next() {
        let (new_i, interface) = class_type_sig(&i[1..])?;
        i = new_i;
        interfaces.push(interface);
    }
    Ok((
        i,
        ClassSignature {
            type_parameters,
            super_class,
            interfaces,
        },
    ))
}

macro_rules! fromstr_impls {
    ($($ty: ty, $fn: expr),*) => {
        $(
            impl FromStr for $ty {
                type Err = crate::error::Error;

                fn from_str(s: &str) -> Result<Self, Self::Err> {
                    $fn(s).map(|(_, r)| r)
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
    SimpleClassTypeSignature,
    simple_type_sig,
    RefTypeSignature,
    ref_type_sig,
    ClassTypeSignature,
    class_type_sig,
    TypeSignature,
    type_sig,
    Throws,
    throws,
    TypeParameter,
    type_parameter,
    ClassSignature,
    class_sig,
    MethodSignature,
    method_sig
);

impl FromStr for FieldSignature {
    type Err = crate::error::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(FromStr::from_str(s)?))
    }
}

cprw_impls!(
    SimpleClassTypeSignature,
    RefTypeSignature,
    ClassTypeSignature,
    TypeSignature,
    Throws,
    TypeParameter,
    ClassSignature,
    MethodSignature,
    FieldSignature
);

impl Display for FieldSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl Display for TypeSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeSignature::Byte => f.write_char('B'),
            TypeSignature::Char => f.write_char('C'),
            TypeSignature::Double => f.write_char('D'),
            TypeSignature::Float => f.write_char('F'),
            TypeSignature::Int => f.write_char('I'),
            TypeSignature::Long => f.write_char('J'),
            TypeSignature::Boolean => f.write_char('Z'),
            TypeSignature::Short => f.write_char('S'),
            TypeSignature::Ref(r) => r.fmt(f),
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
            TypeArgument::Any => f.write_str("*"),
            TypeArgument::Exact(ref t) => t.fmt(f),
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
            RefTypeSignature::ClassType(c) => c.fmt(f),
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
            Throws::Class(c) => c.fmt(f),
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
