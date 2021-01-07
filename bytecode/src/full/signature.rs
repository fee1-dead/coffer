use std::fmt::{Display, Formatter, Write};
use std::borrow::Cow;
use std::str::FromStr;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeSignature {
    Byte, Char, Double, Float, Int, Long, Boolean, Short, Ref(RefTypeSignature)
}

#[repr(transparent)]
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FieldSignature(RefTypeSignature);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ClassSignature {
    pub type_parameters: Vec<TypeParameter>,
    pub super_class: ClassTypeSignature,
    pub interfaces: Vec<ClassTypeSignature>
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MethodSignature {
    pub type_parameters: Vec<TypeParameter>,
    pub parameters: Vec<TypeSignature>,
    pub return_type: Option<TypeSignature>,
    pub throws: Vec<Throws>
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypeParameter {
    pub name: Cow<'static, str>,
    pub class_bound: Option<RefTypeSignature>,
    pub interface_bounds: Vec<RefTypeSignature>
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Throws {
    TypeParameter(Cow<'static, str>),
    Class(ClassTypeSignature)
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeArgument {
    Extends(RefTypeSignature),
    Super(RefTypeSignature),
    Any
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SimpleClassTypeSignature {
    pub name: Cow<'static, str>,
    pub type_arguments: Vec<TypeArgument>
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ClassTypeSignature {
    pub package: Vec<Cow<'static, str>>,
    pub name: SimpleClassTypeSignature,
    pub suffix: Vec<SimpleClassTypeSignature>
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum RefTypeSignature {
    TypeVariable(Cow<'static, str>),
    ArrayRef(u8, Box<TypeSignature>),
    ClassType(ClassTypeSignature)
}

// Implementations

#[inline]
pub(super) fn unexpected_end<T>() -> crate::Result<T> {
    Err(std::io::Error::new(std::io::ErrorKind::InvalidData, "Unexpected end of string").into())
}

use nom::{take, one_of, IResult, take_until1, many0, char, complete, peek, alt, take_until, take_till1, do_parse, terminated, switch, opt};
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
    do_parse!(i,
    char!('L') >>
    package: packages >>
    name: simple_type_sig >>
    suffix: terminated!(many0!(do_parse!(char!('.') >>
    sig: simple_type_sig >> (sig))), char!(';')) >>
    (ClassTypeSignature { package, name, suffix }))
}

fn packages(i: &str) -> IResult<&str, Vec<Cow<'static, str>>> {
    let (i, o) = many0!(i, take_until!("/"))?;
    Ok((i, o.into_iter().map(ToOwned::to_owned).map(Into::into).collect()))
}

fn type_arg(i: &str) -> IResult<&str, TypeArgument> {
    let (i, o) = one_of!(i, "-+*")?;
    Ok(match o {
        '*' => { (i, TypeArgument::Any) }
        '-' => {
            let (i, o) = ref_type_sig(i)?;
            (i, TypeArgument::Super(o))
        }
        '+' => {
            let (i, o) = ref_type_sig(i)?;
            (i, TypeArgument::Extends(o))
        }
        _ => { unsafe { std::hint::unreachable_unchecked() } }
    })
}

fn simple_type_sig(i: &str) -> IResult<&str, SimpleClassTypeSignature> {
    fn type_var_start(c: char) -> bool { c == '<' }
    do_parse!(i, name: take_till1!(type_var_start) >> type_arguments: opt!(do_parse!(char!('<') >> res: many0!(type_arg) >> char!('>') >> (res))) >> (SimpleClassTypeSignature { name: name.to_owned().into(), type_arguments: type_arguments.unwrap_or_default() }))
}

fn type_parameter(i: &str) -> IResult<&str, TypeParameter> {
    do_parse!(i, ident: take_until1!(":") >> char!(':') >> class_bound: opt!(ref_type_sig) >> interface_bounds: many0!(do_parse!(char!(':') >> res: ref_type_sig >> (res))) >> (TypeParameter { name: ident.to_owned().into(), class_bound, interface_bounds }))
}

fn type_parameters(i: &str) -> IResult<&str, Vec<TypeParameter>> {
    do_parse!(i, res: opt!(do_parse!(char!('<') >> res: many0!(type_parameter) >> char!('>') >> (res))) >> (res.unwrap_or_default()))
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

cprw_impls!(SimpleClassTypeSignature, RefTypeSignature, ClassTypeSignature, TypeSignature, Throws, TypeParameter, ClassSignature, MethodSignature);

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