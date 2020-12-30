use std::fmt::{Display, Formatter, Result, Write};
use std::borrow::Cow;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeSignature<'a> {
    Byte, Char, Double, Float, Int, Long, Boolean, Ref(RefTypeSignature<'a>)
}

#[repr(transparent)]
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FieldSignature<'a>(RefTypeSignature<'a>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ClassSignature<'a> {
    pub type_parameters: Vec<TypeParameter<'a>>,
    pub super_class: ClassTypeSignature<'a>,
    pub interfaces: Vec<ClassTypeSignature<'a>>
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MethodSignature<'a> {
    pub type_parameters: Vec<TypeParameter<'a>>,
    pub parameters: Vec<TypeSignature<'a>>,
    pub return_type: Option<TypeSignature<'a>>,
    pub throws: Vec<Throws<'a>>
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypeParameter<'a> {
    pub name: Cow<'a, str>,
    pub class_bound: Option<RefTypeSignature<'a>>,
    pub interface_bounds: Vec<RefTypeSignature<'a>>
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Throws<'a> {
    TypeParameter(Cow<'a , str>),
    Class(ClassTypeSignature<'a>)
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeArgument<'a> {
    Extends(RefTypeSignature<'a>),
    Super(RefTypeSignature<'a>),
    Any
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SimpleClassTypeSignature<'a> {
    pub name: Cow<'a, str>,
    pub type_arguments: Vec<TypeArgument<'a>>
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ClassTypeSignature<'a> {
    pub package: Vec<Cow<'a, str>>,
    pub name: SimpleClassTypeSignature<'a>,
    pub suffix: Vec<SimpleClassTypeSignature<'a>>
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum RefTypeSignature<'a> {
    TypeVariable(Cow<'a, str>),
    ArrayRef(u8, Box<TypeSignature<'a>>),
    ClassType(ClassTypeSignature<'a>)
}

// Implementations

impl<'a> Display for FieldSignature<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.0.fmt(f)
    }
}
impl<'a> Display for TypeSignature<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            TypeSignature::Byte => { f.write_char('B') }
            TypeSignature::Char => { f.write_char('C') }
            TypeSignature::Double => { f.write_char('D') }
            TypeSignature::Float => { f.write_char('F') }
            TypeSignature::Int => { f.write_char('I') }
            TypeSignature::Long => { f.write_char('J') }
            TypeSignature::Boolean => { f.write_char('Z') }
            TypeSignature::Ref(r) => { r.fmt(f) }
        }
    }
}
impl<'a> Display for TypeArgument<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            TypeArgument::Extends(ref t) => {
                f.write_str("+")?;
                t.fmt(f)
            }
            TypeArgument::Super(ref t) => {
                f.write_str("-")?; // TODO not sure, determine whether super is - or +.
                t.fmt(f)
            }
            TypeArgument::Any => {
                f.write_str("*")
            }
        }
    }
}
impl<'a> Display for SimpleClassTypeSignature<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
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
impl<'a> Display for ClassTypeSignature<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
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
impl<'a> Display for RefTypeSignature<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
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
impl<'a> Display for Throws<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
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
impl<'a> Display for TypeParameter<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
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
impl<'a> Display for MethodSignature<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
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
impl<'a> Display for ClassSignature<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
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