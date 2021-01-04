use std::fmt::{Display, Formatter, Result, Write};
use std::borrow::Cow;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeSignature {
    Byte, Char, Double, Float, Int, Long, Boolean, Ref(RefTypeSignature)
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

impl Display for FieldSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.0.fmt(f)
    }
}
impl Display for TypeSignature {
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
impl Display for TypeArgument {
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
impl Display for SimpleClassTypeSignature {
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
impl Display for ClassTypeSignature {
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
impl Display for RefTypeSignature {
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
impl Display for Throws {
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
impl Display for TypeParameter {
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
impl Display for MethodSignature {
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
impl Display for ClassSignature {
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