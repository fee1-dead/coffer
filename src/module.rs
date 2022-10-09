//! A Module is not a class, but it is still represented by a .class file.
//!
//! Modules exist for Java 9+, to make your class recognized as a module,
//! the module bit must be set for the access and
//! the class must have an Module attribute.
//!
//! See [the Java Language Spec](https://docs.oracle.com/javase/specs/jls/se16/html/jls-7.html#jls-7.7) and
//! [the JVM Spec](https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.7.25) for more details.

use wtf_8::Wtf8Str;

use crate::flags::{ModuleFlags, RequireFlags};
use crate::prelude::*;

#[derive(Clone, Eq, PartialEq, Debug, ConstantPoolReadWrite)]
pub struct Require {
    #[coffer(as = "h::Module")]
    pub module: Cow<'static, Wtf8Str>,
    #[coffer(as = "h::Normal")]
    pub flags: RequireFlags,
    pub version: Option<Cow<'static, Wtf8Str>>,
}

#[derive(Clone, Eq, PartialEq, Debug, ConstantPoolReadWrite)]
pub struct Provide {
    #[coffer(as = "h::Class")]
    pub class: Cow<'static, Wtf8Str>,
    #[coffer(as = "h::Vec16<h::Class>")]
    pub with: Vec<Cow<'static, Wtf8Str>>,
}

#[derive(Eq, PartialEq, Debug, Clone, ConstantPoolReadWrite)]
pub struct Module {
    #[coffer(as = "h::Module")]
    pub name: Cow<'static, Wtf8Str>,
    #[coffer(as = "h::Normal")]
    pub flags: ModuleFlags,
    pub version: Option<Cow<'static, Wtf8Str>>,
    #[coffer(as = "h::Vec16")]
    pub requires: Vec<Require>,
    #[coffer(as = "h::Vec16")]
    pub exports: Vec<Export>,
    #[coffer(as = "h::Vec16")]
    pub opens: Vec<Open>,
    #[coffer(as = "h::Vec16<h::Class>")]
    pub uses: Vec<Cow<'static, Wtf8Str>>,
    #[coffer(as = "h::Vec16")]
    pub provides: Vec<Provide>,
}

#[derive(Clone, Eq, PartialEq, Debug, ConstantPoolReadWrite)]
pub struct Export {
    #[coffer(as = "h::Package")]
    pub package: Cow<'static, Wtf8Str>,
    #[coffer(as = "h::Normal")]
    pub flags: ExOpFlags,
    #[coffer(as = "h::Vec16<h::Module>")]
    pub to: Vec<Cow<'static, Wtf8Str>>,
}

impl Export {
    /// Creates a new instance of [`Export`].
    ///
    /// [`Export`]: Export
    pub fn new<
        ToStr: Into<Cow<'static, Wtf8Str>>,
        ToOp: Into<ExOpFlags>,
        ToVec: Into<Vec<Cow<'static, Wtf8Str>>>,
    >(
        pkg: ToStr,
        flags: ToOp,
        to: ToVec,
    ) -> Self {
        Self {
            package: pkg.into(),
            flags: flags.into(),
            to: to.into(),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug, ConstantPoolReadWrite)]
pub struct Open {
    #[coffer(as = "h::Package")]
    pub package: Cow<'static, Wtf8Str>,
    #[coffer(as = "h::Normal")]
    pub flags: ExOpFlags,
    #[coffer(as = "h::Vec16<h::Module>")]
    pub to: Vec<Cow<'static, Wtf8Str>>,
}
