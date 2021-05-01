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
//! A Module is not a class, but it is still represented by a .class file.
//!
//! Modules exist for Java 9+, to make your class recognized as a module,
//! the module bit must be set for the access and
//! the class must have an Module attribute.
//!
//! See [the Java Language Spec](https://docs.oracle.com/javase/specs/jls/se16/html/jls-7.html#jls-7.7) and
//! [the JVM Spec](https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.7.25) for more details.

use crate::flags::{ModuleFlags, RequireFlags};
use crate::prelude::*;

#[derive(Clone, Eq, PartialEq, Debug, ConstantPoolReadWrite)]
pub struct Require {
    #[str_type(Module)]
    pub module: Cow<'static, str>,
    #[use_normal_rw]
    pub flags: RequireFlags,
    #[str_optional]
    pub version: Option<Cow<'static, str>>,
}

#[derive(Clone, Eq, PartialEq, Debug, ConstantPoolReadWrite)]
pub struct Provide {
    #[str_type(Class)]
    pub class: Cow<'static, str>,
    #[vec_len_type(u16)]
    #[str_type(Class)]
    pub with: Vec<Cow<'static, str>>,
}

#[derive(Eq, PartialEq, Debug, Clone, ConstantPoolReadWrite)]
pub struct Module {
    #[str_type(Module)]
    pub name: Cow<'static, str>,
    #[use_normal_rw]
    pub flags: ModuleFlags,
    #[str_optional]
    pub version: Option<Cow<'static, str>>,
    #[vec_len_type(u16)]
    pub requires: Vec<Require>,
    #[vec_len_type(u16)]
    pub exports: Vec<Export>,
    #[vec_len_type(u16)]
    pub opens: Vec<Open>,
    #[vec_len_type(u16)]
    #[str_type(Class)]
    pub uses: Vec<Cow<'static, str>>,
    #[vec_len_type(u16)]
    pub provides: Vec<Provide>,
}

#[derive(Clone, Eq, PartialEq, Debug, ConstantPoolReadWrite)]
pub struct Export {
    #[str_type(Package)]
    pub package: Cow<'static, str>,
    #[use_normal_rw]
    pub flags: ExOpFlags,
    #[vec_len_type(u16)]
    #[str_type(Module)]
    pub to: Vec<Cow<'static, str>>,
}

impl Export {
    /// Creates a new instance of [`Export`].
    ///
    /// [`Export`]: Export
    pub fn new<
        ToStr: Into<Cow<'static, str>>,
        ToOp: Into<ExOpFlags>,
        ToVec: Into<Vec<Cow<'static, str>>>,
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
    #[str_type(Package)]
    pub package: Cow<'static, str>,
    #[use_normal_rw]
    pub flags: ExOpFlags,
    #[vec_len_type(u16)]
    #[str_type(Module)]
    pub to: Vec<Cow<'static, str>>,
}
