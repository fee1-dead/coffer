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
//! Useful re-exports that can be reused.

pub use std::borrow::Cow;
pub use std::io::{Read, Write};
pub use crate::{ConstantPoolReader, ConstantPoolWriter, ConstantPoolReadWrite, ReadWrite, try_cp_read, read_from, write_to, Result, Error};
pub use crate::member::*;
pub use crate::ty::*;
pub use crate::signature::*;
pub use crate::loadable::*;
pub use crate::attr::*;
pub use crate::version::*;
pub use coffer_macros::*;
