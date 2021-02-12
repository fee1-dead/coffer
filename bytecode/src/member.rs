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
//! Members of a java class
//!
//! They can be fields or methods.
use crate::prelude::*;

/// A reference to a member.
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct MemberRef {
    /// The class holding this member.
    pub owner: Cow<'static, str>,
    /// The name of this member.
    pub name: Cow<'static, str>,
    /// The descriptor of this member.
    ///
    /// Can be a field descriptor or a method descriptor.
    ///
    /// This field is used to determine whether this member is a field or a method.
    pub descriptor: Type,
    /// Determines whether the owner is an interface.
    ///
    /// This field is only applicable to methods, and if this member is a field this field will be ignored.
    pub itfs: bool,
}

impl ConstantPoolReadWrite for MemberRef {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> Result<Self, Error> {
        try_cp_read!(cp, reader, read_member)
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(&self, cp: &mut C, writer: &mut W) -> Result<(), Error> {
        cp.insert_member(self.clone()).write_to(writer)
    }
}