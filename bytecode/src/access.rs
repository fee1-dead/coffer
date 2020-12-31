/*
    This file is part of Coffer.

    Coffer is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Coffer is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Coffer. (LICENSE.md)  If not, see <https://www.gnu.org/licenses/>.
*/
use crate::ReadWrite;
use crate::Result;
use std::io::{Read, Write};

bitflags! {
    pub struct AccessFlags: u16 {
        // @formatter:off
        const ACC_PUBLIC       = 0b0000_0000_0000_0001;
        const ACC_PRIVATE      = 0b0000_0000_0000_0010;
        const ACC_PROTECTED    = 0b0000_0000_0000_0100;
        const ACC_STATIC       = 0b0000_0000_0000_1000;
        const ACC_FINAL        = 0b0000_0000_0001_0000;
        const ACC_SUPER        = 0b0000_0000_0010_0000;
        const ACC_SYNCHRONIZED = 0b0000_0000_0010_0000;
        const ACC_OPEN         = 0b0000_0000_0010_0000;
        const ACC_TRANSITIVE   = 0b0000_0000_0010_0000;
        const ACC_VOLATILE     = 0b0000_0000_0100_0000;
        const ACC_BRIDGE       = 0b0000_0000_0100_0000;
        const ACC_STATIC_PHASE = 0b0000_0000_0100_0000;
        const ACC_VARARGS      = 0b0000_0000_1000_0000;
        const ACC_TRANSIENT    = 0b0000_0000_1000_0000;
        const ACC_NATIVE       = 0b0000_0001_0000_0000;
        const ACC_INTERFACE    = 0b0000_0010_0000_0000;
        const ACC_ABSTRACT     = 0b0000_0100_0000_0000;
        const ACC_STRICT       = 0b0000_1000_0000_0000;
        const ACC_SYNTHETIC    = 0b0001_0000_0000_0000;
        const ACC_ANNOTATION   = 0b0010_0000_0000_0000;
        const ACC_ENUM         = 0b0100_0000_0000_0000;
        const ACC_MANDATED     = 0b1000_0000_0000_0000;
        const ACC_MODULE       = 0b1000_0000_0000_0000;
        // @formatter:on
    }
}

impl ReadWrite for AccessFlags {
    fn read_from<T: Read>(reader: &mut T) -> Result<AccessFlags> {
        Ok(AccessFlags::from_bits(u16::read_from(reader)?).unwrap())
    }

    fn write_to<T: Write>(&self, writer: &mut T) -> Result<()> {
        Ok(self.bits().write_to(writer)?)
    }
}