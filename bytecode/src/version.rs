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
//! this module defines structures and enum for java version.

use std::fmt::{Display, Formatter, Result};

/// the version of a java class.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, ReadWrite)]
pub struct JavaVersion {
    /// The minor version.
    pub minor: u16,
    /// The major version.
    pub major: MajorVersion,
}

impl JavaVersion {
    pub const J8: JavaVersion = JavaVersion {
        minor: 0,
        major: MajorVersion::J8,
    };
}

/// Major version of a java class.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, ReadWrite)]
#[tag_type(u16)]
pub enum MajorVersion {
    /// Java version 1.0.2/1.1
    J1 = 45,
    /// Java version 1.2
    J1_2,
    /// Java version 1.3
    J1_3,
    /// Java version 1.4
    J1_4,
    /// Java SE 5
    J5,
    /// Java SE 6
    J6,
    /// Java SE 7
    J7,
    /// Java SE 8
    J8,
    /// Java SE 9
    J9,
    /// Java SE 10
    J10,
    /// Java SE 11
    J11,
    /// Java SE 12
    J12,
    /// Java SE 13
    J13,
    /// Java SE 14
    J14,
    /// Java SE 15
    J15,
}

impl Display for JavaVersion {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self.major {
            MajorVersion::J1 => {
                if self.minor <= 3 {
                    write!(f, "JDK 1.0.2 minor version {}", self.minor)
                } else {
                    write!(f, "JDK 1.1 minor version {}", self.minor)
                }
            }
            MajorVersion::J1_2 => {
                write!(f, "JDK 1.2 minor version {}", self.minor)
            }
            MajorVersion::J1_3 => {
                write!(f, "JDK 1.3 minor version {}", self.minor)
            }
            MajorVersion::J1_4 => {
                write!(f, "JDK 1.4 minor version {}", self.minor)
            }
            MajorVersion::J5 => {
                write!(f, "Java SE 5.0 minor version {}", self.minor)
            }
            MajorVersion::J6 => {
                write!(f, "Java SE 6.0 minor version {}", self.minor)
            }
            MajorVersion::J7 => {
                write!(f, "Java SE 7 minor version {}", self.minor)
            }
            MajorVersion::J8 => {
                write!(f, "Java SE 8 minor version {}", self.minor)
            }
            MajorVersion::J9 => {
                write!(f, "Java SE 9 minor version {}", self.minor)
            }
            MajorVersion::J10 => {
                write!(f, "Java SE 10 minor version {}", self.minor)
            }
            MajorVersion::J11 => {
                write!(f, "Java SE 11 minor version {}", self.minor)
            }
            MajorVersion::J12 => {
                write!(f, "Java SE 12 minor version {}", self.minor)
            }
            MajorVersion::J13 => {
                write!(f, "Java SE 13 minor version {}", self.minor)
            }
            MajorVersion::J14 => {
                write!(f, "Java SE 14 minor version {}", self.minor)
            }
            MajorVersion::J15 => {
                write!(f, "Java SE 15 minor version {}", self.minor)
            }
        }
    }
}
