use std::fmt::{Display, Formatter, Result};

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, ReadWrite)]
pub struct JavaVersion {
    pub minor: u16,
    pub major: MajorVersion
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, ReadWrite)]
#[tag_type(u16)]
pub enum MajorVersion {
    J1 = 45, J1_2, J1_3, J1_4, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14, J15
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