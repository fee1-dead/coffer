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
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    IO(#[from] std::io::Error),
    #[error("Invalid {0}: {1}")]
    Invalid(&'static str, String),
    #[error(transparent)]
    MUTF(#[from] crate::mod_utf8::MUTFError),
    #[error("Attribute length mismatch: actual length ({0} bytes) is greater than length consumed ({1} bytes)")]
    AttributeLength(u32, u32),
    #[error("Conversion overflows")]
    ArithmeticOverflow,
}

pub type Result<T> = std::result::Result<T, Error>;