/*
    This file is part of Coffer.

    Coffer is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Coffer is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Coffer. (LICENSE.md)  If not, see <https://www.gnu.org/licenses/>.
*/
#![feature(seek_convenience)]
#![feature(arbitrary_enum_discriminant)]

#[macro_use]
extern crate bitflags;

pub mod constants;
pub mod index;
pub mod jcoder;
pub mod error;
pub mod insn;
pub mod mod_utf8;
pub mod constant_pool;

#[cfg(test)]
mod tests;
pub(crate) mod byteswapper;