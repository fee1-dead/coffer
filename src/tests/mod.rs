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
use crate::index::JClassIdx;
use std::io::Cursor;

mod mutf8;
mod insn;

#[test]
fn test_parse_class_index() {
    let samples = class_sample::get_sample_name_bytes(100);
    for (name, bytes) in samples {
        test_parse_class(name, bytes)
    }
}

fn test_parse_class(name: String, bytes: Vec<u8>) {
    let mut cursor = Cursor::new(bytes);
    let _jclass = JClassIdx::try_from(&mut cursor).expect(&*format!("Parsing Class {}", name));
}
