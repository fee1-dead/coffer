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
use crate::constants::insn::TABLESWITCH;
use std::io::Cursor;
use crate::insn::InstructionReader;
use crate::insn::Instruction::TableSwitch;

#[test]
fn test_tableswitch() {
    let buf = [TABLESWITCH, 0, 0, 0,
                        0, 0, 0, 12, // Default: 12
                        0, 0, 0, 10, // 10-12
                        0, 0, 0, 12,
                        0, 0, 0, 4,  // 10: 4
                        0, 0, 0, 7,  // 11: 7
                        0, 0, 0, 34]; // 12: 34
    let cursor = Cursor::new(buf);
    let mut instruction_read = InstructionReader::new(cursor).unwrap();
    assert_eq!(instruction_read.read_insn().unwrap(), TableSwitch(12, 10, 12, vec![4, 7, 34]))
}