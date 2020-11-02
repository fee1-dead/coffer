use crate::constants::insn::TABLESWITCH;
use std::io::Cursor;
use crate::insn::InstructionReader;
use crate::insn::Instruction::TableSwitch;

#[test]
fn test_tableswitch() {
    let buf = [TABLESWITCH, 0, 0, 0, 0, 0, 0, 12, 0, 0, 0, 10, 0, 0, 0, 12, 0, 0, 0, 4, 0, 0, 0, 7, 0, 0, 0, 34];
    let cursor = Cursor::new(buf);
    let mut instruction_read = InstructionReader::new(cursor).unwrap();
    assert_eq!(instruction_read.read_insn().unwrap(), TableSwitch(12, 10, 12, vec![4, 7, 34]))
}