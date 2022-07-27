use crate::constants::insn::TABLESWITCH;
use crate::insn::{Instruction, TableSwitch};
use crate::ReadWrite;
use std::io::Cursor;

#[test]
fn test_tableswitch() {
    let buf = [
        TABLESWITCH,
        0,
        0,
        0,
        12, // Default: +12
        0,
        0,
        0,
        10, // 10-12
        0,
        0,
        0,
        12,
        0,
        0,
        0,
        4, // 10: +4
        0,
        0,
        0,
        7, // 11: +7
        0,
        0,
        0,
        34,
    ]; // 12: +34
    let mut cursor = Cursor::new(buf);
    assert_eq!(
        Instruction::read_from(&mut cursor).unwrap(),
        Instruction::TableSwitch(
            12,
            TableSwitch {
                low: 10,
                high: 12,
                offsets: vec![4, 7, 34]
            }
        )
    )
}
