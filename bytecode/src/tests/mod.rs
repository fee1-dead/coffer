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
mod full_type;
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

mod code_reading {
    use crate::full::{Code, BootstrapMethod, OrDynamic, Constant};
    use crate::{ConstantPoolReadWrite, ConstantPoolReader};
    use crate::full::cp::RawConstantEntry;
    use std::io::Cursor;
    use crate::full::Instruction::*;
    use crate::full::StackValueType::One;
    use crate::full::LocalType::Reference;

    struct ArrCp<'a>(&'a [RawConstantEntry]);

    impl<'a> ConstantPoolReader for ArrCp<'a> {
        fn read_raw(&mut self, idx: u16) -> Option<RawConstantEntry> {
            self.0.get(idx as usize - 1).cloned()
        }

        fn resolve_later(&mut self, _bsm_idx: u16, _ptr: &mut Box<BootstrapMethod>) {
            unimplemented!()
        }
    }

    #[test]
    fn code_reading() {
        let mut cp = ArrCp(&[RawConstantEntry::Int(123)]);
        let bytes = [
            0, 255, // max_stack
            0, 254, // max_locals
            0, 0, 0, 44, // code_length
            0, // NOP
            1, // null
            87, // pop
            18, 1, // LDC #1
            170, 0, 0, // tableswitch with padding
            0, 0, 0, 29, // default
            0, 0, 0, 1,
            0, 0, 0, 2,
            0, 0, 0, 33,
            0, 0, 0, 37,
            0, 0, 0, 0,
            0, 0, 1, 176,
            0, 0, 1, 176,
            0, 0, 1, 176,
            0, 0,
            0, 0
        ];
        let lbl1 = crate::Label(0);
        let lbl2 = crate::Label(1);
        let lbl3 = crate::Label(2);
        assert_eq!(Code::read_from(&mut cp, &mut Cursor::new(bytes)).unwrap(), Code {
            max_stack: 255,
            max_locals: 254,
            code: vec![
                NoOp, PushNull, Pop(One), Push(OrDynamic::Static(Constant::I32(123))), TableSwitch {
                    default: lbl1,
                    low: 1,
                    offsets: vec![lbl2, lbl3]
                },
                NoOp, NoOp, NoOp, NoOp, NoOp, NoOp,
                Label(lbl1), PushNull, Return(Some(Reference)), NoOp, NoOp,
                Label(lbl2), PushNull, Return(Some(Reference)), NoOp, NoOp,
                Label(lbl3), PushNull, Return(Some(Reference))
            ],
            catches: vec![],
            attrs: vec![]
        })

    }
}

