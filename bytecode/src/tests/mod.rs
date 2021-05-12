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

mod exec;
mod full_type;
mod insn;
mod mutf8;

mod code {
    use crate::code::{Instruction::Label as Lbl, Instruction::*, Label, LocalType::Reference};
    use crate::prelude::*;
    use crate::{Class, ConstantPoolReadWrite, ConstantPoolReader, ConstantPoolWriter, ReadWrite};
    use std::borrow::Cow;
    use std::io::{Cursor, Write};

    struct ArrCp<'a>(Cow<'a, [RawConstantEntry]>);

    impl<'a> ConstantPoolReader for ArrCp<'a> {
        fn read_raw(&mut self, idx: u16) -> Option<RawConstantEntry> {
            self.0.get(idx as usize - 1).cloned()
        }

        fn resolve_later(&mut self, _bsm_idx: u16, _ptr: Rc<LazyBsm>) {
            unreachable!()
        }

        fn bootstrap_methods(&mut self, _bsms: &[BootstrapMethod]) -> Result<()> {
            unreachable!()
        }
    }

    impl<'a> ConstantPoolWriter for ArrCp<'a> {
        fn insert_raw(&mut self, value: RawConstantEntry) -> u16 {
            self.0.to_mut().push(value);
            (self.0.len() - 1) as u16
        }

        fn insert_bsm(&mut self, _bsm: BootstrapMethod) -> u16 {
            unreachable!()
        }
    }

    fn arr_cp<'a, T: Into<Cow<'a, [RawConstantEntry]>>>(inner: T) -> ArrCp<'a> {
        ArrCp(inner.into())
    }

    use lazy_static::lazy_static;
    use std::fs::File;
    use std::rc::Rc;
    lazy_static! {
        static ref SAMPLE: Vec<(String, Vec<u8>)> = class_sample::get_sample_name_bytes(2 * 1024);
    }

    #[test]
    fn sample_read_write_read() {
        for (s, buf) in SAMPLE.iter().map(|(s, buf)| (s.as_str(), buf.clone())) {
            fn handle_error<E: std::fmt::Display + std::fmt::Debug, P: std::fmt::UpperHex>(e: E, s: &str, buf: &[u8], pos: P, phase: u8) {
                let filename = s.split('/').last().unwrap();
                let res =
                    File::create(filename).and_then(|mut f| f.write_all(buf));
                let message = match res {
                    Ok(()) => format!("A file named {} has been created", filename),
                    Err(e) => format!("Unable to write to file: {:?}", e),
                };
                panic!(
                    "Failure in phase {} of read/write/read for {}: {} ({2:?})\ncursor position: 0x{:X}\n{}",
                    phase,
                    s,
                    e,
                    pos,
                    message
                )
            }
            let mut reader = Cursor::new(buf);
            match Class::read_from(&mut reader) {
                Ok(c) => {
                    let mut writer = Vec::new();
                    match c.write_to(&mut writer) {
                        Ok(()) => {
                            reader = Cursor::new(writer);
                            if let Err(e) = Class::read_from(&mut reader) {
                                handle_error(e, s, reader.get_ref(), reader.position(), 3)
                            }
                        }
                        Err(e) => handle_error(e, s, reader.get_ref(), writer.len(), 2)
                    }
                }
                Err(e) => handle_error(e, s, reader.get_ref(), reader.position(), 1)
            }
        }
    }
    #[test]
    fn code_writing() {
        let lbl1 = Label(0);
        let lbl2 = Label(1);
        let lbl3 = Label(2);
        let mut buffer = Cursor::new(Vec::new());
        let mut cp = arr_cp([].as_ref());
        Code {
            max_stack: 255,
            max_locals: 254,
            code: vec![
                NoOp,
                PushNull,
                Pop1,
                Push(OrDynamic::Static(Constant::I32(123))),
                TableSwitch {
                    default: lbl1,
                    low: 1,
                    offsets: vec![lbl2, lbl3],
                },
                NoOp,
                NoOp,
                NoOp,
                NoOp,
                NoOp,
                NoOp,
                Lbl(lbl1),
                PushNull,
                Return(Some(Reference)),
                NoOp,
                NoOp,
                Lbl(lbl2),
                PushNull,
                Return(Some(Reference)),
                NoOp,
                NoOp,
                Lbl(lbl3),
                PushNull,
                Return(Some(Reference)),
            ],
            catches: vec![],
            attrs: vec![],
        }
        .write_to(&mut cp, &mut buffer)
        .unwrap();
        assert_eq!(
            buffer.into_inner(),
            vec![
                0,
                255, // max_stack
                0,
                254, // max_locals
                0,
                0,
                0,
                43, // code_length
                0,  // NOP
                1,  // null
                87, // pop
                crate::constants::insn::BIPUSH,
                123,
                170,
                0, // tableswitch with padding
                0,
                0,
                0,
                28, // default
                0,
                0,
                0,
                1,
                0,
                0,
                0,
                2,
                0,
                0,
                0,
                32,
                0,
                0,
                0,
                36,
                0,
                0,
                0,
                0,
                0,
                0,
                1,
                176,
                0,
                0,
                1,
                176,
                0,
                0,
                1,
                176,
                0,
                0,
                0,
                0
            ]
        )
    }
    #[test]
    fn code_reading() {
        let mut cp = arr_cp([RawConstantEntry::Int(123)].as_ref());
        let bytes = [
            0, 255, // max_stack
            0, 254, // max_locals
            0, 0, 0, 44, // code_length
            0,  // NOP
            1,  // null
            87, // pop
            18, 1, // LDC #1
            170, 0, 0, // tableswitch with padding
            0, 0, 0, 29, // default
            0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 33, 0, 0, 0, 37, 0, 0, 0, 0, 0, 0, 1, 176, 0, 0, 1,
            176, 0, 0, 1, 176, 0, 0, 0, 0,
        ];
        let lbl1 = Label(0);
        let lbl2 = Label(1);
        let lbl3 = Label(2);
        assert_eq!(
            Code::read_from(&mut cp, &mut Cursor::new(bytes)).unwrap(),
            Code {
                max_stack: 255,
                max_locals: 254,
                code: vec![
                    NoOp,
                    PushNull,
                    Pop1,
                    Push(OrDynamic::Static(Constant::I32(123))),
                    TableSwitch {
                        default: lbl1,
                        low: 1,
                        offsets: vec![lbl2, lbl3]
                    },
                    NoOp,
                    NoOp,
                    NoOp,
                    NoOp,
                    NoOp,
                    NoOp,
                    Lbl(lbl1),
                    PushNull,
                    Return(Some(Reference)),
                    NoOp,
                    NoOp,
                    Lbl(lbl2),
                    PushNull,
                    Return(Some(Reference)),
                    NoOp,
                    NoOp,
                    Lbl(lbl3),
                    PushNull,
                    Return(Some(Reference))
                ],
                catches: vec![],
                attrs: vec![]
            }
        )
    }
}
