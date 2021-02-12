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

mod mutf8;
mod full_type;
mod insn;
mod code {
    use crate::full::{Code, BootstrapMethod, OrDynamic};
    use crate::{ConstantPoolReadWrite, ConstantPoolReader, ConstantPoolWriter, ReadWrite};
    use crate::full::cp::RawConstantEntry;
    use std::io::{Cursor, Read, Seek, SeekFrom, Write};
    use crate::full::Instruction::*;
    use crate::full::StackValueType::One;
    use crate::full::LocalType::Reference;
    use crate::prelude::*;
    use std::borrow::Cow;
    use std::collections::HashMap;


    struct ArrCp<'a>(Cow<'a, [RawConstantEntry]>);

    impl<'a> ConstantPoolReader for ArrCp<'a> {
        fn read_raw(&mut self, idx: u16) -> Option<RawConstantEntry> {
            self.0.get(idx as usize - 1).cloned()
        }

        fn resolve_later(&mut self, _bsm_idx: u16, _ptr: Rc<LazyCell<BootstrapMethod>>) {
            unimplemented!()
        }
    }

    impl<'a> ConstantPoolWriter for ArrCp<'a> {
        fn insert_raw(&mut self, value: RawConstantEntry) -> u16 {
            self.0.to_mut().push(value);
            (self.0.len() - 1) as u16
        }

        fn insert_bsm(&mut self, _bsm: BootstrapMethod) -> u16 {
            unimplemented!()
        }
    }

    fn arr_cp<'a, T: Into<Cow<'a, [RawConstantEntry]>>>(inner: T) -> ArrCp<'a> {
        ArrCp(inner.into())
    }

    #[derive(Debug, Clone, Default)]
    struct MapCp(HashMap<u16, RawConstantEntry>, Vec<Rc<LazyCell<BootstrapMethod>>>);

    impl ConstantPoolReader for MapCp {
        fn read_raw(&mut self, idx: u16) -> Option<RawConstantEntry> {
            self.0.get(&idx).cloned()
        }

        fn resolve_later(&mut self, _bsm_idx: u16, ptr: Rc<LazyCell<BootstrapMethod>>) {
            self.1.push(ptr);
        } // we are using this for testing, so we don't panic here in case a bsm was referenced.
    }

    impl MapCp {
        fn read_from<R: Read>(reader: &mut R) -> crate::Result<Self> {
            let mut map = HashMap::new();
            let count = u16::read_from(reader)?;
            let mut i = 1;
            while i < count {
                let entry = RawConstantEntry::read_from(reader)?;
                let size = if entry.is_wide() {
                    2
                } else {
                    1
                };
                map.insert(i, entry);
                i += size;
            }
            Ok(Self(map, Vec::new()))
        }
    }

    use lazy_static::lazy_static;
    use std::fs::File;
    use lazycell::LazyCell;
    use std::rc::Rc;
    lazy_static! {
        static ref SAMPLE: Vec<(String, Vec<u8>)> = class_sample::get_sample_name_bytes(2 * 1024);
    }

    #[test]
    fn code_reading_with_sample() {
        for (s, buf) in SAMPLE.iter().map(|(s, buf)| (s.as_str(), buf.clone())) {
            let mut reader = Cursor::new(buf);
            if !u32::read_from(&mut reader).map(|h| h == 0xCAFEBABE).unwrap_or_default() {
                // this is an invalid class, we will skip this.
                continue
            }
            // skip versions
            reader.seek(SeekFrom::Current(4)).unwrap();
            let mut cp = MapCp::read_from(&mut reader).unwrap();
            #[inline]
            fn inner(cp: &mut MapCp, reader: &mut Cursor<Vec<u8>>) -> crate::Result<()> {
                reader.seek(SeekFrom::Current(6))?;
                let interfaces = u16::read_from(reader)?;
                reader.seek(SeekFrom::Current(interfaces as i64 * 2))?;
                let fields = u16::read_from(reader)?;
                for _ in 0..fields {
                    reader.seek(SeekFrom::Current(6))?;
                    let attrs = u16::read_from(reader)?;
                    for _ in 0..attrs {
                        reader.seek(SeekFrom::Current(2))?;
                        let length = u32::read_from(reader)?;
                        reader.set_position(reader.position() + length as u64);
                    }
                }
                let methods = u16::read_from(reader)?;
                for _ in 0..methods {
                    reader.seek(SeekFrom::Current(6))?;
                    let attrs = u16::read_from(reader)?;
                    for _ in 0..attrs {
                        let name: Cow<'static, str> = crate::read_from!(cp, reader)?;
                        let length = u32::read_from(reader)?;
                        if name.as_ref() == "Code" {
                            Code::read_from(cp, reader)?;
                        } else {
                            reader.set_position(reader.position() + length as u64);
                        }
                    }
                }
                Ok(())
            }
            if let Err(e) = inner(&mut cp, &mut reader) {
                println!("CP: {:?}", cp);
                let filename = s.split('/').last().unwrap();
                let res = File::create(filename).and_then(|mut f| f.write_all(reader.get_ref().as_ref()));
                let message = match res {
                    Ok(()) => format!("A file named {} has been created", filename),
                    Err(e) => format!("Unable to write to file: {:?}", e)
                };
                panic!("Unable to parse {}: {}\ncursor position: 0x{:X}\n{}", s, e, reader.position(), message)
            }
        }
    }
    #[test]
    fn code_writing() {
        let lbl1 = crate::Label(0);
        let lbl2 = crate::Label(1);
        let lbl3 = crate::Label(2);
        let mut buffer = Cursor::new(Vec::new());
        let mut cp = arr_cp([].as_ref());
        Code {
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
        }.write_to(&mut cp,&mut buffer).unwrap();
        assert_eq!(buffer.into_inner(), vec![
            0, 255, // max_stack
            0, 254, // max_locals
            0, 0, 0, 44, // code_length
            0, // NOP
            1, // null
            87, // pop
            crate::constants::insn::BIPUSH, 123,
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
        ])
    }
    #[test]
    fn code_reading() {
        let mut cp = arr_cp([RawConstantEntry::Int(123)].as_ref());
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

