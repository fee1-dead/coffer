
use std::borrow::Cow;
use std::fs::File;
use std::io::{Cursor, Write};
use std::sync::Arc;

use class_sample::ClassInfo;
use lazy_static::lazy_static;
use once_cell::sync::OnceCell;

use crate::code::Instruction::{LineNumber, *};
use crate::code::Label as Lbl;
use crate::code::LocalType::Reference;
use crate::prelude::{
    BootstrapMethod, Code, Constant, MemberRef, Method, OrDynamic, RawConstantEntry, Result,
};
use crate::Constant::*;
use crate::GetOrPut::{Get, Put};
use crate::LoadOrStore::{Load, Store};
use crate::MemberType::{Static, Virtual};
use crate::{Class, ConstantPoolReadWrite, ConstantPoolReader, ConstantPoolWriter, ReadWrite};

struct ArrCp<'a>(Cow<'a, [RawConstantEntry]>);

impl<'a> ConstantPoolReader for ArrCp<'a> {
    fn read_raw(&mut self, idx: u16) -> Option<RawConstantEntry> {
        self.0.get(idx as usize - 1).cloned()
    }

    fn resolve_later(&mut self, _bsm_idx: u16, _ptr: Arc<OnceCell<BootstrapMethod>>) {
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

lazy_static! {
    static ref SAMPLE: Vec<ClassInfo> =
        class_sample::get_sample_name_bytes(2 * 1024).expect("fetch sample");
}

#[test]
fn sample_read_write_read() {
    for ClassInfo { file_name, bytes } in SAMPLE.iter() {
        fn handle_error<E: std::fmt::Display + std::fmt::Debug, P: std::fmt::UpperHex>(
            error: E,
            path: &str,
            buf: &[u8],
            cursor_position: P,
            discr: &str,
        ) -> ! {
            let filename = path.split('/').last().unwrap();
            let res = File::create(filename).and_then(|mut f| f.write_all(buf));
            let message = match res {
                Ok(()) => format!("A file named {} has been created", filename),
                Err(e) => format!("Unable to write to file: {:?}", e),
            };
            panic!(
                "Failure while {discr} {filename}: {error} ({error:?})\ncursor position: 0x{cursor_position:X}\n{message}"
            )
        }
        let mut reader = Cursor::new(bytes.as_slice());
        let c = match Class::read_from(&mut reader) {
            Ok(c) => c,
            Err(e) => handle_error(e, file_name, bytes.as_slice(), reader.position(), "reading"),
        };
        let mut bytes = Vec::new();
        if let Err(e) = c.write_to(&mut bytes) {
            handle_error(
                e,
                file_name,
                bytes.as_slice(),
                bytes.len(),
                "rewriting deserialized class",
            );
        }
        let mut reader = Cursor::new(bytes.as_slice());
        if let Err(e) = Class::read_from(&mut reader) {
            handle_error(
                e,
                file_name,
                &bytes,
                reader.position(),
                "rereading serialized class",
            );
        }
    }
}
#[test]
fn code_writing() {
    let lbl1 = Lbl(0);
    let lbl2 = Lbl(1);
    let lbl3 = Lbl(2);
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
            Label(lbl1),
            PushNull,
            Return(Some(Reference)),
            NoOp,
            NoOp,
            Label(lbl2),
            PushNull,
            Return(Some(Reference)),
            NoOp,
            NoOp,
            Label(lbl3),
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
        0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 33, 0, 0, 0, 37, 0, 0, 0, 0, 0, 0, 1, 176, 0, 0, 1, 176,
        0, 0, 1, 176, 0, 0, 0, 0,
    ];
    let lbl1 = Lbl(0);
    let lbl2 = Lbl(1);
    let lbl3 = Lbl(2);
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
                Label(lbl1),
                PushNull,
                Return(Some(Reference)),
                NoOp,
                NoOp,
                Label(lbl2),
                PushNull,
                Return(Some(Reference)),
                NoOp,
                NoOp,
                Label(lbl3),
                PushNull,
                Return(Some(Reference))
            ],
            catches: vec![],
            attrs: vec![]
        }
    )
}
