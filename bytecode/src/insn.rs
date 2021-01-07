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

//! Basic Instruction Reading.
//!
//! Does not depend on the context (constant pool/symbol table),
//! therefore hard to understand and requires boilerplate,
//! but is the easiest to implement.

use std::io::{Read, Cursor, Write, Seek, SeekFrom};
use crate::jcoder::JDecoder;
use crate::error::Error;
use crate::insn::Instruction::{LookupSwitch, TableSwitch};

#[repr(u8)]
#[derive(Debug, PartialEq, Eq)]
pub enum Instruction {
    Nop = 0x00,
    AConstNull = 0x01,
    IConstM1 = 0x02,
    IConst0 = 0x03,
    IConst1 = 0x04,
    IConst2 = 0x05,
    IConst3 = 0x06,
    IConst4 = 0x07,
    IConst5 = 0x08,
    LConst0 = 0x09,
    LConst1 = 0x0A,
    FConst0 = 0x0B,
    FConst1 = 0x0C,
    FConst2 = 0x0D,
    DConst0 = 0x0E,
    DConst1 = 0x0F,
    Bipush(i8) = 0x10,
    Sipush(i16) = 0x11,
    LDC(u8) = 0x12,
    LDCW(u16) = 0x13,
    LDC2W(u16) = 0x14,
    ILoad(u8) = 0x15,
    LLoad(u8) = 0x16,
    FLoad(u8) = 0x17,
    DLoad(u8) = 0x18,
    ALoad(u8) = 0x19,
    ILoad0 = 0x1A,
    ILoad1 = 0x1B,
    ILoad2 = 0x1C,
    ILoad3 = 0x1D,
    LLoad0 = 0x1E,
    LLoad1 = 0x1F,
    LLoad2 = 0x20,
    LLoad3 = 0x21,
    FLoad0 = 0x22,
    FLoad1 = 0x23,
    FLoad2 = 0x24,
    FLoad3 = 0x25,
    DLoad0 = 0x26,
    DLoad1 = 0x27,
    DLoad2 = 0x28,
    DLoad3 = 0x29,
    ALoad0 = 0x2A,
    ALoad1 = 0x2B,
    ALoad2 = 0x2C,
    ALoad3 = 0x2D,
    IALoad = 0x2E,
    LALoad = 0x2F,
    FALoad = 0x30,
    DALoad = 0x31,
    AALoad = 0x32,
    BALoad = 0x33,
    CALoad = 0x34,
    SALoad = 0x35,
    IStore(u8) = 0x36,
    LStore(u8) = 0x37,
    FStore(u8) = 0x38,
    DStore(u8) = 0x39,
    AStore(u8) = 0x3A,
    IStore0 = 0x3B,
    IStore1 = 0x3C,
    IStore2 = 0x3D,
    IStore3 = 0x3E,
    LStore0 = 0x3F,
    LStore1 = 0x40,
    LStore2 = 0x41,
    LStore3 = 0x42,
    FStore0 = 0x43,
    FStore1 = 0x44,
    FStore2 = 0x45,
    FStore3 = 0x46,
    DStore0 = 0x47,
    DStore1 = 0x48,
    DStore2 = 0x49,
    DStore3 = 0x4A,
    AStore0 = 0x4B,
    AStore1 = 0x4C,
    AStore2 = 0x4D,
    AStore3 = 0x4E,
    IAStore = 0x4F,
    LAStore = 0x50,
    FAStore = 0x51,
    DAStore = 0x52,
    AAStore = 0x53,
    BAStore = 0x54,
    CAStore = 0x55,
    SAStore = 0x56,
    Pop = 0x57,
    Pop2 = 0x58,
    Dup = 0x59,
    Dupx1 = 0x5A,
    Dupx2 = 0x5B,
    Dup2 = 0x5C,
    Dup2x1 = 0x5D,
    Dup2x2 = 0x5E,
    Swap = 0x5F,
    IAdd = 0x60,
    LAdd = 0x61,
    FAdd = 0x62,
    DAdd = 0x63,
    ISub = 0x64,
    LSub = 0x65,
    FSub = 0x66,
    DSub = 0x67,
    IMul = 0x68,
    LMul = 0x69,
    FMul = 0x6A,
    DMul = 0x6B,
    IDiv = 0x6C,
    LDiv = 0x6D,
    FDiv = 0x6E,
    DDiv = 0x6F,
    IRem = 0x70,
    LRem = 0x71,
    FRem = 0x72,
    DRem = 0x73,
    INeg = 0x74,
    LNeg = 0x75,
    FNeg = 0x76,
    DNeg = 0x77,
    IShl = 0x78,
    LShl = 0x79,
    IShr = 0x7A,
    LShr = 0x7B,
    IUshr = 0x7C,
    LUshr = 0x7D,
    IAnd = 0x7E,
    LAnd = 0x7F,
    IOr = 0x80,
    LOr = 0x81,
    IXor = 0x82,
    LXor = 0x83,
    IInc(u8, i8) = 0x84,
    I2L = 0x85,
    I2F = 0x86,
    I2D = 0x87,
    L2I = 0x88,
    L2F = 0x89,
    L2D = 0x8A,
    F2I = 0x8B,
    F2L = 0x8C,
    F2D = 0x8D,
    D2I = 0x8E,
    D2L = 0x8F,
    D2F = 0x90,
    I2B = 0x91,
    I2C = 0x92,
    I2S = 0x93,
    LCmp = 0x94,
    FCmpL = 0x95,
    FCmpG = 0x96,
    DCmpL = 0x97,
    DCmpG = 0x98,
    IfEq(i16) = 0x99,
    IfNe(i16) = 0x9A,
    IfLt(i16) = 0x9B,
    IfGe(i16) = 0x9C,
    IfGt(i16) = 0x9D,
    IfLe(i16) = 0x9E,
    IfICmpEq(i16) = 0x9F,
    IfICmpNe(i16) = 0xA0,
    IfICmpLt(i16) = 0xA1,
    IfICmpGe(i16) = 0xA2,
    IfICmpGt(i16) = 0xA3,
    IfICmpLe(i16) = 0xA4,
    IfACmpEq(i16) = 0xA5,
    IfACmpNe(i16) = 0xA6,
    Goto(i16) = 0xA7,
    Jsr(i16) = 0xA8,
    Ret(u8) = 0xA9,
    TableSwitch(i32, i32, i32, Vec<i32>)= 0xAA,
    LookupSwitch(i32, Vec<(i32, i32)>) = 0xAB,
    IReturn = 0xAC,
    LReturn = 0xAD,
    FReturn = 0xAE,
    DReturn = 0xAF,
    AReturn = 0xB0,
    Return = 0xB1,
    GetStatic(u16) = 0xB2,
    PutStatic(u16) = 0xB3,
    GetField(u16) = 0xB4,
    PutField(u16) = 0xB5,
    InvokeVirtual(u16) = 0xB6,
    InvokeSpecial(u16) = 0xB7,
    InvokeStatic(u16) = 0xB8,
    InvokeInterface(u16, u8) = 0xB9,
    InvokeDynamic(u16) = 0xBA,
    New(u16) = 0xBB,
    NewArray(u8) = 0xBC,
    ANewArray(u16) = 0xBD,
    ArrayLength = 0xBE,
    AThrow = 0xBF,
    CheckCast(u16) = 0xC0,
    InstanceOf(u16) = 0xC1,
    MonitorEnter = 0xC2,
    MonitorExit = 0xC3,
    Wide(u8, u16, Option<u16>) = 0xC4,
    MultiANewArray(u16, u8) = 0xC5,
    IfNull(i16) = 0xC6,
    IfNonNull(i16) = 0xC7,
    GotoW(u32) = 0xC8,
    JsrW(u32) = 0xC9,
}

const SIZE: usize = std::mem::size_of::<Instruction>();

pub trait JSwitchPadder {
    fn pad(&mut self) -> Result<(), std::io::Error>;
}

/// A Reader for instructions inside a method.
///
/// Each reader must be initialized with `start_idx` at the start of a method definition in a java class.
///
/// You should use a BufReader for files, etc. A Cursor for readers that don't implement Seek.
pub struct InstructionReader<T> {
    start_idx: u64,
    inner: T,
}

impl<T: Seek> InstructionReader<T> {
    pub fn new(mut value: T) -> std::io::Result<Self> {
        let pos = value.stream_position()?;
        Ok(InstructionReader {
            start_idx: pos,
            inner: value,
        })
    }
}

impl<T: Seek> JSwitchPadder for InstructionReader<T> {
    fn pad(&mut self) -> Result<(), std::io::Error> {
        let diff = self.stream_position()? - self.start_idx;
        self.seek(SeekFrom::Current(((4 - (diff % 4)) & 3) as i64))?;
        Ok(())
    }
}

impl<T: Seek + Read> InstructionReader<T> {
    pub fn read_insn(&mut self) -> Result<Instruction, Error> {
        InstructionRead::read_insn(self, JSwitchPadder::pad)
    }
}

use delegate::delegate;

// noinspection RsTraitImplementation
impl<T: Read> Read for InstructionReader<T> {
    delegate! {
        to self.inner {
            fn read(&mut self, buf: &mut [u8]) -> Result<usize, std::io::Error>;
            fn read_to_end(&mut self, buf: &mut Vec<u8>) -> Result<usize, std::io::Error>;
        }
    }
}

// noinspection RsTraitImplementation
impl<T: Seek> Seek for InstructionReader<T> {
    delegate! {
        to self.inner {
            fn seek(&mut self, pos: SeekFrom) -> std::io::Result<u64>;
            fn stream_len(&mut self) -> std::io::Result<u64>;
            fn stream_position(&mut self) -> std::io::Result<u64>;
        }
    }
}

pub trait InstructionRead: Read + Sized {
    /// Uses transmute to construct enums with no fields
    ///
    /// pad_func is a function where when called, will read a 0-3 byte padding that will align the current position to a multiple of 4 from the starting position of the instructions.
    ///
    /// ```
    /// # use std::io::Cursor;
    /// # use crate::coffer::insn::InstructionRead;
    /// # use coffer::insn::Instruction;
    /// # use coffer::constants::insn::*;
    /// let mut slice = [ARETURN, LDC, 1];
    /// # let mut cursor = Cursor::new(slice);
    /// assert_eq!((&mut cursor).read_insn(|_|(Ok(()))).unwrap(), Instruction::AReturn);
    /// assert_eq!((&mut cursor).read_insn(|_|(Ok(()))).unwrap(), Instruction::LDC(1));
    /// ```
    fn read_insn<F>(&mut self, mut pad_func: F) -> Result<Instruction, Error> where F: FnMut(&mut Self) -> Result<(), std::io::Error> {
        fn transmute_slice<T>(opcode: u8, mut fun: T) -> Result<Instruction, Error> where T: FnMut(Cursor<&mut [u8]>) -> Result<(), Error> {
            let mut slice = [0u8; SIZE];
            slice[0] = opcode;
            let mut cursor = Cursor::new(&mut slice[..SIZE]);
            cursor.set_position(1);
            fun(cursor)?;
            // We know it is safe to transmute because the opcode and their variants are known
            Ok(unsafe {
                std::mem::transmute::<[u8; SIZE], Instruction>(slice)
            })
        }
        use crate::constants::insn::*;
        let op = self.read_u8()?;
        Ok(match op {
            ARRAYLENGTH | ATHROW | MONITORENTER | MONITOREXIT | IRETURN..=RETURN | I2L..=DCMPG | ISTORE_0..=LXOR | ILOAD_0..=SALOAD | NOP..=DCONST_1 => {
                transmute_slice(op, |_| Ok(()))?
            }
            LDC | ILOAD..=ALOAD | ISTORE..=ASTORE | RET | NEWARRAY | BIPUSH => {
                transmute_slice(op, |mut c| {
                    c.write_all(&[self.read_u8()?])?;
                    Ok(())
                })?
            }
            // i16 and u16 are essentially the same
            // IINC is here as well
            IFEQ..=JSR | GETSTATIC..=INVOKESTATIC | INVOKEDYNAMIC | NEW | ANEWARRAY | CHECKCAST | INSTANCEOF | IFNULL | IFNONNULL | SIPUSH | LDC_W | LDC2_W | IINC | INVOKEINTERFACE | MULTIANEWARRAY => {
                transmute_slice(op, |mut c| {
                    if op != IINC { // IINC does not need alignment, others are 16bit values so they will need the padding
                        c.seek(SeekFrom::Current(1))?;
                    }
                    c.write_all(&self.read_u16()?.to_ne_bytes())?;
                    match op {
                        INVOKEINTERFACE => {
                            c.write_all(&[self.read_u8()?])?;
                            self.read_u8()?;
                        }
                        MULTIANEWARRAY => {
                            c.write_all(&[self.read_u8()?])?;
                        }
                        INVOKEDYNAMIC => {
                            self.read_u16()?;
                        }
                        _ => {}
                    }
                    Ok(())
                })?
            }
            GOTO_W | JSR_W => {
                transmute_slice(op, |mut c| {
                    c.seek(SeekFrom::Current(3))?; // alignment padding
                    c.write_all(&self.read_u32()?.to_ne_bytes())?;
                    Ok(())
                })?
            }
            WIDE => {
                transmute_slice(op, |mut c| {
                    c.write_all(&[self.read_u8()?])?;
                    c.write_all(&self.read_u16()?.to_ne_bytes())?;
                    if op == IINC {
                        c.write_all(&[1, 0])?; // Option::Some tag with alignment padding
                        c.write_all(&self.read_i16()?.to_ne_bytes())?;
                    }
                    Ok(())
                })?
            }
            LOOKUPSWITCH => {
                pad_func(self)?;
                let default = self.read_i32()?;
                let npairs = self.read_i32()?;
                let mut vec = Vec::with_capacity(npairs as usize);
                for _ in 0..npairs {
                    let match_ = self.read_i32()?;
                    let offset = self.read_i32()?;
                    vec.push((match_, offset))
                }
                LookupSwitch(default, vec)
            }
            TABLESWITCH => {
                pad_func(self)?;
                let default = self.read_i32()?;
                let low = self.read_i32()?;
                let high = self.read_i32()?;
                let n = high - low + 1;
                let mut vec = Vec::with_capacity(n as usize);
                for _ in 0..n {
                    vec.push(self.read_i32()?)
                }
                TableSwitch(default, low, high, vec)
            }
            _ => {
                return Err(Error::Invalid("opcode", op.to_string().into()));
            }
        })
    }
}

impl<T: Read + Sized> InstructionRead for T {}

pub trait InstructionWrite: Write + Sized {
    fn write_insn<F>(&mut self, insn: &Instruction, mut pad_func: F) -> Result<(), std::io::Error> where F: FnMut(&mut Self) -> Result<(), std::io::Error> {
        use crate::insn::Instruction::*;
        self.write_all(&[unsafe { *(self as *const Self as *const u8) }])?;
        match insn {
            Bipush(b) => {
                self.write_all(&[*b as u8])?;
            }
            LDC(b) | ILoad(b) | LLoad(b) | FLoad(b) | DLoad(b) | ALoad(b) | IStore(b) | LStore(b) | FStore(b) | DStore(b) | AStore(b) | Ret(b) | NewArray(b) => {
                self.write_all(&[*b])?;
            }
            Sipush(s) | IfEq(s) | IfNe(s) | IfLt(s) | IfGe(s) | IfGt(s) | IfLe(s) | IfICmpEq(s) | IfICmpNe(s) | IfICmpLt(s) | IfICmpGe(s) | IfICmpGt(s) | IfICmpLe(s) | IfACmpEq(s) | IfACmpNe(s) | Goto(s) | Jsr(s) | IfNull(s) | IfNonNull(s) => {
                self.write_all(&s.to_be_bytes())?;
            }
            LDCW(s) | LDC2W(s) | GetStatic(s) | PutStatic(s) | GetField(s) | PutField(s) | InvokeVirtual(s) | InvokeSpecial(s) | InvokeStatic(s) | InvokeDynamic(s) | New(s) | ANewArray(s) | CheckCast(s) | InstanceOf(s) => {
                self.write_all(&s.to_be_bytes())?;
                if let InvokeDynamic(_) = insn {
                    self.write_all(&[0, 0])?;
                }
            }
            MultiANewArray(s, b) | InvokeInterface(s, b) => {
                self.write_all(&s.to_be_bytes())?;
                self.write_all(&[*b])?;
                if let InvokeInterface(..) = insn {
                    self.write_all(&[0])?;
                }
            }
            Wide(op, arg, arg2) => {
                self.write_all(&[*op])?;
                self.write_all(&arg.to_be_bytes())?;
                if let Some(s) = arg2 {
                    self.write_all(&s.to_be_bytes())?;
                }
            }
            IInc(b1, b2) => {
                self.write_all(&[*b1, *b2 as u8])?;
            }
            GotoW(i) | JsrW(i) => {
                self.write_all(&i.to_be_bytes())?;
            }
            TableSwitch(dflt, low, high, offsets) => {
                pad_func(self)?;
                self.write_all(&dflt.to_be_bytes())?;
                self.write_all(&low.to_be_bytes())?;
                self.write_all(&high.to_be_bytes())?;
                for off in offsets {
                    self.write_all(&off.to_be_bytes())?;
                }
            }
            LookupSwitch(dflt, pairs) => {
                pad_func(self)?;
                self.write_all(&dflt.to_be_bytes())?;
                self.write_all(&(pairs.len() as u32).to_be_bytes())?;
                for (i, off) in pairs {
                    self.write_all(&i.to_be_bytes())?;
                    self.write_all(&off.to_be_bytes())?;
                }
            }
            Nop | AConstNull | IConstM1 | IConst0 | IConst1 | IConst2 | IConst3 | IConst4 | IConst5 | LConst0 | LConst1 | FConst0 | FConst1 | FConst2 | DConst0 | DConst1 | ILoad0 | ILoad1 | ILoad2 | ILoad3 | LLoad0 | LLoad1 | LLoad2 | LLoad3 | FLoad0 | FLoad1 | FLoad2 | FLoad3 | DLoad0 | DLoad1 | DLoad2 | DLoad3 | ALoad0 | ALoad1 | ALoad2 | ALoad3 | IALoad | LALoad | FALoad | DALoad | AALoad | BALoad | CALoad | SALoad | IStore0 | IStore1 | IStore2 | IStore3 | LStore0 | LStore1 | LStore2 | LStore3 | FStore0 | FStore1 | FStore2 | FStore3 | DStore0 | DStore1 | DStore2 | DStore3 | AStore0 | AStore1 | AStore2 | AStore3 | IAStore | LAStore | FAStore | DAStore | AAStore | BAStore | CAStore | SAStore | Pop | Pop2 | Dup | Dupx1 | Dupx2 | Dup2 | Dup2x1 | Dup2x2 | Swap | IAdd | LAdd | FAdd | DAdd | ISub | LSub | FSub | DSub | IMul | LMul | FMul | DMul | IDiv | LDiv | FDiv | DDiv | IRem | LRem | FRem | DRem | INeg | LNeg | FNeg | DNeg | IShl | LShl | IShr | LShr | IUshr | LUshr | IAnd | LAnd | IOr | LOr | IXor | LXor | I2L | I2F | I2D | L2I | L2F | L2D | F2I | F2L | F2D | D2I | D2L | D2F | I2B | I2C | I2S | LCmp | FCmpL | FCmpG | DCmpL | DCmpG | IReturn | LReturn | FReturn | DReturn | AReturn | Return | ArrayLength | AThrow | MonitorEnter | MonitorExit => {}
        }
        Ok(())
    }
}