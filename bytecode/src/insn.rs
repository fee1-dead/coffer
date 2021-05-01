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
//! Used for translating raw code to useful code.

use crate::{ReadWrite, Result};

#[derive(Copy, Clone, Debug, Eq, PartialEq, ReadWrite)]
pub(crate) struct SwitchEntry(pub i32, pub i32);

#[derive(Copy, Clone, Debug, Eq, PartialEq, ReadWrite)]
#[tag_type(u8)]
pub(crate) enum Wide {
    #[tag(0x15)]
    ILoad(u16),
    LLoad(u16),
    FLoad(u16),
    DLoad(u16),
    ALoad(u16),
    #[tag(0x36)]
    IStore(u16),
    LStore(u16),
    FStore(u16),
    DStore(u16),
    AStore(u16),
    #[tag(0x84)]
    IInc(u16, i16),
    #[tag(0xA9)]
    Ret(u16),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct TableSwitch {
    pub low: i32,
    pub high: i32,
    pub offsets: Vec<i32>,
}

impl ReadWrite for TableSwitch {
    fn read_from<T: std::io::Read>(reader: &mut T) -> Result<Self> {
        let low = i32::read_from(reader)?;
        let high = i32::read_from(reader)?;
        let len = (high - low + 1) as usize;
        let mut offsets = Vec::with_capacity(len);
        for _ in 0..len {
            offsets.push(i32::read_from(reader)?);
        }
        Ok(Self { low, high, offsets })
    }

    fn write_to<T: std::io::Write>(&self, writer: &mut T) -> Result<()> {
        self.low.write_to(writer)?;
        self.high.write_to(writer)?;
        for off in &self.offsets {
            off.write_to(writer)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, ReadWrite)]
#[tag_type(u8)]
pub(crate) enum Instruction {
    #[tag(0x00)]
    Nop,
    #[tag(0x01)]
    AConstNull,
    #[tag(0x02)]
    IConstM1,
    #[tag(0x03)]
    IConst0,
    #[tag(0x04)]
    IConst1,
    #[tag(0x05)]
    IConst2,
    #[tag(0x06)]
    IConst3,
    #[tag(0x07)]
    IConst4,
    #[tag(0x08)]
    IConst5,
    #[tag(0x09)]
    LConst0,
    #[tag(0x0A)]
    LConst1,
    #[tag(0x0B)]
    FConst0,
    #[tag(0x0C)]
    FConst1,
    #[tag(0x0D)]
    FConst2,
    #[tag(0x0E)]
    DConst0,
    #[tag(0x0F)]
    DConst1,
    #[tag(0x10)]
    Bipush(i8),
    #[tag(0x11)]
    Sipush(i16),
    #[tag(0x12)]
    Ldc(u8),
    #[tag(0x13)]
    LdcW(u16),
    #[tag(0x14)]
    Ldc2W(u16),
    #[tag(0x15)]
    ILoad(u8),
    #[tag(0x16)]
    LLoad(u8),
    #[tag(0x17)]
    FLoad(u8),
    #[tag(0x18)]
    DLoad(u8),
    #[tag(0x19)]
    ALoad(u8),
    #[tag(0x1A)]
    ILoad0,
    #[tag(0x1B)]
    ILoad1,
    #[tag(0x1C)]
    ILoad2,
    #[tag(0x1D)]
    ILoad3,
    #[tag(0x1E)]
    LLoad0,
    #[tag(0x1F)]
    LLoad1,
    #[tag(0x20)]
    LLoad2,
    #[tag(0x21)]
    LLoad3,
    #[tag(0x22)]
    FLoad0,
    #[tag(0x23)]
    FLoad1,
    #[tag(0x24)]
    FLoad2,
    #[tag(0x25)]
    FLoad3,
    #[tag(0x26)]
    DLoad0,
    #[tag(0x27)]
    DLoad1,
    #[tag(0x28)]
    DLoad2,
    #[tag(0x29)]
    DLoad3,
    #[tag(0x2A)]
    ALoad0,
    #[tag(0x2B)]
    ALoad1,
    #[tag(0x2C)]
    ALoad2,
    #[tag(0x2D)]
    ALoad3,
    #[tag(0x2E)]
    IALoad,
    #[tag(0x2F)]
    LALoad,
    #[tag(0x30)]
    FALoad,
    #[tag(0x31)]
    DALoad,
    #[tag(0x32)]
    AALoad,
    #[tag(0x33)]
    BALoad,
    #[tag(0x34)]
    CALoad,
    #[tag(0x35)]
    SALoad,
    #[tag(0x36)]
    IStore(u8),
    #[tag(0x37)]
    LStore(u8),
    #[tag(0x38)]
    FStore(u8),
    #[tag(0x39)]
    DStore(u8),
    #[tag(0x3A)]
    AStore(u8),
    #[tag(0x3B)]
    IStore0,
    #[tag(0x3C)]
    IStore1,
    #[tag(0x3D)]
    IStore2,
    #[tag(0x3E)]
    IStore3,
    #[tag(0x3F)]
    LStore0,
    #[tag(0x40)]
    LStore1,
    #[tag(0x41)]
    LStore2,
    #[tag(0x42)]
    LStore3,
    #[tag(0x43)]
    FStore0,
    #[tag(0x44)]
    FStore1,
    #[tag(0x45)]
    FStore2,
    #[tag(0x46)]
    FStore3,
    #[tag(0x47)]
    DStore0,
    #[tag(0x48)]
    DStore1,
    #[tag(0x49)]
    DStore2,
    #[tag(0x4A)]
    DStore3,
    #[tag(0x4B)]
    AStore0,
    #[tag(0x4C)]
    AStore1,
    #[tag(0x4D)]
    AStore2,
    #[tag(0x4E)]
    AStore3,
    #[tag(0x4F)]
    IAStore,
    #[tag(0x50)]
    LAStore,
    #[tag(0x51)]
    FAStore,
    #[tag(0x52)]
    DAStore,
    #[tag(0x53)]
    AAStore,
    #[tag(0x54)]
    BAStore,
    #[tag(0x55)]
    CAStore,
    #[tag(0x56)]
    SAStore,
    #[tag(0x57)]
    Pop,
    #[tag(0x58)]
    Pop2,
    #[tag(0x59)]
    Dup,
    #[tag(0x5A)]
    Dupx1,
    #[tag(0x5B)]
    Dupx2,
    #[tag(0x5C)]
    Dup2,
    #[tag(0x5D)]
    Dup2x1,
    #[tag(0x5E)]
    Dup2x2,
    #[tag(0x5F)]
    Swap,
    #[tag(0x60)]
    IAdd,
    #[tag(0x61)]
    LAdd,
    #[tag(0x62)]
    FAdd,
    #[tag(0x63)]
    DAdd,
    #[tag(0x64)]
    ISub,
    #[tag(0x65)]
    LSub,
    #[tag(0x66)]
    FSub,
    #[tag(0x67)]
    DSub,
    #[tag(0x68)]
    IMul,
    #[tag(0x69)]
    LMul,
    #[tag(0x6A)]
    FMul,
    #[tag(0x6B)]
    DMul,
    #[tag(0x6C)]
    IDiv,
    #[tag(0x6D)]
    LDiv,
    #[tag(0x6E)]
    FDiv,
    #[tag(0x6F)]
    DDiv,
    #[tag(0x70)]
    IRem,
    #[tag(0x71)]
    LRem,
    #[tag(0x72)]
    FRem,
    #[tag(0x73)]
    DRem,
    #[tag(0x74)]
    INeg,
    #[tag(0x75)]
    LNeg,
    #[tag(0x76)]
    FNeg,
    #[tag(0x77)]
    DNeg,
    #[tag(0x78)]
    IShl,
    #[tag(0x79)]
    LShl,
    #[tag(0x7A)]
    IShr,
    #[tag(0x7B)]
    LShr,
    #[tag(0x7C)]
    IUshr,
    #[tag(0x7D)]
    LUshr,
    #[tag(0x7E)]
    IAnd,
    #[tag(0x7F)]
    LAnd,
    #[tag(0x80)]
    IOr,
    #[tag(0x81)]
    LOr,
    #[tag(0x82)]
    IXor,
    #[tag(0x83)]
    LXor,
    #[tag(0x84)]
    IInc(u8, i8),
    #[tag(0x85)]
    I2L,
    #[tag(0x86)]
    I2F,
    #[tag(0x87)]
    I2D,
    #[tag(0x88)]
    L2I,
    #[tag(0x89)]
    L2F,
    #[tag(0x8A)]
    L2D,
    #[tag(0x8B)]
    F2I,
    #[tag(0x8C)]
    F2L,
    #[tag(0x8D)]
    F2D,
    #[tag(0x8E)]
    D2I,
    #[tag(0x8F)]
    D2L,
    #[tag(0x90)]
    D2F,
    #[tag(0x91)]
    I2B,
    #[tag(0x92)]
    I2C,
    #[tag(0x93)]
    I2S,
    #[tag(0x94)]
    LCmp,
    #[tag(0x95)]
    FCmpL,
    #[tag(0x96)]
    FCmpG,
    #[tag(0x97)]
    DCmpL,
    #[tag(0x98)]
    DCmpG,
    #[tag(0x99)]
    IfEq(i16),
    #[tag(0x9A)]
    IfNe(i16),
    #[tag(0x9B)]
    IfLt(i16),
    #[tag(0x9C)]
    IfGe(i16),
    #[tag(0x9D)]
    IfGt(i16),
    #[tag(0x9E)]
    IfLe(i16),
    #[tag(0x9F)]
    IfICmpEq(i16),
    #[tag(0xA0)]
    IfICmpNe(i16),
    #[tag(0xA1)]
    IfICmpLt(i16),
    #[tag(0xA2)]
    IfICmpGe(i16),
    #[tag(0xA3)]
    IfICmpGt(i16),
    #[tag(0xA4)]
    IfICmpLe(i16),
    #[tag(0xA5)]
    IfACmpEq(i16),
    #[tag(0xA6)]
    IfACmpNe(i16),
    #[tag(0xA7)]
    Goto(i16),
    #[tag(0xA8)]
    Jsr(i16),
    #[tag(0xA9)]
    Ret(u8),
    #[tag(0xAA)]
    TableSwitch(i32, TableSwitch),
    #[tag(0xAB)]
    LookupSwitch(i32, #[vec_len_type(u32)] Vec<SwitchEntry>),
    #[tag(0xAC)]
    IReturn,
    #[tag(0xAD)]
    LReturn,
    #[tag(0xAE)]
    FReturn,
    #[tag(0xAF)]
    DReturn,
    #[tag(0xB0)]
    AReturn,
    #[tag(0xB1)]
    Return,
    #[tag(0xB2)]
    GetStatic(u16),
    #[tag(0xB3)]
    PutStatic(u16),
    #[tag(0xB4)]
    GetField(u16),
    #[tag(0xB5)]
    PutField(u16),
    #[tag(0xB6)]
    InvokeVirtual(u16),
    #[tag(0xB7)]
    InvokeSpecial(u16),
    #[tag(0xB8)]
    InvokeStatic(u16),
    #[tag(0xB9)]
    InvokeInterface(u16, u8, u8),
    #[tag(0xBA)]
    InvokeDynamic(u16, u16),
    #[tag(0xBB)]
    New(u16),
    #[tag(0xBC)]
    NewArray(u8),
    #[tag(0xBD)]
    ANewArray(u16),
    #[tag(0xBE)]
    ArrayLength,
    #[tag(0xBF)]
    AThrow,
    #[tag(0xC0)]
    CheckCast(u16),
    #[tag(0xC1)]
    InstanceOf(u16),
    #[tag(0xC2)]
    MonitorEnter,
    #[tag(0xC3)]
    MonitorExit,
    #[tag(0xC4)]
    Wide(Wide),
    #[tag(0xC5)]
    MultiANewArray(u16, u8),
    #[tag(0xC6)]
    IfNull(i16),
    #[tag(0xC7)]
    IfNonNull(i16),
    #[tag(0xC8)]
    GotoW(i32),
    #[tag(0xC9)]
    JsrW(i32),
}
