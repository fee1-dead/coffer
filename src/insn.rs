//! Basic Instruction Reading.
//!
//! Used for translating raw code to useful code.

use crate::{ReadWrite, Result};
use crate::helper as h;

#[derive(Copy, Clone, Debug, Eq, PartialEq, ReadWrite)]
pub(crate) struct SwitchEntry(pub i32, pub i32);

#[derive(Copy, Clone, Debug, Eq, PartialEq, ReadWrite)]
#[coffer(tag_type(u8))]
pub(crate) enum Wide {
    #[coffer(tag = 0x15)]
    ILoad(u16),
    LLoad(u16),
    FLoad(u16),
    DLoad(u16),
    ALoad(u16),
    #[coffer(tag = 0x36)]
    IStore(u16),
    LStore(u16),
    FStore(u16),
    DStore(u16),
    AStore(u16),
    #[coffer(tag = 0x84)]
    IInc(u16, i16),
    #[coffer(tag = 0xA9)]
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
#[coffer(tag_type(u8))]
pub(crate) enum Instruction {
    #[coffer(tag = 0x00)]
    Nop,
    #[coffer(tag = 0x01)]
    AConstNull,
    #[coffer(tag = 0x02)]
    IConstM1,
    #[coffer(tag = 0x03)]
    IConst0,
    #[coffer(tag = 0x04)]
    IConst1,
    #[coffer(tag = 0x05)]
    IConst2,
    #[coffer(tag = 0x06)]
    IConst3,
    #[coffer(tag = 0x07)]
    IConst4,
    #[coffer(tag = 0x08)]
    IConst5,
    #[coffer(tag = 0x09)]
    LConst0,
    #[coffer(tag = 0x0A)]
    LConst1,
    #[coffer(tag = 0x0B)]
    FConst0,
    #[coffer(tag = 0x0C)]
    FConst1,
    #[coffer(tag = 0x0D)]
    FConst2,
    #[coffer(tag = 0x0E)]
    DConst0,
    #[coffer(tag = 0x0F)]
    DConst1,
    #[coffer(tag = 0x10)]
    Bipush(i8),
    #[coffer(tag = 0x11)]
    Sipush(i16),
    #[coffer(tag = 0x12)]
    Ldc(u8),
    #[coffer(tag = 0x13)]
    LdcW(u16),
    #[coffer(tag = 0x14)]
    Ldc2W(u16),
    #[coffer(tag = 0x15)]
    ILoad(u8),
    #[coffer(tag = 0x16)]
    LLoad(u8),
    #[coffer(tag = 0x17)]
    FLoad(u8),
    #[coffer(tag = 0x18)]
    DLoad(u8),
    #[coffer(tag = 0x19)]
    ALoad(u8),
    #[coffer(tag = 0x1A)]
    ILoad0,
    #[coffer(tag = 0x1B)]
    ILoad1,
    #[coffer(tag = 0x1C)]
    ILoad2,
    #[coffer(tag = 0x1D)]
    ILoad3,
    #[coffer(tag = 0x1E)]
    LLoad0,
    #[coffer(tag = 0x1F)]
    LLoad1,
    #[coffer(tag = 0x20)]
    LLoad2,
    #[coffer(tag = 0x21)]
    LLoad3,
    #[coffer(tag = 0x22)]
    FLoad0,
    #[coffer(tag = 0x23)]
    FLoad1,
    #[coffer(tag = 0x24)]
    FLoad2,
    #[coffer(tag = 0x25)]
    FLoad3,
    #[coffer(tag = 0x26)]
    DLoad0,
    #[coffer(tag = 0x27)]
    DLoad1,
    #[coffer(tag = 0x28)]
    DLoad2,
    #[coffer(tag = 0x29)]
    DLoad3,
    #[coffer(tag = 0x2A)]
    ALoad0,
    #[coffer(tag = 0x2B)]
    ALoad1,
    #[coffer(tag = 0x2C)]
    ALoad2,
    #[coffer(tag = 0x2D)]
    ALoad3,
    #[coffer(tag = 0x2E)]
    IALoad,
    #[coffer(tag = 0x2F)]
    LALoad,
    #[coffer(tag = 0x30)]
    FALoad,
    #[coffer(tag = 0x31)]
    DALoad,
    #[coffer(tag = 0x32)]
    AALoad,
    #[coffer(tag = 0x33)]
    BALoad,
    #[coffer(tag = 0x34)]
    CALoad,
    #[coffer(tag = 0x35)]
    SALoad,
    #[coffer(tag = 0x36)]
    IStore(u8),
    #[coffer(tag = 0x37)]
    LStore(u8),
    #[coffer(tag = 0x38)]
    FStore(u8),
    #[coffer(tag = 0x39)]
    DStore(u8),
    #[coffer(tag = 0x3A)]
    AStore(u8),
    #[coffer(tag = 0x3B)]
    IStore0,
    #[coffer(tag = 0x3C)]
    IStore1,
    #[coffer(tag = 0x3D)]
    IStore2,
    #[coffer(tag = 0x3E)]
    IStore3,
    #[coffer(tag = 0x3F)]
    LStore0,
    #[coffer(tag = 0x40)]
    LStore1,
    #[coffer(tag = 0x41)]
    LStore2,
    #[coffer(tag = 0x42)]
    LStore3,
    #[coffer(tag = 0x43)]
    FStore0,
    #[coffer(tag = 0x44)]
    FStore1,
    #[coffer(tag = 0x45)]
    FStore2,
    #[coffer(tag = 0x46)]
    FStore3,
    #[coffer(tag = 0x47)]
    DStore0,
    #[coffer(tag = 0x48)]
    DStore1,
    #[coffer(tag = 0x49)]
    DStore2,
    #[coffer(tag = 0x4A)]
    DStore3,
    #[coffer(tag = 0x4B)]
    AStore0,
    #[coffer(tag = 0x4C)]
    AStore1,
    #[coffer(tag = 0x4D)]
    AStore2,
    #[coffer(tag = 0x4E)]
    AStore3,
    #[coffer(tag = 0x4F)]
    IAStore,
    #[coffer(tag = 0x50)]
    LAStore,
    #[coffer(tag = 0x51)]
    FAStore,
    #[coffer(tag = 0x52)]
    DAStore,
    #[coffer(tag = 0x53)]
    AAStore,
    #[coffer(tag = 0x54)]
    BAStore,
    #[coffer(tag = 0x55)]
    CAStore,
    #[coffer(tag = 0x56)]
    SAStore,
    #[coffer(tag = 0x57)]
    Pop,
    #[coffer(tag = 0x58)]
    Pop2,
    #[coffer(tag = 0x59)]
    Dup,
    #[coffer(tag = 0x5A)]
    Dupx1,
    #[coffer(tag = 0x5B)]
    Dupx2,
    #[coffer(tag = 0x5C)]
    Dup2,
    #[coffer(tag = 0x5D)]
    Dup2x1,
    #[coffer(tag = 0x5E)]
    Dup2x2,
    #[coffer(tag = 0x5F)]
    Swap,
    #[coffer(tag = 0x60)]
    IAdd,
    #[coffer(tag = 0x61)]
    LAdd,
    #[coffer(tag = 0x62)]
    FAdd,
    #[coffer(tag = 0x63)]
    DAdd,
    #[coffer(tag = 0x64)]
    ISub,
    #[coffer(tag = 0x65)]
    LSub,
    #[coffer(tag = 0x66)]
    FSub,
    #[coffer(tag = 0x67)]
    DSub,
    #[coffer(tag = 0x68)]
    IMul,
    #[coffer(tag = 0x69)]
    LMul,
    #[coffer(tag = 0x6A)]
    FMul,
    #[coffer(tag = 0x6B)]
    DMul,
    #[coffer(tag = 0x6C)]
    IDiv,
    #[coffer(tag = 0x6D)]
    LDiv,
    #[coffer(tag = 0x6E)]
    FDiv,
    #[coffer(tag = 0x6F)]
    DDiv,
    #[coffer(tag = 0x70)]
    IRem,
    #[coffer(tag = 0x71)]
    LRem,
    #[coffer(tag = 0x72)]
    FRem,
    #[coffer(tag = 0x73)]
    DRem,
    #[coffer(tag = 0x74)]
    INeg,
    #[coffer(tag = 0x75)]
    LNeg,
    #[coffer(tag = 0x76)]
    FNeg,
    #[coffer(tag = 0x77)]
    DNeg,
    #[coffer(tag = 0x78)]
    IShl,
    #[coffer(tag = 0x79)]
    LShl,
    #[coffer(tag = 0x7A)]
    IShr,
    #[coffer(tag = 0x7B)]
    LShr,
    #[coffer(tag = 0x7C)]
    IUshr,
    #[coffer(tag = 0x7D)]
    LUshr,
    #[coffer(tag = 0x7E)]
    IAnd,
    #[coffer(tag = 0x7F)]
    LAnd,
    #[coffer(tag = 0x80)]
    IOr,
    #[coffer(tag = 0x81)]
    LOr,
    #[coffer(tag = 0x82)]
    IXor,
    #[coffer(tag = 0x83)]
    LXor,
    #[coffer(tag = 0x84)]
    IInc(u8, i8),
    #[coffer(tag = 0x85)]
    I2L,
    #[coffer(tag = 0x86)]
    I2F,
    #[coffer(tag = 0x87)]
    I2D,
    #[coffer(tag = 0x88)]
    L2I,
    #[coffer(tag = 0x89)]
    L2F,
    #[coffer(tag = 0x8A)]
    L2D,
    #[coffer(tag = 0x8B)]
    F2I,
    #[coffer(tag = 0x8C)]
    F2L,
    #[coffer(tag = 0x8D)]
    F2D,
    #[coffer(tag = 0x8E)]
    D2I,
    #[coffer(tag = 0x8F)]
    D2L,
    #[coffer(tag = 0x90)]
    D2F,
    #[coffer(tag = 0x91)]
    I2B,
    #[coffer(tag = 0x92)]
    I2C,
    #[coffer(tag = 0x93)]
    I2S,
    #[coffer(tag = 0x94)]
    LCmp,
    #[coffer(tag = 0x95)]
    FCmpL,
    #[coffer(tag = 0x96)]
    FCmpG,
    #[coffer(tag = 0x97)]
    DCmpL,
    #[coffer(tag = 0x98)]
    DCmpG,
    #[coffer(tag = 0x99)]
    IfEq(i16),
    #[coffer(tag = 0x9A)]
    IfNe(i16),
    #[coffer(tag = 0x9B)]
    IfLt(i16),
    #[coffer(tag = 0x9C)]
    IfGe(i16),
    #[coffer(tag = 0x9D)]
    IfGt(i16),
    #[coffer(tag = 0x9E)]
    IfLe(i16),
    #[coffer(tag = 0x9F)]
    IfICmpEq(i16),
    #[coffer(tag = 0xA0)]
    IfICmpNe(i16),
    #[coffer(tag = 0xA1)]
    IfICmpLt(i16),
    #[coffer(tag = 0xA2)]
    IfICmpGe(i16),
    #[coffer(tag = 0xA3)]
    IfICmpGt(i16),
    #[coffer(tag = 0xA4)]
    IfICmpLe(i16),
    #[coffer(tag = 0xA5)]
    IfACmpEq(i16),
    #[coffer(tag = 0xA6)]
    IfACmpNe(i16),
    #[coffer(tag = 0xA7)]
    Goto(i16),
    #[coffer(tag = 0xA8)]
    Jsr(i16),
    #[coffer(tag = 0xA9)]
    Ret(u8),
    #[coffer(tag = 0xAA)]
    TableSwitch(i32, TableSwitch),
    #[coffer(tag = 0xAB)]
    LookupSwitch(i32, #[coffer(as = "h::Vec32")] Vec<SwitchEntry>),
    #[coffer(tag = 0xAC)]
    IReturn,
    #[coffer(tag = 0xAD)]
    LReturn,
    #[coffer(tag = 0xAE)]
    FReturn,
    #[coffer(tag = 0xAF)]
    DReturn,
    #[coffer(tag = 0xB0)]
    AReturn,
    #[coffer(tag = 0xB1)]
    Return,
    #[coffer(tag = 0xB2)]
    GetStatic(u16),
    #[coffer(tag = 0xB3)]
    PutStatic(u16),
    #[coffer(tag = 0xB4)]
    GetField(u16),
    #[coffer(tag = 0xB5)]
    PutField(u16),
    #[coffer(tag = 0xB6)]
    InvokeVirtual(u16),
    #[coffer(tag = 0xB7)]
    InvokeSpecial(u16),
    #[coffer(tag = 0xB8)]
    InvokeStatic(u16),
    #[coffer(tag = 0xB9)]
    InvokeInterface(u16, u8, u8),
    #[coffer(tag = 0xBA)]
    InvokeDynamic(u16, u16),
    #[coffer(tag = 0xBB)]
    New(u16),
    #[coffer(tag = 0xBC)]
    NewArray(u8),
    #[coffer(tag = 0xBD)]
    ANewArray(u16),
    #[coffer(tag = 0xBE)]
    ArrayLength,
    #[coffer(tag = 0xBF)]
    AThrow,
    #[coffer(tag = 0xC0)]
    CheckCast(u16),
    #[coffer(tag = 0xC1)]
    InstanceOf(u16),
    #[coffer(tag = 0xC2)]
    MonitorEnter,
    #[coffer(tag = 0xC3)]
    MonitorExit,
    #[coffer(tag = 0xC4)]
    Wide(Wide),
    #[coffer(tag = 0xC5)]
    MultiANewArray(u16, u8),
    #[coffer(tag = 0xC6)]
    IfNull(i16),
    #[coffer(tag = 0xC7)]
    IfNonNull(i16),
    #[coffer(tag = 0xC8)]
    GotoW(i32),
    #[coffer(tag = 0xC9)]
    JsrW(i32),
}
