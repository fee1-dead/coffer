use std::io::{Read, Seek, SeekFrom};
use crate::error::{Error, Result};


/// Decoder for Big Endian Values. Does not support little endian.
///
/// ```
/// # use coffer::decoder::Decoder;
/// # use std::io::Cursor;
/// let mut buf : [u8; 30] = [
/// 0xBE, 0xEF,
/// 0xCA, 0xFE, 0xBA, 0xBE,
/// 0x70, 0x80, 0x20, 0x10, 0x20, 0x90, 0x24, 0x12,
/// 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F
/// ];
/// let mut cursor = Cursor::new(buf);
/// let mut decoder = Decoder::new(&mut cursor);
/// assert_eq!((&mut decoder).u16().unwrap(), 0xBEEFu16);
/// assert_eq!((&mut decoder).u32().unwrap(), 0xCAFEBABEu32);
/// assert_eq!((&mut decoder).u64().unwrap(), 0x7080201020902412u64);
/// assert_eq!((&mut decoder).u128().unwrap(), 0x101112131415161718191A1B1C1D1E1Fu128)
/// ```
pub struct Decoder<'a, T: Read + Seek> {
    inner: &'a mut T,
    pub(crate) idx: u64
}

macro_rules! read_fn {
    ($type:ty, $fnName: ident, $bytesize:literal) => {
        pub fn $fnName(&mut self) -> Result<$type> {
            let mut buf = [0u8; $bytesize];
            let bytes = self.inner.read(&mut buf)?;
            if bytes != $bytesize {
                Err(Error::EOF)
            } else {
                Ok(<$type>::from_be_bytes(buf))
            }
        }
    }
}

impl<T: Read + Seek> Decoder<'_, T> {
    pub fn new(read: &mut T) -> Decoder<T> {
        Decoder {
            inner: read,
            idx: 0
        }
    }

    pub fn jump(&mut self, off: i64) -> Result<()> {
        self.seek(SeekFrom::Current(off))?;
        Ok(())
    }
    pub fn u8(&mut self) -> Result<u8> {
        Ok(self.inner.bytes().next().ok_or(Error::EOF)??)
    }
    read_fn!(u128, u128, 16);
    read_fn!(u64, u64, 8);
    read_fn!(u32, u32, 4);
    read_fn!(u16, u16, 2);
}


impl<T: Read + Seek> Seek for Decoder<'_, T> {
    fn seek(&mut self, pos: SeekFrom) -> std::io::Result<u64> {
        let result = self.inner.seek(pos)?;
        self.idx = result;
        Ok(result)
    }
}

impl<T: Read + Seek> AsMut<T> for Decoder<'_, T> {
    fn as_mut(&mut self) -> &mut T {
        self.inner
    }
}

impl<'a, T: Read + Seek> From<&'a mut T> for Decoder<'a, T> {
    fn from(read: &'a mut T) -> Self {
        Decoder::new(read)
    }
}