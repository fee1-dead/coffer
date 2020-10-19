use crate::decoder::Decoder;
use std::io::{Read, Seek};
use std::convert::TryFrom;
use std::any::Any;

/// Contains info of the offsets of the end of each attributes. The offsets are relative to its owner, it is only absolute if it is in an `IClass`.
type AttributesIdx = Vec<usize>;

pub struct JClassIdx {
    /// Contains the offset of the end of each entry. For example: `constant_pool[0]` will contain the end of the first entry, also the start of the second entry.
    /// Since double takes up two entries, if a double was the first entry, then `constant_pool[1]` would be equal to `0`.
    pub constant_pool: Vec<usize>,
    /// Contains the offset of the end of each fields as well as the offset information of the attributes that it holds.
    pub fields: Vec<(usize, AttributesIdx)>,
    /// Contains the offset of the end of each methods as well as the offset information of the attributes that it holds.
    pub methods: Vec<(usize, AttributesIdx)>,
    pub attrs: AttributesIdx
}

impl<T: Read + Seek> TryFrom<&mut Decoder<'_, T>> for JClassIdx {
    type Error = crate::error::Error;

    fn try_from(mut value: &mut Decoder<'_, T>) -> Result<Self, Self::Error> {
        value.jump(10)?;
        let constant_pool_size = value.u16()?;
        for mut i in 0..constant_pool_size - 1 {
            i = i + 1;
        }
        unimplemented!()
    }
}

