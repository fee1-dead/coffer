use crate::decoder::Decoder;
use std::io::{Read, Seek, SeekFrom};
use std::convert::TryFrom;
use crate::error::Error;

#[derive(Debug)]
pub struct JClassIdx {
    /// Contains the offset of the end of each entry. For example: `constant_pool[0]` will contain the end of the first entry, also the start of the second entry.
    /// Since double takes up two entries, if a double was the first entry, then `constant_pool[1]` would be equal to `0`.
    pub constant_pool: Vec<u64>,
    pub itfs: u16,
    /// Contains the offset information of the attributes that the fields holds. the last `u64` of a field is the end of the field.
    pub fields: Vec<Vec<u64>>,
    /// Contains the offset information of the attributes that the methods holds. the last `u64` of a method is the end of the method.
    pub methods: Vec<Vec<u64>>,
    pub attrs: Vec<u64>
}
fn attrs<T: Read + Seek>(value: &mut Decoder<'_, T>) -> Result<Vec<u64>, Error> {
    let attribute_count = value.u16()?;
    let mut vec_inner: Vec<u64> = Vec::with_capacity(attribute_count as usize);
    for _ in 0..attribute_count {
        value.seek(SeekFrom::Current(2))?;
        let length = value.u32()?;
        value.seek(SeekFrom::Current(length as i64))?;
        vec_inner.push(value.idx);
    }
    Ok(vec_inner)
}
fn fields_or_methods<T: Read + Seek>(value: &mut Decoder<'_, T>) -> Result<Vec<Vec<u64>>, Error> {
    let count = value.u16()?;
    let mut vec_outer: Vec<Vec<u64>> = Vec::with_capacity(count as usize);
    for _ in 0..count {
        value.seek(SeekFrom::Current(6))?;
        let vec_inner = attrs(value)?;
        vec_outer.push(vec_inner)
    }
    Ok(vec_outer)
}
impl<T: Read + Seek> TryFrom<&mut Decoder<'_, T>> for JClassIdx {
    type Error = crate::error::Error;

    fn try_from(value: &mut Decoder<'_, T>) -> Result<Self, Self::Error> {
        value.seek(SeekFrom::Current(8))?;
        let constant_pool_size = value.u16()?;
        let mut constant_pool: Vec<u64> = Vec::with_capacity(constant_pool_size as usize);
        for mut i in 0..constant_pool_size - 1 {
            let tag = value.u8()?;
            let is_wide = false;
            let jump = match tag {
                1 => value.u16()? as i64,
                7 | 8 => 2,
                9 | 10 | 11 | 12 => 4,
                _ => return Err(Error::Unrecognized("constant entry tag", format!("{} at index {}", tag, i)))
            };
            value.seek(SeekFrom::Current(jump));
            constant_pool.push(value.idx);
            if is_wide {
                constant_pool.push(0);
                i = i + 1;
            }
        }
        value.seek(SeekFrom::Current(6))?;
        let itfs = value.u16()?;
        value.seek(SeekFrom::Current(itfs as i64 * 2));

        let fields = fields_or_methods(value)?;
        let methods = fields_or_methods(value)?;
        let attrs = attrs(value)?;
        let len = value.stream_len()?;
        let idx = value.idx;
        if idx != len {
            return Err(Error::ExtraBytes(len - idx))
        }
        Ok(JClassIdx {
            constant_pool,
            itfs,
            fields,
            methods,
            attrs
        })
    }
}

