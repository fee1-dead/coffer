//! Helpers for use with procedural macro.
use std::any::type_name;
use std::borrow::Cow;
use std::fmt::Display;
use std::io::{Read, Write};
use std::marker::PhantomData;

use wtf_8::Wtf8Str;

use crate::rw::ReadWrite;
use crate::{ConstantPoolReadWrite, ConstantPoolReader, ConstantPoolWriter, Error};

pub trait ConstantPoolReadWriteAs<T> {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> crate::Result<T>;
    fn write_to<C: ConstantPoolWriter, W: Write>(
        x: &T,
        cp: &mut C,
        writer: &mut W,
    ) -> crate::Result<()>;
}

pub trait ReadWriteAs<T> {
    fn read_from<R: Read>(reader: &mut R) -> crate::Result<T>;
    fn write_to<W: Write>(
        x: &T,
        writer: &mut W,
    ) -> crate::Result<()>;
}

pub struct Normal;

impl<T: ReadWrite> ConstantPoolReadWriteAs<T> for Normal {
    fn read_from<C: ConstantPoolReader, R: Read>(_: &mut C, reader: &mut R) -> crate::Result<T> {
        T::read_from(reader)
    }
    fn write_to<C: ConstantPoolWriter, W: Write>(
        x: &T,
        _: &mut C,
        writer: &mut W,
    ) -> crate::Result<()> {
        T::write_to(x, writer)
    }
}

pub struct StringCp<const TAG: u8>;

impl<const TAG: u8> ConstantPoolReadWriteAs<Cow<'static, Wtf8Str>> for StringCp<TAG> {
    fn read_from<C: ConstantPoolReader, R: Read>(
        cp: &mut C,
        reader: &mut R,
    ) -> crate::Result<Cow<'static, Wtf8Str>> {
        let index = u16::read_from(reader)?;
        cp.read_indirect_str(TAG, index)
            .ok_or_else(|| Error::Invalid("constant pool entry", format!("{index}").into()))
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(
        x: &Cow<'static, Wtf8Str>,
        cp: &mut C,
        writer: &mut W,
    ) -> crate::Result<()> {
        cp.insert_indirect_str(TAG, x.clone()).write_to(writer)
    }
}

impl<const TAG: u8> ConstantPoolReadWriteAs<Option<Cow<'static, Wtf8Str>>> for StringCp<TAG> {
    fn read_from<C: ConstantPoolReader, R: Read>(
        cp: &mut C,
        reader: &mut R,
    ) -> crate::Result<Option<Cow<'static, Wtf8Str>>> {
        let index = u16::read_from(reader)?;
        if index == 0 {
            Ok(None)
        } else {
            cp.read_indirect_str(TAG, index)
                .ok_or_else(|| Error::Invalid("constant pool entry", format!("{index}").into()))
                .map(Some)
        }
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(
        x: &Option<Cow<'static, Wtf8Str>>,
        cp: &mut C,
        writer: &mut W,
    ) -> crate::Result<()> {
        if let Some(x) = x {
            cp.insert_indirect_str(TAG, x.clone()).write_to(writer)
        } else {
            0u16.write_to(writer)
        }
    }
}

pub type Class = StringCp<7>;
pub type String = StringCp<8>;
pub type Module = StringCp<19>;
pub type Package = StringCp<20>;

pub struct Identity;

impl<T: ConstantPoolReadWrite> ConstantPoolReadWriteAs<T> for Identity {
    fn read_from<C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) -> crate::Result<T> {
        T::read_from(cp, reader)
    }
    fn write_to<C: ConstantPoolWriter, W: Write>(
        x: &T,
        cp: &mut C,
        writer: &mut W,
    ) -> crate::Result<()> {
        x.write_to(cp, writer)
    }
}

impl<T: ReadWrite> ReadWriteAs<T> for Identity {
    fn read_from<R: Read>(reader: &mut R) -> crate::Result<T> {
        T::read_from(reader)
    }
    fn write_to<W: Write>(
        x: &T,
        writer: &mut W,
    ) -> crate::Result<()> {
        x.write_to(writer)
    }
}

pub struct VecLen<Helper, T>(PhantomData<(Helper, T)>);

impl<Helper: ReadWriteAs<T>, T, L> ReadWriteAs<Vec<T>> for VecLen<Helper, L>
where
    L: TryInto<usize> + TryFrom<usize> + ReadWrite + Display + Copy,
{
    fn read_from<R: Read>(reader: &mut R) -> crate::Result<Vec<T>> {
        let len = L::read_from(reader)?;
        let n = len.try_into().map_err(|_| {
            Error::Invalid(
                "vec length",
                format!("{len} ({}) does not fit in a `usize`", type_name::<L>()).into(),
            )
        })?;
        (0..n).map(|_| Helper::read_from(reader)).collect()
    }
    fn write_to<W: Write>(
        x: &Vec<T>,
        writer: &mut W,
    ) -> crate::Result<()> {
        let len = x.len();
        let l = L::try_from(len).map_err(|_| {
            Error::Invalid(
                "vec length",
                format!("{len} does not fit in a `{}`", type_name::<L>()).into(),
            )
        })?;
        L::write_to(&l, writer)?;
        for x in x {
            Helper::write_to(x, writer)?;
        }
        Ok(())
    }
}

impl<Helper: ConstantPoolReadWriteAs<T>, T, L> ConstantPoolReadWriteAs<Vec<T>> for VecLen<Helper, L>
where
    L: Into<usize> + TryFrom<usize> + ReadWrite,
{
    fn read_from<C: ConstantPoolReader, R: Read>(
        cp: &mut C,
        reader: &mut R,
    ) -> crate::Result<Vec<T>> {
        let len = L::read_from(reader)?;
        let n = len.into();
        (0..n).map(|_| Helper::read_from(cp, reader)).collect()
    }
    fn write_to<C: ConstantPoolWriter, W: Write>(
        x: &Vec<T>,
        cp: &mut C,
        writer: &mut W,
    ) -> crate::Result<()> {
        let len = x.len();
        let l = L::try_from(len).map_err(|_| {
            Error::Invalid(
                "vec length",
                format!("{len} does not fit in a `{}`", type_name::<L>()).into(),
            )
        })?;
        L::write_to(&l, writer)?;
        for x in x {
            Helper::write_to(x, cp, writer)?;
        }
        Ok(())
    }
}

pub type Vec8<H = Identity> = VecLen<H, u8>;
pub type Vec16<H = Identity> = VecLen<H, u16>;
pub type Vec32<H = Identity> = VecLen<H, u32>;
