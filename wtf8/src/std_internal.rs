// Derived from https://github.com/rust-lang/rust/blob/2f847b81a0d8633f200f2c2269c1c43fe9e7def3/library/core/src/str/validations.rs
// and https://github.com/rust-lang/rust/blob/2f847b81a0d8633f200f2c2269c1c43fe9e7def3/library/core/src/char/methods.rs
// and https://github.com/rust-lang/rust/blob/2f847b81a0d8633f200f2c2269c1c43fe9e7def3/library/core/src/char/mod.rs

#![cfg_attr(not(feature = "alloc"), allow(dead_code))]

use core::slice;

const TAG_CONT: u8 = 0b1000_0000;
const TAG_TWO_B: u8 = 0b1100_0000;
const TAG_THREE_B: u8 = 0b1110_0000;
const TAG_FOUR_B: u8 = 0b1111_0000;
const MAX_ONE_B: u32 = 0x80;
const MAX_TWO_B: u32 = 0x800;
const MAX_THREE_B: u32 = 0x10000;

pub fn encode_utf8_raw(code: u32, dst: &mut [u8]) -> &mut [u8] {
    let len = len_utf8(code);
    match (len, &mut dst[..]) {
        (1, [a, ..]) => {
            *a = code as u8;
        }
        (2, [a, b, ..]) => {
            *a = (code >> 6 & 0x1F) as u8 | TAG_TWO_B;
            *b = (code & 0x3F) as u8 | TAG_CONT;
        }
        (3, [a, b, c, ..]) => {
            *a = (code >> 12 & 0x0F) as u8 | TAG_THREE_B;
            *b = (code >> 6 & 0x3F) as u8 | TAG_CONT;
            *c = (code & 0x3F) as u8 | TAG_CONT;
        }
        (4, [a, b, c, d, ..]) => {
            *a = (code >> 18 & 0x07) as u8 | TAG_FOUR_B;
            *b = (code >> 12 & 0x3F) as u8 | TAG_CONT;
            *c = (code >> 6 & 0x3F) as u8 | TAG_CONT;
            *d = (code & 0x3F) as u8 | TAG_CONT;
        }
        _ => panic!(
            "encode_utf8: need {} bytes to encode U+{:X}, but the buffer has {}",
            len,
            code,
            dst.len(),
        ),
    };
    &mut dst[..len]
}

/// Encodes a raw u32 value as UTF-16 into the provided `u16` buffer,
/// and then returns the subslice of the buffer that contains the encoded character.
///
/// Unlike `char::encode_utf16`, this method also handles codepoints in the surrogate range.
/// (Creating a `char` in the surrogate range is UB.)
///
/// # Panics
///
/// Panics if the buffer is not large enough.
/// A buffer of length 2 is large enough to encode any `char`.
#[inline]
pub fn encode_utf16_raw(mut code: u32, dst: &mut [u16]) -> &mut [u16] {
    // SAFETY: each arm checks whether there are enough bits to write into
    unsafe {
        if (code & 0xFFFF) == code && !dst.is_empty() {
            // The BMP falls through
            *dst.get_unchecked_mut(0) = code as u16;
            slice::from_raw_parts_mut(dst.as_mut_ptr(), 1)
        } else if dst.len() >= 2 {
            // Supplementary planes break into surrogates.
            code -= 0x1_0000;
            *dst.get_unchecked_mut(0) = 0xD800 | ((code >> 10) as u16);
            *dst.get_unchecked_mut(1) = 0xDC00 | ((code as u16) & 0x3FF);
            slice::from_raw_parts_mut(dst.as_mut_ptr(), 2)
        } else {
            panic!(
                "encode_utf16: need {} units to encode U+{:X}, but the buffer has {}",
                char::from_u32_unchecked(code).len_utf16(),
                code,
                dst.len(),
            )
        }
    }
}

#[inline]
const fn len_utf8(code: u32) -> usize {
    if code < MAX_ONE_B {
        1
    } else if code < MAX_TWO_B {
        2
    } else if code < MAX_THREE_B {
        3
    } else {
        4
    }
}

/// Returns the initial codepoint accumulator for the first byte.
/// The first byte is special, only want bottom 5 bits for width 2, 4 bits
/// for width 3, and 3 bits for width 4.
#[inline]
const fn utf8_first_byte(byte: u8, width: u32) -> u32 {
    (byte & (0x7F >> width)) as u32
}

/// Returns the value of `ch` updated with continuation byte `byte`.
#[inline]
const fn utf8_acc_cont_byte(ch: u32, byte: u8) -> u32 {
    (ch << 6) | (byte & CONT_MASK) as u32
}

/// Reads the next code point out of a byte iterator (assuming a
/// UTF-8-like encoding).
///
/// # Safety
///
/// `bytes` must produce a valid UTF-8-like (UTF-8 or WTF-8) string
#[inline]
pub unsafe fn next_code_point<'a, I: Iterator<Item = &'a u8>>(bytes: &mut I) -> Option<u32> {
    // Decode UTF-8
    let x = *bytes.next()?;
    if x < 128 {
        return Some(x as u32);
    }

    // Multibyte case follows
    // Decode from a byte combination out of: [[[x y] z] w]
    // NOTE: Performance is sensitive to the exact formulation here
    let init = utf8_first_byte(x, 2);
    // SAFETY: `bytes` produces an UTF-8-like string,
    // so the iterator must produce a value here.
    let y = unsafe { *bytes.next().unwrap_unchecked() };
    let mut ch = utf8_acc_cont_byte(init, y);
    if x >= 0xE0 {
        // [[x y z] w] case
        // 5th bit in 0xE0 .. 0xEF is always clear, so `init` is still valid
        // SAFETY: `bytes` produces an UTF-8-like string,
        // so the iterator must produce a value here.
        let z = unsafe { *bytes.next().unwrap_unchecked() };
        let y_z = utf8_acc_cont_byte((y & CONT_MASK) as u32, z);
        ch = init << 12 | y_z;
        if x >= 0xF0 {
            // [x y z w] case
            // use only the lower 3 bits of `init`
            // SAFETY: `bytes` produces an UTF-8-like string,
            // so the iterator must produce a value here.
            let w = unsafe { *bytes.next().unwrap_unchecked() };
            ch = (init & 7) << 18 | utf8_acc_cont_byte(y_z, w);
        }
    }

    Some(ch)
}

/// Mask of the value bits of a continuation byte.
const CONT_MASK: u8 = 0b0011_1111;
