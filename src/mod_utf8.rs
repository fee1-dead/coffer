//! Module for converting between Java's Modified UTF-8 and UTF-8.
//!
//! Rust uses UTF-8 natively, so a conversion is needed for every string in Java.
//!
//! An UTF-8 codepoint might be 1, 2, or 4 bytes, but for MUTF-8 it might be 1, 3, or 6.
//! Another important thing is that there will never be a NUL byte in a string.
//!
//! Refer to the [JVM Spec](https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.4.7) for more info.

// FIXME please fix this stinky code

use thiserror::Error;
use wtf_8::{Wtf8Error, Wtf8Str, Wtf8String};

/// An error encountered during conversion.
#[derive(Debug, Error)]
pub enum MutfError {
    /// The code point is 3 or 6 bytes long if the MUTF is valid,
    /// however it reached end of string, resulting in a partial
    /// code point at the end.
    #[error("Malformed Input: Partial character at end")]
    PartialCharacterAtEnd,

    /// The code point at index is invalid.
    #[error("Malformed Input around byte: {0}")]
    AroundByte(usize),

    /// The modified utf-8 buffer becomes an invalid UTF-8 sequence when decoded.
    #[error(transparent)]
    WTF8Error(#[from] Wtf8Error),
}

/// Converts a modified utf-8 sequence to an owned wtf-8 string.
pub fn modified_utf8_to_string(buf: &[u8]) -> Result<Wtf8String, MutfError> {
    // based on https://datatracker.ietf.org/doc/html/rfc3629#page-4 with wtf-8 modifications
    let mut iter = buf.iter().copied().enumerate();
    let mut is_high_surrogate = false;
    let mut modification = false;
    while let Some((index, b)) = iter.next() {
        macro_rules! err {
            () => {
                return Err(MutfError::AroundByte(index))
            };
            ($e: expr) => {
                return Err(MutfError::WTF8Error(Wtf8Error {
                    valid_up_to: index,
                    error_len: NonZeroU8::new($e),
                }))
            };
        }
        macro_rules! next {
            () => {
                if let Some((_, b)) = iter.next() {
                    b
                } else {
                    err!();
                }
            };
        }
        match b {
            0xC0 if next!() == 0x80 => {
                modification = true;
                break;
            }
            0xED => {
                let n = next!();
                match n {
                    0xA0..=0xAF => {
                        is_high_surrogate = true;
                        next!();
                    }
                    0xB0..=0xBF => {
                        // Encoded surrogate pairs need to be modified to WTF-8.
                        if is_high_surrogate {
                            modification = true;
                            break;
                        }
                        is_high_surrogate = false;
                    }
                    _ => is_high_surrogate = false,
                }
            }
            _ => is_high_surrogate = false,
        }
        
    }
    if modification {
        let mut out = Vec::with_capacity(buf.len());
        let mut iter = buf.iter().copied().enumerate();
        while let Some((index, b)) = iter.next() {
            match b {
                0xC0 if buf.get(index + 1) == Some(&0x80) => {
                    out.push(0x0);
                    iter.next();
                }
                0xED => {
                    if let (
                        Some(n @ 0xA0..=0xAF),
                        Some(n1),
                        Some(0xED),
                        Some(n3 @ 0xB0..=0xBF),
                        Some(n4),
                    ) = (
                        buf.get(index + 1),
                        buf.get(index + 2),
                        buf.get(index + 3),
                        buf.get(index + 4),
                        buf.get(index + 5),
                    ) {
                        let char = 0x10000
                            + (((*n & 0xf) as u32) << 16)
                            + (((*n1 & 0x3f) as u32) << 10)
                            + (((*n3 & 0xf) as u32) << 6)
                            + (*n4 & 0x3f) as u32;
                        let ch = char::from_u32(char).unwrap();
                        let mut bytes = [0; 4];
                        ch.encode_utf8(&mut bytes);
                        out.extend_from_slice(&bytes);
                        iter.nth(5 - 1);
                    } else {
                        out.push(b);
                    }
                }
                _ => out.push(b),
            }
        }
        Ok(Wtf8String::from_bytes(out)?)
    } else {
        Ok(Wtf8String::from_bytes(buf.to_owned())?)
    }
}

/// Converts a string to modified UTF-8.
///
/// This will never error because `&str` is guarenteed to be in UTF-8,
/// therefore it will always be able to convert to modified UTF-8.
pub fn string_to_modified_utf8(str: &Wtf8Str) -> Vec<u8> {
    let mut utflen: usize = 0;
    for c in str.codepoints() {
        utflen += match c.to_u32() {
            0x1..=0x7F => 1,
            0x0 | 0x80..=0x7FF => 2,
            0x800..=0x7FFF => 3,
            _ => 6,
        }
    }
    let mut vec = Vec::with_capacity(utflen);
    for c in str.codepoints() {
        let c = c.to_u32();

        #[allow(clippy::unusual_byte_groupings)]
        match c {
            0b1..=0b1111111 => {
                vec.push(c as u8);
            }
            0b0 | 0b100_00000..=0b11111_111111 => {
                // 110xxxxx 10xxxxxx
                vec.push(((c >> 6) as u8 & 0b011111) | 0b110_00000);
                vec.push((c as u8 & 0b111111) | 0b10_000000);
            }
            0b1000_00000000..=0b1111_111111_111111 => {
                // 1110xxxx 10xxxxxx 10xxxxxx
                vec.push(((c >> 12) as u8 & 0b001111) | 0b1110_0000);
                vec.push(((c >> 6) as u8 & 0b111111) | 0b10_000000);
                vec.push((c as u8 & 0b111111) | 0b10_000000);
            }
            _ => {
                // 11101101 1010(xxxxx - 1) 10xxxxxx 11101101 1011xxxx 10xxxxxx
                vec.push(0b11101101);
                vec.push((((c >> 16) as u8 & 0b11111) - 1) | 0b1010_0000);
                vec.push(0b1000_0000 | ((c >> 10) as u8 & 0b111111));
                vec.push(0b11101101);
                vec.push(((c >> 6) as u8 & 0b1111) | 0b1011_0000);
                vec.push((c as u8 & 0b111111) | 0b1000_0000);
            }
        }
    }
    vec
}
