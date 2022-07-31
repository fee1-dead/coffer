//! Module for converting between Java's Modified UTF-8 and UTF-8.
//!
//! Rust uses UTF-8 natively, so a conversion is needed for every string in Java.
//!
//! An UTF-8 codepoint might be 1, 2, or 4 bytes, but for MUTF-8 it might be 1, 3, or 6.
//! Another important thing is that there will never be a NUL byte in a string.
//!
//! Refer to the [JVM Spec](https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.4.7) for more info.

use std::convert::TryFrom;

use thiserror::Error;
use wtf_8::{Wtf8String, Wtf8Error};

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
    let mut index: usize = 0;

    let len = buf.len();
    let mut out = Vec::with_capacity(len);

    let mut bytes = buf.iter().copied();
    macro_rules! error {
        () => {
            MutfError::AroundByte(index)
        };
    }
    loop {
        macro_rules! next {
            () => {{
                bytes
                    .next()
                    .ok_or(MutfError::PartialCharacterAtEnd)?
            }};
        }
        let c = match bytes.next() {
            Some(c) => c,
            None => break,
        };
        match c {
            0b0000_0000..=0b0111_1111 => out.push(c),
            // c1: 110x xxxx
            // c2: 10xx xxxx
            0xC0 if next!() == 0x80 => {
                out.push(0);
            }
            // c1: 1110 xxxx
            // c2: 10xx xxxx
            // c3: 10xx xxxx
            0b1110_0000..=0b1110_1111 => {
                let c2 = next!();
                let c3 = next!();

                // c1: 1110 1101
                // c2: 1010 xxxx (plus one when decoding)
                // c3: 10xx xxxx
                // c4: 1110 1101
                // c5: 1011 xxxx
                // c6: 10xx xxxx
                if c == 0b1110_1101 && (c2 & 0b1111_0000) == 0b1010_0000 {
                    if bytes.len() >= 3 {
                        let c4 = next!();
                        let c5 = next!();
                        let c6 = next!();
                        let valid = c4 == 0b1110_1101
                            && (c5 & 0b1111_0000) == 0b1011_0000
                            && (c6 & 0b1100_0000) == 0b1000_0000;
                        if valid {
                            let c2 = c2 as u32;
                            let c3 = c3 as u32;
                            let c5 = c5 as u32;
                            let c6 = c6 as u32;
        
                            let mut buf = [0; 4];
                            let c = char::try_from(
                                0x10000
                                    | ((c2 & 0b1111) << 16)
                                    | ((c3 & 0b111111) << 10)
                                    | ((c5 & 0b1111) << 6)
                                    | (c6 & 0b111111),
                            )
                            .map_err(|_| error!())?;
                            c.encode_utf8(&mut buf);
                            out.extend_from_slice(&buf);
                        } else {
                            out.extend_from_slice(&[c, c2, c3, c4, c5, c6]);
                            continue;
                        }
                    }

                    continue;
                }

                out.extend_from_slice(&[c, c2, c3]);
            }
            _ => return Err(error!()),
        }
    }

    Wtf8String::from_bytes(out).map_err(Into::into)
}

/// Converts a string to modified UTF-8.
///
/// This will never error because `&str` is guarenteed to be in UTF-8,
/// therefore it will always be able to convert to modified UTF-8.
pub fn string_to_modified_utf8(str: &str) -> Vec<u8> {
    let mut utflen: usize = 0;
    for c in str.chars() {
        utflen += match c as u32 {
            0x1..=0x7F => 1,
            0x0 | 0x80..=0x7FF => 2,
            0x800..=0x7FFF => 3,
            _ => 6,
        }
    }
    let mut vec = Vec::with_capacity(utflen);
    for c in str.chars() {
        let c = c as u32;

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
