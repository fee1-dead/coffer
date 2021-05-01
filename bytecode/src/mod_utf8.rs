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

/// An error encountered during conversion.
#[derive(Debug, Error)]
pub enum MUTFError {
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
    UTF8Error(#[from] std::str::Utf8Error),
}

/// Converts a modified utf-8 sequence to an owned rust string.
pub fn modified_utf8_to_string(buf: &[u8]) -> Result<String, MUTFError> {
    let mut count: usize = 0;

    let len = buf.len();
    let mut str: String = String::with_capacity(len);

    'outer: while count < len {
        let c = buf[count];
        match c >> 4u8 {
            0..=7 => {
                count += 1;
                str.push(c as char);
            }
            12 | 13 => {
                let c = c as u32;
                count += 2;
                if count > len {
                    return Err(MUTFError::PartialCharacterAtEnd);
                }
                let c2 = buf[count - 1] as u32;
                if (c2 & 0xC0) != 0x80 {
                    return Err(MUTFError::AroundByte(count));
                }
                str.push(
                    char::try_from(((c & 0x1F) << 6) | (c2 & 0x3F))
                        .ok()
                        .ok_or(MUTFError::AroundByte(count))?,
                )
            }
            14 => {
                // Reason: Rust characters differ from java, in java a character is 2 bytes, and in rust it is 4.
                // This means that rust is able to represent an emoji as one character (not two), whereas java will need two `char`s
                if c == 0b1110_1101 {
                    count += 6;
                    if count <= len {
                        let c2 = buf[count - 5];
                        let c3 = buf[count - 4];
                        let c4 = buf[count - 3];
                        let c5 = buf[count - 2];
                        let c6 = buf[count - 1];
                        if c4 == 0b1110_1101 && c2 >> 4u8 == 0b1010 && c5 >> 4u8 == 0b1011 {
                            let c2 = c2 as u32;
                            let c3 = c3 as u32;
                            let c5 = c5 as u32;
                            let c6 = c6 as u32;
                            str.push(
                                char::try_from(
                                    (((c2 & 0b1111) + 1) << 16)
                                        | ((c3 & 0b111111) << 10)
                                        | ((c5 & 0b1111) << 6)
                                        | (c6 & 0b111111),
                                )
                                .ok()
                                .ok_or(MUTFError::AroundByte(count))?,
                            );
                            continue 'outer;
                        }
                    }
                    count -= 6;
                }
                let c = c as u32;
                count += 3;
                if count > len {
                    return Err(MUTFError::PartialCharacterAtEnd);
                }
                let c2 = buf[count - 2];
                let c3 = buf[count - 1];

                if (c2 & 0xC0) != 0x80 || (c3 & 0xC0) != 0x80 {
                    return Err(MUTFError::AroundByte(count));
                }
                let c2 = c2 as u32;
                let c3 = c3 as u32;

                str.push(
                    char::try_from(((c & 0xF) << 12) | ((c2 & 0x3F) << 6) | (c3 & 0x3F))
                        .ok()
                        .ok_or(MUTFError::AroundByte(count))?,
                )
            }
            _ => return Err(MUTFError::AroundByte(count)),
        }
    }
    str.shrink_to_fit();
    Ok(str)
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
