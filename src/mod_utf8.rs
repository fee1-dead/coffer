use thiserror::Error;

#[derive(Debug, Error)]
pub enum MUTFError {
    #[error("Malformed Input: Partial character as end")]
    PartialCharacterAtEnd,
    #[error("Malformed Input around byte {0}")]
    AroundByte(usize),
    #[error(transparent)]
    UTF8Error(#[from] std::str::Utf8Error),
}
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
                    char::from_u32(((c & 0x1F) << 6) | (c2 & 0x3F))
                        .ok_or(MUTFError::AroundByte(count))?
                )
            }
            14 => {
                'surrougate: loop {
                    if c == 0b1110_1101 {
                        count += 6;
                        if count > len {
                            count -= 6;
                            break 'surrougate;
                        }
                        let c2 = buf[count - 5];
                        let c3 = buf[count - 4];
                        let c4 = buf[count - 3];
                        let c5 = buf[count - 2];
                        let c6 = buf[count - 1];
                        if c4 != 0b1110_1101 {
                            break 'surrougate;
                        }
                        if c2 >> 4u8 != 0b1010 {
                            break 'surrougate;
                        }
                        if c5 >> 4u8 != 0b1011 {
                            break 'surrougate;
                        }
                        let c2 = c2 as u32;
                        let c3 = c3 as u32;
                        let c5 = c5 as u32;
                        let c6 = c6 as u32;
                        str.push(char::from_u32(
                            (((c2 & 0b1111) + 1) << 16)
                                | ((c3 & 0b111111) << 10)
                                | ((c5 & 0b1111) << 6)
                                | (c6 & 0b111111),
                        ).ok_or(MUTFError::AroundByte(count))?);
                        continue 'outer;
                    }
                    break;
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
                let c2 = c2 as u32; let c3 = c3 as u32;

                str.push(
                    char::from_u32(((c & 0xF) << 12) | ((c2 & 0x3F) << 6) | (c3 & 0x3F))
                        .ok_or(MUTFError::AroundByte(count))?
                )
            }
            _ => return Err(MUTFError::AroundByte(count)),
        }
    }
    Ok(str)
}

pub fn string_to_modified_utf8(str: &str) -> Result<Vec<u8>, MUTFError> {
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
        match c {
            0x1..=0b1111111 => {
                vec.push(c as u8);
            }
            0x0 | 0b100_00000..=0b11111_111111 => {
                // 110xxxxx 10xxxxxx
                vec.push(((c >> 6) as u8 & 0b011111) | 0b110_00000);
                vec.push(((c >> 0) as u8 & 0b111111) | 0b10_000000);
            }
            0b1000_00000000..=0b1111_111111_111111 => {
                // 1110xxxx 10xxxxxx 10xxxxxx
                vec.push(((c >> 12) as u8 & 0b001111) | 0b1110_0000);
                vec.push(((c >> 6) as u8 & 0b111111) | 0b10_000000);
                vec.push(((c >> 0) as u8 & 0b111111) | 0b10_000000);
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
    Ok(vec)
}
