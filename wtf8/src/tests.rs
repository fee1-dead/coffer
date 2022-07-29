// https://github.com/rust-lang/rust/blob/2f847b81a0d8633f200f2c2269c1c43fe9e7def3/library/std/src/sys_common/wtf8/tests.rs

use std::borrow::Cow;

use super::*;

#[test]
fn code_point_from_u32() {
    assert!(Codepoint::from_u32(0).is_some());
    assert!(Codepoint::from_u32(0xD800).is_some());
    assert!(Codepoint::from_u32(0x10FFFF).is_some());
    assert!(Codepoint::from_u32(0x110000).is_none());
}

#[test]
fn code_point_to_u32() {
    fn c(value: u32) -> Codepoint {
        Codepoint::from_u32(value).unwrap()
    }
    assert_eq!(c(0).to_u32(), 0);
    assert_eq!(c(0xD800).to_u32(), 0xD800);
    assert_eq!(c(0x10FFFF).to_u32(), 0x10FFFF);
}

#[test]
fn code_point_from_char() {
    assert_eq!(Codepoint::from_char('a').to_u32(), 0x61);
    assert_eq!(Codepoint::from_char('💩').to_u32(), 0x1F4A9);
}

#[test]
fn code_point_to_string() {
    assert_eq!(format!("{:?}", Codepoint::from_char('a')), "U+0061");
    assert_eq!(format!("{:?}", Codepoint::from_char('💩')), "U+1F4A9");
}

#[test]
fn code_point_to_char() {
    fn c(value: u32) -> Codepoint {
        Codepoint::from_u32(value).unwrap()
    }
    assert_eq!(c(0x61).to_char(), Some('a'));
    assert_eq!(c(0x1F4A9).to_char(), Some('💩'));
    assert_eq!(c(0xD800).to_char(), None);
}

#[test]
fn code_point_to_char_lossy() {
    fn c(value: u32) -> Codepoint {
        Codepoint::from_u32(value).unwrap()
    }
    assert_eq!(c(0x61).to_char_lossy(), 'a');
    assert_eq!(c(0x1F4A9).to_char_lossy(), '💩');
    assert_eq!(c(0xD800).to_char_lossy(), '\u{FFFD}');
}

#[test]
fn wtf8buf_new() {
    assert_eq!(Wtf8String::new().bytes, b"");
}

#[test]
fn wtf8buf_from_borrowed() {
    assert_eq!(Wtf8String::from_borrowed("").bytes, b"");
    assert_eq!(
        Wtf8String::from_borrowed("aé 💩").bytes,
        b"a\xC3\xA9 \xF0\x9F\x92\xA9"
    );
}

#[test]
fn wtf8buf_from_string() {
    assert_eq!(Wtf8String::from_string(String::from("")).bytes, b"");
    assert_eq!(
        Wtf8String::from_string(String::from("aé 💩")).bytes,
        b"a\xC3\xA9 \xF0\x9F\x92\xA9"
    );
}

#[test]
fn wtf8buf_from_wide() {
    assert_eq!(Wtf8String::from_wide(&[]).bytes, b"");
    assert_eq!(
        Wtf8String::from_wide(&[0x61, 0xE9, 0x20, 0xD83D, 0xD83D, 0xDCA9]).bytes,
        b"a\xC3\xA9 \xED\xA0\xBD\xF0\x9F\x92\xA9"
    );
}

#[test]
fn wtf8buf_push_str() {
    let mut string = Wtf8String::new();
    assert_eq!(string.bytes, b"");
    string.push_str("aé 💩");
    assert_eq!(string.bytes, b"a\xC3\xA9 \xF0\x9F\x92\xA9");
}

#[test]
fn wtf8buf_push_char() {
    let mut string = Wtf8String::from_borrowed("aé ");
    assert_eq!(string.bytes, b"a\xC3\xA9 ");
    string.push_char('💩');
    assert_eq!(string.bytes, b"a\xC3\xA9 \xF0\x9F\x92\xA9");
}

#[test]
fn wtf8buf_push() {
    let mut string = Wtf8String::from_borrowed("aé ");
    assert_eq!(string.bytes, b"a\xC3\xA9 ");
    string.push(Codepoint::from_char('💩'));
    assert_eq!(string.bytes, b"a\xC3\xA9 \xF0\x9F\x92\xA9");

    fn c(value: u32) -> Codepoint {
        Codepoint::from_u32(value).unwrap()
    }

    let mut string = Wtf8String::new();
    string.push(c(0xD83D)); // lead
    string.push(c(0xDCA9)); // trail
    assert_eq!(string.bytes, b"\xF0\x9F\x92\xA9"); // Magic!

    let mut string = Wtf8String::new();
    string.push(c(0xD83D)); // lead
    string.push(c(0x20)); // not surrogate
    string.push(c(0xDCA9)); // trail
    assert_eq!(string.bytes, b"\xED\xA0\xBD \xED\xB2\xA9");

    let mut string = Wtf8String::new();
    string.push(c(0xD800)); // lead
    string.push(c(0xDBFF)); // lead
    assert_eq!(string.bytes, b"\xED\xA0\x80\xED\xAF\xBF");

    let mut string = Wtf8String::new();
    string.push(c(0xD800)); // lead
    string.push(c(0xE000)); // not surrogate
    assert_eq!(string.bytes, b"\xED\xA0\x80\xEE\x80\x80");

    let mut string = Wtf8String::new();
    string.push(c(0xD7FF)); // not surrogate
    string.push(c(0xDC00)); // trail
    assert_eq!(string.bytes, b"\xED\x9F\xBF\xED\xB0\x80");

    let mut string = Wtf8String::new();
    string.push(c(0x61)); // not surrogate, < 3 bytes
    string.push(c(0xDC00)); // trail
    assert_eq!(string.bytes, b"\x61\xED\xB0\x80");

    let mut string = Wtf8String::new();
    string.push(c(0xDC00)); // trail
    assert_eq!(string.bytes, b"\xED\xB0\x80");
}

#[test]
fn wtf8buf_push_wtf8() {
    let mut string = Wtf8String::from_borrowed("aé");
    assert_eq!(string.bytes, b"a\xC3\xA9");
    string.push_wtf8(Wtf8Str::new(" 💩"));
    assert_eq!(string.bytes, b"a\xC3\xA9 \xF0\x9F\x92\xA9");

    fn w(v: &[u8]) -> &Wtf8Str {
        unsafe { Wtf8Str::from_bytes_unchecked(v) }
    }

    let mut string = Wtf8String::new();
    string.push_wtf8(w(b"\xED\xA0\xBD")); // lead
    string.push_wtf8(w(b"\xED\xB2\xA9")); // trail
    assert_eq!(string.bytes, b"\xF0\x9F\x92\xA9"); // Magic!

    let mut string = Wtf8String::new();
    string.push_wtf8(w(b"\xED\xA0\xBD")); // lead
    string.push_wtf8(w(b" ")); // not surrogate
    string.push_wtf8(w(b"\xED\xB2\xA9")); // trail
    assert_eq!(string.bytes, b"\xED\xA0\xBD \xED\xB2\xA9");

    let mut string = Wtf8String::new();
    string.push_wtf8(w(b"\xED\xA0\x80")); // lead
    string.push_wtf8(w(b"\xED\xAF\xBF")); // lead
    assert_eq!(string.bytes, b"\xED\xA0\x80\xED\xAF\xBF");

    let mut string = Wtf8String::new();
    string.push_wtf8(w(b"\xED\xA0\x80")); // lead
    string.push_wtf8(w(b"\xEE\x80\x80")); // not surrogate
    assert_eq!(string.bytes, b"\xED\xA0\x80\xEE\x80\x80");

    let mut string = Wtf8String::new();
    string.push_wtf8(w(b"\xED\x9F\xBF")); // not surrogate
    string.push_wtf8(w(b"\xED\xB0\x80")); // trail
    assert_eq!(string.bytes, b"\xED\x9F\xBF\xED\xB0\x80");

    let mut string = Wtf8String::new();
    string.push_wtf8(w(b"a")); // not surrogate, < 3 bytes
    string.push_wtf8(w(b"\xED\xB0\x80")); // trail
    assert_eq!(string.bytes, b"\x61\xED\xB0\x80");

    let mut string = Wtf8String::new();
    string.push_wtf8(w(b"\xED\xB0\x80")); // trail
    assert_eq!(string.bytes, b"\xED\xB0\x80");
}

#[test]
fn wtf8buf_truncate() {
    let mut string = Wtf8String::from_borrowed("aé");
    string.truncate(1);
    assert_eq!(string.bytes, b"a");
}

#[test]
#[should_panic]
fn wtf8buf_truncate_fail_code_point_boundary() {
    let mut string = Wtf8String::from_borrowed("aé");
    string.truncate(2);
}

#[test]
#[should_panic]
fn wtf8buf_truncate_fail_longer() {
    let mut string = Wtf8String::from_borrowed("aé");
    string.truncate(4);
}

#[test]
fn wtf8buf_into_string() {
    let mut string = Wtf8String::from_borrowed("aé 💩");
    assert_eq!(string.clone().into_string(), Ok(String::from("aé 💩")));
    string.push(Codepoint::from_u32(0xD800).unwrap());
    assert_eq!(string.clone().into_string(), Err(string));
}

#[test]
fn wtf8buf_into_string_lossy() {
    let mut string = Wtf8String::from_borrowed("aé 💩");
    assert_eq!(string.clone().into_string_lossy(), String::from("aé 💩"));
    string.push(Codepoint::from_u32(0xD800).unwrap());
    assert_eq!(string.clone().into_string_lossy(), String::from("aé 💩�"));
}

#[test]
fn wtf8buf_from_iterator() {
    fn f(values: &[u32]) -> Wtf8String {
        values
            .iter()
            .map(|&c| Codepoint::from_u32(c).unwrap())
            .collect::<Wtf8String>()
    }
    assert_eq!(
        f(&[0x61, 0xE9, 0x20, 0x1F4A9]).bytes,
        b"a\xC3\xA9 \xF0\x9F\x92\xA9"
    );

    assert_eq!(f(&[0xD83D, 0xDCA9]).bytes, b"\xF0\x9F\x92\xA9"); // Magic!
    assert_eq!(
        f(&[0xD83D, 0x20, 0xDCA9]).bytes,
        b"\xED\xA0\xBD \xED\xB2\xA9"
    );
    assert_eq!(f(&[0xD800, 0xDBFF]).bytes, b"\xED\xA0\x80\xED\xAF\xBF");
    assert_eq!(f(&[0xD800, 0xE000]).bytes, b"\xED\xA0\x80\xEE\x80\x80");
    assert_eq!(f(&[0xD7FF, 0xDC00]).bytes, b"\xED\x9F\xBF\xED\xB0\x80");
    assert_eq!(f(&[0x61, 0xDC00]).bytes, b"\x61\xED\xB0\x80");
    assert_eq!(f(&[0xDC00]).bytes, b"\xED\xB0\x80");
}

#[test]
fn wtf8buf_extend() {
    fn e(initial: &[u32], extended: &[u32]) -> Wtf8String {
        fn c(value: &u32) -> Codepoint {
            Codepoint::from_u32(*value).unwrap()
        }
        let mut string = initial.iter().map(c).collect::<Wtf8String>();
        string.extend(extended.iter().map(c));
        string
    }

    assert_eq!(
        e(&[0x61, 0xE9], &[0x20, 0x1F4A9]).bytes,
        b"a\xC3\xA9 \xF0\x9F\x92\xA9"
    );

    assert_eq!(e(&[0xD83D], &[0xDCA9]).bytes, b"\xF0\x9F\x92\xA9"); // Magic!
    assert_eq!(
        e(&[0xD83D, 0x20], &[0xDCA9]).bytes,
        b"\xED\xA0\xBD \xED\xB2\xA9"
    );
    assert_eq!(e(&[0xD800], &[0xDBFF]).bytes, b"\xED\xA0\x80\xED\xAF\xBF");
    assert_eq!(e(&[0xD800], &[0xE000]).bytes, b"\xED\xA0\x80\xEE\x80\x80");
    assert_eq!(e(&[0xD7FF], &[0xDC00]).bytes, b"\xED\x9F\xBF\xED\xB0\x80");
    assert_eq!(e(&[0x61], &[0xDC00]).bytes, b"\x61\xED\xB0\x80");
    assert_eq!(e(&[], &[0xDC00]).bytes, b"\xED\xB0\x80");
}

#[test]
fn wtf8buf_show() {
    let mut string = Wtf8String::from_borrowed("a\té \u{7f}💩\r");
    string.push(Codepoint::from_u32(0xD800).unwrap());
    assert_eq!(
        format!("{string:?}"),
        "\"a\\té \\u{7f}\u{1f4a9}\\r\\u{d800}\""
    );
}

#[test]
fn wtf8buf_as_slice() {
    assert_eq!(
        Wtf8String::from_borrowed("aé").as_wstr(),
        Wtf8Str::new("aé")
    );
}

#[test]
fn wtf8buf_show_str() {
    let text = "a\té 💩\r";
    let string = Wtf8String::from_borrowed(text);
    assert_eq!(format!("{text:?}"), format!("{string:?}"));
}

#[test]
fn wtf8_from_borrowed() {
    assert_eq!(&Wtf8Str::new("").bytes, b"");
    assert_eq!(&Wtf8Str::new("aé 💩").bytes, b"a\xC3\xA9 \xF0\x9F\x92\xA9");
}

#[test]
fn wtf8_len() {
    assert_eq!(Wtf8Str::new("").len(), 0);
    assert_eq!(Wtf8Str::new("aé 💩").len(), 8);
}

#[test]
fn wtf8_slice() {
    assert_eq!(&Wtf8Str::new("aé 💩")[1..4].bytes, b"\xC3\xA9 ");
}

#[test]
#[should_panic]
fn wtf8_slice_not_code_point_boundary() {
    let _ = &Wtf8Str::new("aé 💩")[2..4];
}

#[test]
fn wtf8_slice_from() {
    assert_eq!(
        &Wtf8Str::new("aé 💩")[1..].bytes,
        b"\xC3\xA9 \xF0\x9F\x92\xA9"
    );
}

#[test]
#[should_panic]
fn wtf8_slice_from_not_code_point_boundary() {
    let _ = &Wtf8Str::new("aé 💩")[2..];
}

#[test]
fn wtf8_slice_to() {
    assert_eq!(&Wtf8Str::new("aé 💩")[..4].bytes, b"a\xC3\xA9 ");
}

#[test]
#[should_panic]
fn wtf8_slice_to_not_code_point_boundary() {
    let _ = &Wtf8Str::new("aé 💩")[5..];
}

#[test]
fn wtf8_ascii_byte_at() {
    let slice = Wtf8Str::new("aé 💩");
    assert_eq!(slice.ascii_byte_at(0), b'a');
    assert_eq!(slice.ascii_byte_at(1), b'\xFF');
    assert_eq!(slice.ascii_byte_at(2), b'\xFF');
    assert_eq!(slice.ascii_byte_at(3), b' ');
    assert_eq!(slice.ascii_byte_at(4), b'\xFF');
}

#[test]
fn wtf8_code_points() {
    fn c(value: u32) -> Codepoint {
        Codepoint::from_u32(value).unwrap()
    }
    fn cp(string: &Wtf8String) -> Vec<Option<char>> {
        string
            .code_points()
            .map(|c| c.to_char())
            .collect::<Vec<_>>()
    }
    let mut string = Wtf8String::from_borrowed("é ");
    assert_eq!(cp(&string), [Some('é'), Some(' ')]);
    string.push(c(0xD83D));
    assert_eq!(cp(&string), [Some('é'), Some(' '), None]);
    string.push(c(0xDCA9));
    assert_eq!(cp(&string), [Some('é'), Some(' '), Some('💩')]);
}

#[test]
fn wtf8_as_str() {
    assert_eq!(Wtf8Str::new("").as_str(), Some(""));
    assert_eq!(Wtf8Str::new("aé 💩").as_str(), Some("aé 💩"));
    let mut string = Wtf8String::new();
    string.push(Codepoint::from_u32(0xD800).unwrap());
    assert_eq!(string.as_str(), None);
}

#[test]
fn wtf8_to_string_lossy() {
    assert_eq!(Wtf8Str::new("").to_string_lossy(), Cow::Borrowed(""));
    assert_eq!(
        Wtf8Str::new("aé 💩").to_string_lossy(),
        Cow::Borrowed("aé 💩")
    );
    let mut string = Wtf8String::from_borrowed("aé 💩");
    string.push(Codepoint::from_u32(0xD800).unwrap());
    let expected: Cow<'_, str> = Cow::Owned(String::from("aé 💩�"));
    assert_eq!(string.to_string_lossy(), expected);
}

#[test]
fn wtf8_display() {
    fn d(b: &[u8]) -> String {
        (&unsafe { Wtf8Str::from_bytes_unchecked(b) }).to_string()
    }

    assert_eq!("", d("".as_bytes()));
    assert_eq!("aé 💩", d("aé 💩".as_bytes()));

    let mut string = Wtf8String::from_borrowed("aé 💩");
    string.push(Codepoint::from_u32(0xD800).unwrap());
    assert_eq!("aé 💩�", d(string.as_bytes()));
}

#[test]
fn wtf8_encode_wide() {
    let mut string = Wtf8String::from_borrowed("aé ");
    string.push(Codepoint::from_u32(0xD83D).unwrap());
    string.push_char('💩');
    assert_eq!(
        string.encode_wide().collect::<Vec<_>>(),
        vec![0x61, 0xE9, 0x20, 0xD83D, 0xD83D, 0xDCA9]
    );
}

#[test]
fn wtf8_encode_wide_size_hint() {
    let string = Wtf8String::from_borrowed("\u{12345}");
    let mut iter = string.encode_wide();
    assert_eq!((1, Some(8)), iter.size_hint());
    iter.next().unwrap();
    assert_eq!((1, Some(1)), iter.size_hint());
    iter.next().unwrap();
    assert_eq!((0, Some(0)), iter.size_hint());
    assert!(iter.next().is_none());
}
