//! Implementation of [the WTF-8 encoding](https://simonsapin.github.io/wtf-8/).
//!
//! This library uses Rust’s type system to maintain
//! [well-formedness](https://simonsapin.github.io/wtf-8/#well-formed),
//! like the `String` and `&str` types do for UTF-8.
//!
//! [WTF-8 must not be used for interchange]
//! (https://simonsapin.github.io/wtf-8/#intended-audience),
//! so bytes exposed from this library should only be used when dealing with
//! nasty things such as Java's class format.
//!
//! WTF-8 strings can be obtained from UTF-8, UTF-16, or code points.
#![cfg_attr(not(feature = "alloc"), allow(dead_code))]
#![cfg_attr(all(not(test), not(feature = "std")), no_std)]
#![deny(unsafe_op_in_unsafe_fn)]

// derived from https://github.com/rust-lang/rust/blob/2f847b81a0d8633f200f2c2269c1c43fe9e7def3/library/std/src/sys_common/wtf8.rs

#[cfg(feature = "alloc")]
extern crate alloc;

#[cfg(all(test, feature = "alloc"))]
mod tests;

mod std_internal;

use core::hash::{Hash, Hasher};
use core::iter::FusedIterator;
use core::num::NonZeroU8;
use core::{char, fmt, ops, slice, str};

use std_internal::next_code_point;

const UTF8_REPLACEMENT_CHARACTER: &str = "\u{FFFD}";

/// A Unicode code point: from U+0000 to U+10FFFF.
///
/// Compares with the `char` type,
/// which represents a Unicode scalar value:
/// a code point that is not a surrogate (U+D800 to U+DFFF).
#[derive(Eq, PartialEq, Ord, PartialOrd, Clone, Copy)]
#[repr(transparent)]
pub struct Codepoint {
    value: u32,
}

/// Format the code point as `U+` followed by four to six hexadecimal digits.
/// Example: `U+1F4A9`
impl fmt::Debug for Codepoint {
    #[inline]
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "U+{:04X}", self.value)
    }
}

impl Codepoint {
    /// Unsafely creates a new `CodePoint` without checking the value.
    ///
    /// # Safety
    /// Only use when `value` is known to be less than or equal to 0x10FFFF.
    #[inline]
    pub unsafe fn from_u32_unchecked(value: u32) -> Codepoint {
        Codepoint { value }
    }

    /// Creates a new `CodePoint` if the value is a valid code point.
    ///
    /// Returns `None` if `value` is above 0x10FFFF.
    #[inline]
    pub fn from_u32(value: u32) -> Option<Codepoint> {
        match value {
            0..=0x10FFFF => Some(Codepoint { value }),
            _ => None,
        }
    }

    /// Creates a new `CodePoint` from a `char`.
    ///
    /// Since all Unicode scalar values are code points, this always succeeds.
    #[inline]
    pub fn from_char(value: char) -> Codepoint {
        Codepoint {
            value: value as u32,
        }
    }

    /// Returns the numeric value of the code point.
    #[inline]
    pub fn to_u32(&self) -> u32 {
        self.value
    }

    /// Optionally returns a Unicode scalar value for the code point.
    ///
    /// Returns `None` if the code point is a surrogate (from U+D800 to U+DFFF).
    #[inline]
    pub fn to_char(&self) -> Option<char> {
        match self.value {
            0xD800..=0xDFFF => None,
            // SAFETY: Codepoint = char | D800-DFFF
            // we checked that it is not in surrogate range so it is safe.
            _ => Some(unsafe { char::from_u32_unchecked(self.value) }),
        }
    }

    /// Returns a Unicode scalar value for the code point.
    ///
    /// Returns `'\u{FFFD}'` (the replacement character “�”)
    /// if the code point is a surrogate (from U+D800 to U+DFFF).
    #[inline]
    pub fn to_char_lossy(&self) -> char {
        self.to_char().unwrap_or('\u{FFFD}')
    }
}

/// A borrowed slice of well-formed WTF-8 data.
///
/// Similar to `&str`, but can additionally contain surrogate code points
/// if they’re not in a surrogate pair.
#[derive(Eq, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Wtf8Str {
    bytes: [u8],
}

/// Format the slice with double quotes,
/// and surrogates as `\u` followed by four hexadecimal digits.
/// Example: `"a\u{D800}"` for a slice with code points [U+0061, U+D800]
impl fmt::Debug for Wtf8Str {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn write_str_escaped(f: &mut fmt::Formatter<'_>, s: &str) -> fmt::Result {
            use crate::fmt::Write;
            for c in s.chars().flat_map(|c| c.escape_debug()) {
                f.write_char(c)?
            }
            Ok(())
        }

        formatter.write_str("\"")?;
        let mut pos = 0;
        while let Some((surrogate_pos, surrogate)) = self.next_surrogate(pos) {
            write_str_escaped(formatter, unsafe {
                str::from_utf8_unchecked(&self.bytes[pos..surrogate_pos])
            })?;
            write!(formatter, "\\u{{{:x}}}", surrogate)?;
            pos = surrogate_pos + 3;
        }
        write_str_escaped(formatter, unsafe {
            str::from_utf8_unchecked(&self.bytes[pos..])
        })?;
        formatter.write_str("\"")
    }
}

impl fmt::Display for Wtf8Str {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let wtf8_bytes = &self.bytes;
        let mut pos = 0;
        loop {
            match self.next_surrogate(pos) {
                Some((surrogate_pos, _)) => {
                    formatter.write_str(unsafe {
                        str::from_utf8_unchecked(&wtf8_bytes[pos..surrogate_pos])
                    })?;
                    formatter.write_str(UTF8_REPLACEMENT_CHARACTER)?;
                    pos = surrogate_pos + 3;
                }
                None => {
                    let s = unsafe { str::from_utf8_unchecked(&wtf8_bytes[pos..]) };
                    if pos == 0 {
                        return s.fmt(formatter);
                    } else {
                        return formatter.write_str(s);
                    }
                }
            }
        }
    }
}

impl Wtf8Str {
    /// Creates a WTF-8 slice from a UTF-8 `&str` slice.
    ///
    /// Since WTF-8 is a superset of UTF-8, this always succeeds.
    #[inline]
    pub fn new(value: &str) -> &Wtf8Str {
        unsafe { Wtf8Str::from_bytes_unchecked(value.as_bytes()) }
    }

    /// Creates a WTF-8 slice from a WTF-8 byte slice without checking that it is valid.
    ///
    /// Please DO NOT use this for interchange!
    ///
    /// # Safety
    ///
    /// Bytes supplied must be valid WTF-8.
    #[inline]
    pub unsafe fn from_bytes_unchecked(value: &[u8]) -> &Wtf8Str {
        // SAFETY: callers must ensure that the bytems are valid WTF-8
        unsafe { &*(value as *const [u8] as *const Wtf8Str) }
    }

    /// Creates a mutable WTF-8 slice from a mutable WTF-8 byte slice without checking
    /// that it is valid.
    ///
    /// Please DO NOT use this for interchange!
    ///
    /// # Safety
    ///
    /// Bytes supplied must be valid WTF-8.
    #[inline]
    pub unsafe fn from_bytes_mut_unchecked(value: &mut [u8]) -> &mut Wtf8Str {
        // SAFETY: callers must ensure that the bytems are valid WTF-8
        unsafe { &mut *(value as *mut [u8] as *mut Wtf8Str) }
    }

    /// Creates a WTF-8 slice from a WTF-8 byte slice.
    ///
    /// Please DO NOT use this for interchange!
    pub fn from_bytes(bytes: &[u8]) -> Result<&Wtf8Str, Wtf8Error> {
        validate_wtf8(bytes)?;
        // SAFETY: we just checked that it is valid wtf-8.
        Ok(unsafe { Self::from_bytes_unchecked(bytes) })
    }

    /// Creates a mutable WTF-8 slice from a mutable WTF-8 byte slice.
    ///
    /// Please DO NOT use this for interchange!
    pub fn from_bytes_mut(bytes: &mut [u8]) -> Result<&mut Wtf8Str, Wtf8Error> {
        validate_wtf8(bytes)?;
        // SAFETY: we just checked that it is valid wtf-8.
        Ok(unsafe { Self::from_bytes_mut_unchecked(bytes) })
    }

    /// Returns the length, in WTF-8 bytes.
    #[inline]
    pub fn len(&self) -> usize {
        self.bytes.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.bytes.is_empty()
    }

    /// Returns the wtf-8 encoded bytes for this string.
    /// This should not be used for interchange!
    pub fn as_bytes(&self) -> &[u8] {
        &self.bytes
    }

    /// Returns the code point at `position` if it is in the ASCII range,
    /// or `b'\xFF' otherwise.
    ///
    /// # Panics
    ///
    /// Panics if `position` is beyond the end of the string.
    #[inline]
    pub fn ascii_byte_at(&self, position: usize) -> u8 {
        match self.bytes[position] {
            ascii_byte @ 0x00..=0x7F => ascii_byte,
            _ => 0xFF,
        }
    }

    /// Returns an iterator for the string’s code points.
    #[inline]
    pub fn code_points(&self) -> Wtf8CodePoints<'_> {
        Wtf8CodePoints {
            bytes: self.bytes.iter(),
        }
    }

    /// Tries to convert the string to UTF-8 and return a `&str` slice.
    ///
    /// Returns `None` if the string contains surrogates.
    ///
    /// This does not copy the data.
    #[inline]
    pub fn as_str(&self) -> Option<&str> {
        // Well-formed WTF-8 is also well-formed UTF-8
        // if and only if it contains no surrogate.
        match self.next_surrogate(0) {
            None => Some(unsafe { str::from_utf8_unchecked(&self.bytes) }),
            Some(_) => None,
        }
    }

    /// Converts the WTF-8 string to potentially ill-formed UTF-16
    /// and return an iterator of 16-bit code units.
    ///
    /// This is lossless:
    /// calling `Wtf8String::from_ill_formed_utf16` on the resulting code units
    /// would always return the original WTF-8 string.
    #[inline]
    pub fn encode_wide(&self) -> EncodeWide<'_> {
        EncodeWide {
            code_points: self.code_points(),
            extra: 0,
        }
    }

    #[inline]
    fn next_surrogate(&self, mut pos: usize) -> Option<(usize, u16)> {
        let mut iter = self.bytes[pos..].iter();
        loop {
            let b = *iter.next()?;
            if b < 0x80 {
                pos += 1;
            } else if b < 0xE0 {
                iter.next();
                pos += 2;
            } else if b == 0xED {
                match (iter.next(), iter.next()) {
                    (Some(&b2), Some(&b3)) if b2 >= 0xA0 => {
                        return Some((pos, decode_surrogate(b2, b3)));
                    }
                    _ => pos += 3,
                }
            } else if b < 0xF0 {
                iter.next();
                iter.next();
                pos += 3;
            } else {
                iter.next();
                iter.next();
                iter.next();
                pos += 4;
            }
        }
    }

    #[inline]
    fn final_lead_surrogate(&self) -> Option<u16> {
        match self.bytes {
            [.., 0xED, b2 @ 0xA0..=0xAF, b3] => Some(decode_surrogate(b2, b3)),
            _ => None,
        }
    }

    #[inline]
    fn initial_trail_surrogate(&self) -> Option<u16> {
        match self.bytes {
            [0xED, b2 @ 0xB0..=0xBF, b3, ..] => Some(decode_surrogate(b2, b3)),
            _ => None,
        }
    }

    #[inline]
    pub fn make_ascii_lowercase(&mut self) {
        self.bytes.make_ascii_lowercase()
    }

    #[inline]
    pub fn make_ascii_uppercase(&mut self) {
        self.bytes.make_ascii_uppercase()
    }

    #[inline]
    pub fn is_ascii(&self) -> bool {
        self.bytes.is_ascii()
    }

    #[inline]
    pub fn eq_ignore_ascii_case(&self, other: &Self) -> bool {
        self.bytes.eq_ignore_ascii_case(&other.bytes)
    }
}

#[cfg(feature = "alloc")]
mod alloc_impl {
    use core::borrow::Borrow;
    use core::hash::{Hash, Hasher};
    use core::{mem, ops, fmt};
    use core::str;

    use alloc::borrow::{Cow, ToOwned};
    use alloc::boxed::Box;
    use alloc::collections::TryReserveError;
    use alloc::rc::Rc;
    use alloc::string::String;
    use alloc::sync::Arc;
    use alloc::vec::Vec;

    use crate::std_internal::encode_utf8_raw;
    use crate::{Wtf8Str, Codepoint, UTF8_REPLACEMENT_CHARACTER, validate_wtf8, decode_surrogate_pair, is_code_point_boundary, Wtf8Error};

    /// An owned, growable string of well-formed WTF-8 data.
    ///
    /// Similar to `String`, but can additionally contain surrogate code points
    /// if they’re not in a surrogate pair.
    #[derive(Eq, PartialEq, Ord, PartialOrd, Clone, Default)]
    #[repr(transparent)]
    pub struct Wtf8String {
        pub(crate) bytes: Vec<u8>,
    }

    impl ops::Deref for Wtf8String {
        type Target = Wtf8Str;

        fn deref(&self) -> &Wtf8Str {
            self.as_wstr()
        }
    }

    impl ops::DerefMut for Wtf8String {
        fn deref_mut(&mut self) -> &mut Wtf8Str {
            self.as_mut_wstr()
        }
    }

    /// Format the string with double quotes,
    /// and surrogates as `\u` followed by four hexadecimal digits.
    /// Example: `"a\u{D800}"` for a string with code points [U+0061, U+D800]
    impl fmt::Debug for Wtf8String {
        #[inline]
        fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            fmt::Debug::fmt(&**self, formatter)
        }
    }

    impl Wtf8String {
        /// Creates a new, empty WTF-8 string.
        #[inline]
        pub fn new() -> Wtf8String {
            Wtf8String { bytes: Vec::new() }
        }

        /// Creates a new, empty WTF-8 string with pre-allocated capacity for `capacity` bytes.
        #[inline]
        pub fn with_capacity(capacity: usize) -> Wtf8String {
            Wtf8String {
                bytes: Vec::with_capacity(capacity),
            }
        }

        /// Creates a WTF-8 string from a UTF-8 `String`.
        ///
        /// This takes ownership of the `String` and does not copy.
        ///
        /// Since WTF-8 is a superset of UTF-8, this always succeeds.
        #[inline]
        pub fn from_string(string: String) -> Wtf8String {
            Wtf8String {
                bytes: string.into_bytes(),
            }
        }

        /// Creates a WTF-8 string from WTF-8 bytes, without checking its validity.
        ///
        /// This does not copy.
        ///
        /// Please DO NOT use this for interchange!
        ///
        /// # Safety
        ///
        /// Supplied bytes must be valid WTF-8.
        pub unsafe fn from_bytes_unchecked(bytes: Vec<u8>) -> Wtf8String {
            Wtf8String { bytes }
        }

        /// Creates a WTF-8 string from WTF-8 bytes.
        ///
        /// This does not copy.
        ///
        /// Please DO NOT use this for interchange!
        pub fn from_bytes(bytes: Vec<u8>) -> Result<Wtf8String, Wtf8Error> {
            validate_wtf8(&bytes)?;
            // SAFETY: we just checked that it is valid WTF-8.
            Ok(unsafe { Wtf8String::from_bytes_unchecked(bytes) })
        }

        /// Creates a WTF-8 string from a UTF-8 `&str` slice.
        ///
        /// This copies the content of the slice.
        ///
        /// Since WTF-8 is a superset of UTF-8, this always succeeds.
        #[inline]
        pub fn from_borrowed(str: &str) -> Wtf8String {
            Wtf8String {
                bytes: <[_]>::to_vec(str.as_bytes()),
            }
        }

        pub fn clear(&mut self) {
            self.bytes.clear()
        }

        /// Creates a WTF-8 string from a potentially ill-formed UTF-16 slice of 16-bit code units.
        ///
        /// This is lossless: calling `.encode_wide()` on the resulting string
        /// will always return the original code units.
        pub fn from_wide(v: &[u16]) -> Wtf8String {
            let mut string = Wtf8String::with_capacity(v.len());
            for item in char::decode_utf16(v.iter().cloned()) {
                match item {
                    Ok(ch) => string.push_char(ch),
                    Err(surrogate) => {
                        let surrogate = surrogate.unpaired_surrogate();
                        // SAFETY: Surrogates are known to be in the code point range.
                        let code_point = unsafe { Codepoint::from_u32_unchecked(surrogate as u32) };
                        // SAFETY: Skip the WTF-8 concatenation check,
                        // surrogate pairs are already decoded by decode_utf16
                        unsafe { string.push_code_point_unchecked(code_point) }
                    }
                }
            }
            string
        }

        /// Copied from String::push
        /// This does **not** include the WTF-8 concatenation check, where paired surrogates are fixed.
        ///
        /// # Safety
        /// Callers must ensure that the last codepoint for the string does not contain a high surrogate codepoint
        /// when inserting a low surrogate codepoint.
        pub unsafe fn push_code_point_unchecked(&mut self, code_point: Codepoint) {
            let mut bytes = [0; 4];
            let bytes = encode_utf8_raw(code_point.value, &mut bytes);
            self.bytes.extend_from_slice(bytes)
        }

        #[inline]
        pub fn as_wstr(&self) -> &Wtf8Str {
            // SAFETY: Valid `Wtf8String`s are also valid `Wtf8Str`s.
            unsafe { Wtf8Str::from_bytes_unchecked(&self.bytes) }
        }

        #[inline]
        pub fn as_mut_wstr(&mut self) -> &mut Wtf8Str {
            // SAFETY: Valid `Wtf8String`s are also valid `Wtf8Str`s.
            unsafe { Wtf8Str::from_bytes_mut_unchecked(&mut self.bytes) }
        }

        pub fn into_bytes(self) -> Vec<u8> {
            self.bytes
        }

        /// Reserves capacity for at least `additional` more bytes to be inserted
        /// in the given `Wtf8String`.
        /// The collection may reserve more space to avoid frequent reallocations.
        ///
        /// # Panics
        ///
        /// Panics if the new capacity overflows `usize`.
        #[inline]
        pub fn reserve(&mut self, additional: usize) {
            self.bytes.reserve(additional)
        }

        /// Tries to reserve capacity for at least `additional` more length units
        /// in the given `Wtf8String`. The `Wtf8String` may reserve more space to avoid
        /// frequent reallocations. After calling `try_reserve`, capacity will be
        /// greater than or equal to `self.len() + additional`. Does nothing if
        /// capacity is already sufficient.
        ///
        /// # Errors
        ///
        /// If the capacity overflows, or the allocator reports a failure, then an error
        /// is returned.
        #[inline]
        pub fn try_reserve(&mut self, additional: usize) -> Result<(), TryReserveError> {
            self.bytes.try_reserve(additional)
        }

        #[inline]
        pub fn reserve_exact(&mut self, additional: usize) {
            self.bytes.reserve_exact(additional)
        }

        /// Tries to reserve the minimum capacity for exactly `additional`
        /// length units in the given `Wtf8String`. After calling
        /// `try_reserve_exact`, capacity will be greater than or equal to
        /// `self.len() + additional` if it returns `Ok(())`.
        /// Does nothing if the capacity is already sufficient.
        ///
        /// Note that the allocator may give the `Wtf8String` more space than it
        /// requests. Therefore, capacity can not be relied upon to be precisely
        /// minimal. Prefer [`try_reserve`] if future insertions are expected.
        ///
        /// [`try_reserve`]: Wtf8String::try_reserve
        ///
        /// # Errors
        ///
        /// If the capacity overflows, or the allocator reports a failure, then an error
        /// is returned.
        #[inline]
        pub fn try_reserve_exact(&mut self, additional: usize) -> Result<(), TryReserveError> {
            self.bytes.try_reserve_exact(additional)
        }

        #[inline]
        pub fn shrink_to_fit(&mut self) {
            self.bytes.shrink_to_fit()
        }

        #[inline]
        pub fn shrink_to(&mut self, min_capacity: usize) {
            self.bytes.shrink_to(min_capacity)
        }

        /// Returns the number of bytes that this string buffer can hold without reallocating.
        #[inline]
        pub fn capacity(&self) -> usize {
            self.bytes.capacity()
        }

        /// Append a UTF-8 slice at the end of the string.
        #[inline]
        pub fn push_str(&mut self, other: &str) {
            self.bytes.extend_from_slice(other.as_bytes())
        }

        /// Append a WTF-8 slice at the end of the string.
        ///
        /// This replaces newly paired surrogates at the boundary
        /// with a supplementary code point,
        /// like concatenating ill-formed UTF-16 strings effectively would.
        #[inline]
        pub fn push_wtf8(&mut self, other: &Wtf8Str) {
            match (
                (&*self).final_lead_surrogate(),
                other.initial_trail_surrogate(),
            ) {
                // Replace newly paired surrogates by a supplementary code point.
                (Some(lead), Some(trail)) => {
                    let len_without_lead_surrogate = self.len() - 3;
                    self.bytes.truncate(len_without_lead_surrogate);
                    let other_without_trail_surrogate = &other.bytes[3..];
                    // 4 bytes for the supplementary code point
                    self.bytes.reserve(4 + other_without_trail_surrogate.len());
                    self.push_char(decode_surrogate_pair(lead, trail));
                    self.bytes.extend_from_slice(other_without_trail_surrogate);
                }
                _ => self.bytes.extend_from_slice(&other.bytes),
            }
        }

        /// Append a Unicode scalar value at the end of the string.
        #[inline]
        pub fn push_char(&mut self, c: char) {
            // SAFETY: `char`s are not surrogates.
            unsafe { self.push_code_point_unchecked(Codepoint::from_char(c)) }
        }

        /// Append a code point at the end of the string.
        ///
        /// This replaces newly paired surrogates at the boundary
        /// with a supplementary code point,
        /// like concatenating ill-formed UTF-16 strings effectively would.
        #[inline]
        pub fn push(&mut self, code_point: Codepoint) {
            if let trail @ 0xDC00..=0xDFFF = code_point.to_u32() {
                if let Some(lead) = (&*self).final_lead_surrogate() {
                    let len_without_lead_surrogate = self.len() - 3;
                    self.bytes.truncate(len_without_lead_surrogate);
                    self.push_char(decode_surrogate_pair(lead, trail as u16));
                    return;
                }
            }

            // SAFETY: No newly paired surrogates at the boundary.
            unsafe { self.push_code_point_unchecked(code_point) }
        }

        /// Shortens a string to the specified length.
        ///
        /// # Panics
        ///
        /// Panics if `new_len` > current length,
        /// or if `new_len` is not a code point boundary.
        #[inline]
        pub fn truncate(&mut self, new_len: usize) {
            assert!(is_code_point_boundary(self, new_len));
            self.bytes.truncate(new_len)
        }

        /// Consumes the WTF-8 string and tries to convert it to UTF-8.
        ///
        /// This does not copy the data.
        ///
        /// If the contents are not well-formed UTF-8
        /// (that is, if the string contains surrogates),
        /// the original WTF-8 string is returned instead.
        pub fn into_string(self) -> Result<String, Wtf8String> {
            match self.next_surrogate(0) {
                // SAFETY: We do not have any surrogates so it is a valid utf-8 sequence.
                None => Ok(unsafe { String::from_utf8_unchecked(self.bytes) }),
                Some(_) => Err(self),
            }
        }

        /// Consumes the WTF-8 string and converts it lossily to UTF-8.
        ///
        /// This does not copy the data (but may overwrite parts of it in place).
        ///
        /// Surrogates are replaced with `"\u{FFFD}"` (the replacement character “�”)
        pub fn into_string_lossy(mut self) -> String {
            let mut pos = 0;
            loop {
                match self.next_surrogate(pos) {
                    Some((surrogate_pos, _)) => {
                        pos = surrogate_pos + 3;
                        self.bytes[surrogate_pos..pos]
                            .copy_from_slice(UTF8_REPLACEMENT_CHARACTER.as_bytes());
                    }
                    // SAFETY: We do not have any surrogates so it is a valid utf-8 sequence.
                    None => return unsafe { String::from_utf8_unchecked(self.bytes) },
                }
            }
        }

        /// Converts this `Wtf8String` into a boxed `Wtf8Str`.
        #[inline]
        pub fn into_box(self) -> Box<Wtf8Str> {
            // SAFETY: `Box<str>` is the same as `Box<Wtf8Str>` in terms of layouts.
            // this is because `str` has the same layout as `[u8]` and `Wtf8Str` is
            // `#[repr(transparent)]` over `[u8]`.
            unsafe { mem::transmute(self.bytes.into_boxed_slice()) }
        }

        /// Converts a `Box<Wtf8>` into a `Wtf8String`.
        pub fn from_box(boxed: Box<Wtf8Str>) -> Wtf8String {
            // SAFETY: `Wtf8Str` is `#[repr(transparent)]` over `[u8]`.
            let bytes: Box<[u8]> = unsafe { mem::transmute(boxed) };
            Wtf8String {
                bytes: bytes.into_vec(),
            }
        }
    }

    #[allow(clippy::derive_hash_xor_eq)]
    impl Hash for Wtf8String {
        #[inline]
        fn hash<H: Hasher>(&self, state: &mut H) {
            state.write(&self.bytes);
            0xfeu8.hash(state)
        }
    }

    impl Wtf8Str {
        /// Lossily converts the string to UTF-8.
        /// Returns a UTF-8 `&str` slice if the contents are well-formed in UTF-8.
        ///
        /// Surrogates are replaced with `"\u{FFFD}"` (the replacement character “�”).
        ///
        /// This only copies the data if necessary (if it contains any surrogate).
        pub fn to_string_lossy(&self) -> Cow<'_, str> {
            let surrogate_pos = match self.next_surrogate(0) {
                None => return Cow::Borrowed(unsafe { str::from_utf8_unchecked(&self.bytes) }),
                Some((pos, _)) => pos,
            };
            let wtf8_bytes = &self.bytes;
            let mut utf8_bytes = Vec::with_capacity(self.len());
            utf8_bytes.extend_from_slice(&wtf8_bytes[..surrogate_pos]);
            utf8_bytes.extend_from_slice(UTF8_REPLACEMENT_CHARACTER.as_bytes());
            let mut pos = surrogate_pos + 3;
            loop {
                match self.next_surrogate(pos) {
                    Some((surrogate_pos, _)) => {
                        utf8_bytes.extend_from_slice(&wtf8_bytes[pos..surrogate_pos]);
                        utf8_bytes.extend_from_slice(UTF8_REPLACEMENT_CHARACTER.as_bytes());
                        pos = surrogate_pos + 3;
                    }
                    None => {
                        utf8_bytes.extend_from_slice(&wtf8_bytes[pos..]);
                        return Cow::Owned(unsafe { String::from_utf8_unchecked(utf8_bytes) });
                    }
                }
            }
        }

        pub fn clone_into(&self, buf: &mut Wtf8String) {
            // drop anything in target that will not be overwritten
            buf.bytes.truncate(self.len());

            // target.len <= self.len due to the truncate above, so the
            // slices here are always in-bounds.
            let (init, tail) = self.bytes.split_at(buf.len());

            // reuse the contained values' allocations/resources.
            buf.bytes.clone_from_slice(init);
            buf.bytes.extend_from_slice(tail);
        }

        #[inline]
        pub fn to_ascii_lowercase(&self) -> Wtf8String {
            Wtf8String {
                bytes: self.bytes.to_ascii_lowercase(),
            }
        }

        #[inline]
        pub fn to_ascii_uppercase(&self) -> Wtf8String {
            Wtf8String {
                bytes: self.bytes.to_ascii_uppercase(),
            }
        }

        /// Boxes this `Wtf8Str`.
        #[inline]
        pub fn into_box(&self) -> Box<Wtf8Str> {
            let boxed: Box<[u8]> = self.bytes.into();
            unsafe { mem::transmute(boxed) }
        }

        /// Creates a boxed, empty `Wtf8Str`.
        pub fn empty_box() -> Box<Wtf8Str> {
            let boxed: Box<[u8]> = Default::default();
            unsafe { mem::transmute(boxed) }
        }

        #[inline]
        pub fn into_arc(&self) -> Arc<Wtf8Str> {
            let arc: Arc<[u8]> = Arc::from(&self.bytes);
            unsafe { Arc::from_raw(Arc::into_raw(arc) as *const Wtf8Str) }
        }

        #[inline]
        pub fn into_rc(&self) -> Rc<Wtf8Str> {
            let rc: Rc<[u8]> = Rc::from(&self.bytes);
            unsafe { Rc::from_raw(Rc::into_raw(rc) as *const Wtf8Str) }
        }
    }

    /// Creates a new WTF-8 string from an iterator of code points.
    ///
    /// This replaces surrogate code point pairs with supplementary code points,
    /// like concatenating ill-formed UTF-16 strings effectively would.
    impl FromIterator<Codepoint> for Wtf8String {
        fn from_iter<T: IntoIterator<Item = Codepoint>>(iter: T) -> Wtf8String {
            let mut string = Wtf8String::new();
            string.extend(iter);
            string
        }
    }

    /// Append code points from an iterator to the string.
    ///
    /// This replaces surrogate code point pairs with supplementary code points,
    /// like concatenating ill-formed UTF-16 strings effectively would.
    impl Extend<Codepoint> for Wtf8String {
        fn extend<T: IntoIterator<Item = Codepoint>>(&mut self, iter: T) {
            let iterator = iter.into_iter();
            let (low, _high) = iterator.size_hint();
            // Lower bound of one byte per code point (ASCII only)
            self.bytes.reserve(low);
            iterator.for_each(move |code_point| self.push(code_point));
        }

        // TODO https://github.com/rust-lang/rust/issues/72631
        /* #[inline]
        fn extend_one(&mut self, code_point: Codepoint) {
            self.push(code_point);
        }

        #[inline]
        fn extend_reserve(&mut self, additional: usize) {
            // Lower bound of one byte per code point (ASCII only)
            self.bytes.reserve(additional);
        } */
    }
    impl Borrow<Wtf8Str> for Wtf8String {
        fn borrow(&self) -> &Wtf8Str {
            &*self
        }
    }

    impl ToOwned for Wtf8Str {
        type Owned = Wtf8String;
        fn to_owned(&self) -> Self::Owned {
            Wtf8String::from(self)
        }
    }

    impl<'a> From<&'a Wtf8Str> for Wtf8String {
        fn from(s: &'a Wtf8Str) -> Self {
            Wtf8String {
                bytes: s.bytes.to_owned(),
            }
        }
    }
}

#[cfg(feature = "alloc")]
pub use alloc_impl::Wtf8String;

/// Returns a slice of the given string for the byte range \[`begin`..`end`).
///
/// # Panics
///
/// Panics when `begin` and `end` do not point to code point boundaries,
/// or point beyond the end of the string.
impl ops::Index<ops::Range<usize>> for Wtf8Str {
    type Output = Wtf8Str;

    #[inline]
    fn index(&self, range: ops::Range<usize>) -> &Wtf8Str {
        // is_code_point_boundary checks that the index is in [0, .len()]
        if range.start <= range.end
            && is_code_point_boundary(self, range.start)
            && is_code_point_boundary(self, range.end)
        {
            unsafe { slice_unchecked(self, range.start, range.end) }
        } else {
            slice_error_fail(self, range.start, range.end)
        }
    }
}

/// Returns a slice of the given string from byte `begin` to its end.
///
/// # Panics
///
/// Panics when `begin` is not at a code point boundary,
/// or is beyond the end of the string.
impl ops::Index<ops::RangeFrom<usize>> for Wtf8Str {
    type Output = Wtf8Str;

    #[inline]
    fn index(&self, range: ops::RangeFrom<usize>) -> &Wtf8Str {
        // is_code_point_boundary checks that the index is in [0, .len()]
        if is_code_point_boundary(self, range.start) {
            unsafe { slice_unchecked(self, range.start, self.len()) }
        } else {
            slice_error_fail(self, range.start, self.len())
        }
    }
}

/// Returns a slice of the given string from its beginning to byte `end`.
///
/// # Panics
///
/// Panics when `end` is not at a code point boundary,
/// or is beyond the end of the string.
impl ops::Index<ops::RangeTo<usize>> for Wtf8Str {
    type Output = Wtf8Str;

    #[inline]
    fn index(&self, range: ops::RangeTo<usize>) -> &Wtf8Str {
        // is_code_point_boundary checks that the index is in [0, .len()]
        if is_code_point_boundary(self, range.end) {
            unsafe { slice_unchecked(self, 0, range.end) }
        } else {
            slice_error_fail(self, 0, range.end)
        }
    }
}

impl ops::Index<ops::RangeFull> for Wtf8Str {
    type Output = Wtf8Str;

    #[inline]
    fn index(&self, _range: ops::RangeFull) -> &Wtf8Str {
        self
    }
}

#[inline]
fn decode_surrogate(second_byte: u8, third_byte: u8) -> u16 {
    // The first byte is assumed to be 0xED
    0xD800 | (second_byte as u16 & 0x3F) << 6 | third_byte as u16 & 0x3F
}

#[inline]
fn decode_surrogate_pair(lead: u16, trail: u16) -> char {
    let code_point = 0x10000 + ((((lead - 0xD800) as u32) << 10) | (trail - 0xDC00) as u32);
    unsafe { char::from_u32_unchecked(code_point) }
}

/// Copied from core::str::StrPrelude::is_char_boundary
#[inline]
fn is_code_point_boundary(slice: &Wtf8Str, index: usize) -> bool {
    if index == slice.len() {
        return true;
    }
    match slice.bytes.get(index) {
        None => false,
        Some(&b) => !(128..192).contains(&b),
    }
}

/// Copied from core::str::raw::slice_unchecked
#[inline]
unsafe fn slice_unchecked(s: &Wtf8Str, begin: usize, end: usize) -> &Wtf8Str {
    // SAFETY: memory layout of a &[u8] and &Wtf8 are the same, as denoted by
    // `#[repr(transparent)]`.
    unsafe {
        Wtf8Str::from_bytes_unchecked(slice::from_raw_parts(
            s.bytes.as_ptr().add(begin),
            end - begin,
        ))
    }
}

/// Copied from core::str::raw::slice_error_fail
#[inline(never)]
fn slice_error_fail(s: &Wtf8Str, begin: usize, end: usize) -> ! {
    assert!(begin <= end);
    panic!("index {begin} and/or {end} in `{s:?}` do not lie on character boundary");
}

/// Iterator for the code points of a WTF-8 string.
///
/// Created with the method `.code_points()`.
#[derive(Clone)]
pub struct Wtf8CodePoints<'a> {
    bytes: slice::Iter<'a, u8>,
}

impl<'a> Iterator for Wtf8CodePoints<'a> {
    type Item = Codepoint;

    #[inline]
    fn next(&mut self) -> Option<Codepoint> {
        // SAFETY: `self.bytes` has been created from a WTF-8 string
        unsafe { next_code_point(&mut self.bytes).map(|c| Codepoint { value: c }) }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.bytes.len();
        (len.saturating_add(3) / 4, Some(len))
    }
}

/// Generates a wide character sequence for potentially ill-formed UTF-16.
#[derive(Clone)]
pub struct EncodeWide<'a> {
    code_points: Wtf8CodePoints<'a>,
    extra: u16,
}

// Copied from libunicode/u_str.rs
impl<'a> Iterator for EncodeWide<'a> {
    type Item = u16;

    #[inline]
    fn next(&mut self) -> Option<u16> {
        if self.extra != 0 {
            let tmp = self.extra;
            self.extra = 0;
            return Some(tmp);
        }

        let mut buf = [0; 2];
        self.code_points.next().map(|code_point| {
            let n = std_internal::encode_utf16_raw(code_point.value, &mut buf).len();
            if n == 2 {
                self.extra = buf[1];
            }
            buf[0]
        })
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let (low, high) = self.code_points.size_hint();
        let ext = (self.extra != 0) as usize;
        // every code point gets either one u16 or two u16,
        // so this iterator is between 1 or 2 times as
        // long as the underlying iterator.
        (
            low + ext,
            high.and_then(|n| n.checked_mul(2))
                .and_then(|n| n.checked_add(ext)),
        )
    }
}

impl FusedIterator for EncodeWide<'_> {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Wtf8Error {
    valid_up_to: usize,
    error_len: Option<NonZeroU8>,
}

impl Wtf8Error {
    /// Returns the index in the given string up to which valid WTF-8 was
    /// verified.
    ///
    /// It is the maximum index such that `from_wtf8(&input[..index])`
    /// would return `Ok(_)`.
    #[inline]
    pub const fn valid_up_to(&self) -> usize {
        self.valid_up_to
    }

    /// Provides more information about the failure:
    ///
    /// * `None`: the end of the input was reached unexpectedly.
    ///   `self.valid_up_to()` is 1 to 3 bytes from the end of the input.
    ///   If a byte stream (such as a file or a network socket) is being decoded incrementally,
    ///   this could be a valid `Codepoint` whose WTF-8 byte sequence is spanning multiple chunks.
    ///
    /// * `Some(len)`: an unexpected byte was encountered.
    ///   The length provided is that of the invalid byte sequence
    ///   that starts at the index given by `valid_up_to()`.
    ///   Decoding should resume after that sequence
    ///   (after inserting a [`U+FFFD REPLACEMENT CHARACTER`][U+FFFD]) in case of
    ///   lossy decoding.
    ///
    /// [U+FFFD]: ../../std/char/constant.REPLACEMENT_CHARACTER.html
    #[inline]
    pub const fn error_len(&self) -> Option<NonZeroU8> {
        // This should become `map` again, once it's `const`
        match self.error_len {
            Some(len) => Some(len),
            None => None,
        }
    }
}

impl fmt::Display for Wtf8Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Wtf8Error { valid_up_to, error_len: Some(error_len) } => {
                write!(f, "invalid wtf-8 sequence of {error_len} bytes from index {valid_up_to}")
            }
            Wtf8Error { valid_up_to, error_len: None } => {
                write!(f, "incomplete wtf-8 byte sequence from index {valid_up_to}")
            }
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for Wtf8Error {}

pub fn validate_wtf8(bytes: &[u8]) -> Result<(), Wtf8Error> {
    // based on https://datatracker.ietf.org/doc/html/rfc3629#page-4 with wtf-8 modifications
    let mut iter = bytes.iter().copied().enumerate();
    let tail = 0x80..=0xBF;
    let mut is_high_surrogate = false;
    while let Some((index, b)) = iter.next() {
        macro_rules! err {
            () => {
                return Err(Wtf8Error {
                    valid_up_to: index,
                    error_len: None,
                })
            };
            ($e: expr) => {
                return Err(Wtf8Error {
                    valid_up_to: index,
                    error_len: NonZeroU8::new($e),
                })
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
            0x00..=0x7F => {}
            0xC2..=0xDF => {
                if !tail.contains(&next!()) {
                    err!(2);
                }
            }
            0xE0 => {
                if !(0xA0..=0xBF).contains(&next!()) || !tail.contains(&next!()) {
                    err!(3);
                }
            }
            0xE1..=0xEC | 0xEE..=0xEF => {
                if !tail.contains(&next!()) || !tail.contains(&next!()) {
                    err!(3);
                }
            }
            0xED => {
                let n = next!();
                match n {
                    0x80..=0x9f => {}
                    0xA0..=0xAF => {
                        is_high_surrogate = true;
                        continue;
                    }
                    0xB0..=0xBF => {
                        // Encoded surrogate pairs are invalid.
                        if is_high_surrogate {
                            return Err(Wtf8Error {
                                valid_up_to: index - 3,
                                error_len: NonZeroU8::new(6),
                            });
                        }
                    }
                    _ => err!(3),
                }
                if !tail.contains(&next!()) {
                    err!(3);
                }
            }
            _ => err!(1),
        }
        is_high_surrogate = false;
    }
    Ok(())
}

#[allow(clippy::derive_hash_xor_eq)]
impl Hash for Codepoint {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state)
    }
}

#[allow(clippy::derive_hash_xor_eq)]
impl Hash for Wtf8Str {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write(&self.bytes);
        0xfeu8.hash(state)
    }
}
