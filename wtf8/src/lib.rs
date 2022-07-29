#![cfg_attr(all(not(test), not(feature = "std")), no_std)]
#![deny(unsafe_op_in_unsafe_fn, clippy::undocumented_unsafe_blocks)]

use core::fmt;
use core::hash::Hash;
use core::num::NonZeroU8;
use core::str::Utf8Error;

#[cfg(feature = "alloc")]
extern crate alloc;

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct Wtf8Str {
    bytes: [u8],
}

impl Wtf8Str {
    /// Convert a slice of bytes to wobbly utf-8.
    /// 
    /// # Safety
    ///
    /// The bytes must be a valid encoded utf-8 sequence with an exception of allowing unpaired
    /// surrougate values in the encoding.
    pub const unsafe fn from_wtf8_unchecked(bytes: &[u8]) -> &Wtf8Str {
        // SAFETY: callers must ensure that the bytes is a valid wtf-8 sequence.
        // Since Wtf8Str is `#[repr(transparent)]` over `[u8]`, this is a safe cast.
        unsafe {
            &*(bytes as *const [u8] as *const Wtf8Str)
        }
    }

    /// Convert a slice of bytes to wobbly utf-8.
    /// 
    /// # Safety
    /// 
    /// The bytes must be a valid encoded utf-8 sequence with an exception of allowing unpaired
    /// surrougate values in the encoding.
    pub unsafe fn from_wtf8_unchecked_mut(bytes: &mut [u8]) -> &mut Wtf8Str {
        // SAFETY: callers must ensure that the bytes is a valid wtf-8 sequence.
        // Since Wtf8Str is `#[repr(transparent)]` over `[u8]`, this is a safe cast.
        unsafe {
            &mut *(bytes as *mut [u8] as *mut Wtf8Str)
        }
    }

    pub const fn as_bytes(&self) -> &[u8] {
        &self.bytes
    }

    /// Returns a mutable reference to the underlying bytes.
    ///
    /// # Safety
    ///
    /// Transforming the bytes to an invalid wtf-8 sequence is insta-UB if the `Wtf8Str` is still
    /// borrowed. 
    pub unsafe fn as_bytes_mut(&mut self) -> &mut [u8] {
        &mut self.bytes
    }

    /// Creates a `Wtf8Str` from a `str`.
    pub const fn new(s: &str) -> &Wtf8Str {
        // SAFETY: wtf-8 is a superset of UTF-8, and a `str` is valid UTF-8,
        // so it must be valid WTF-8 as well.
        unsafe { Wtf8Str::from_wtf8_unchecked(s.as_bytes()) }
    }

    pub fn as_str(&self) -> Result<&str, Utf8Error> {
        core::str::from_utf8(&self.bytes)
    }

    pub fn as_mut_str(&mut self) -> Result<&mut str, Utf8Error> {
        core::str::from_utf8_mut(&mut self.bytes)
    }

    /// Like [`as_str_mut`], but replaces unpaired surrougate codepoints with the replacement
    /// character instead of erroring. This operation is infallible and in place.
    pub fn as_mut_str_lossy(&mut self) -> &mut str {
        replace_unpaired_surrougates_with_replacement_characters(&mut self.bytes);
        debug_assert!(core::str::from_utf8(&self.bytes).is_ok());
        // SAFETY: we replaced unpaired surrougate codepoints with valid utf8.
        unsafe { core::str::from_utf8_unchecked_mut(&mut self.bytes) }
    }
}

#[cfg(feature = "alloc")]
mod string {
    use alloc::vec::Vec;
    use alloc::string::String;

    use crate::replace_unpaired_surrougates_with_replacement_characters;

    #[repr(transparent)]
    pub struct Wtf8String {
        bytes: Vec<u8>,
    }
    
    impl Wtf8String {
        pub const fn new() -> Wtf8String {
            Self { bytes: Vec::new() }
        }
        /// Construct a `Wtf8String` from a vector of bytes.
        ///
        /// # Safety
        /// The bytes provided must be valid WTF-8.
        pub unsafe fn from_wtf8_unchecked(bytes: Vec<u8>) -> Wtf8String {
            Self { bytes }
        }
        pub fn to_string_lossy(mut self) -> String {
            replace_unpaired_surrougates_with_replacement_characters(&mut self.bytes);
            debug_assert!(core::str::from_utf8(&self.bytes).is_ok());
            // SAFETY: we replaced unpaired surrougate codepoints with valid utf8.
            unsafe { String::from_utf8_unchecked(self.bytes) }
        }
        pub fn into_inner(self) -> Vec<u8> {
            self.bytes
        }
    }
}

#[cfg(feature = "alloc")]
pub use string::Wtf8String;

fn replace_unpaired_surrougates_with_replacement_characters(bytes: &mut [u8]) {
    let mut bytes = bytes.iter_mut();
    while let Some(c1) = bytes.next() {
        if *c1 != 0b1110_1101 {
            continue;
        }
        let (c2, c3) = if let Some(x) = bytes.next().zip(bytes.next()) {
            x
        } else {
            break;
        };

        if *c2 & 0b1111_0000 != 0b1010_0000 {
            continue;
        }
        *c1 = 0xEF;
        *c2 = 0xBF;
        *c3 = 0xBD; // replacement character
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub struct Wtf8Error {
    valid_up_to: usize,
    error_len: Option<NonZeroU8>,
}

impl fmt::Display for Wtf8Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(error_len) = self.error_len {
            write!(
                f,
                "invalid wtf-8 sequence of {} bytes from index {}",
                error_len, self.valid_up_to
            )
        } else {
            write!(f, "incomplete wtf-8 byte sequence from index {}", self.valid_up_to)
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for Wtf8Error {}

fn run_wtf8_validation(v: &[u8]) -> Result<(), Wtf8Error> {
    let mut iter = v.iter().enumerate();

    // https://simonsapin.github.io/wtf-8/#decoding-wtf-8
    //    UTF8-octets = *( UTF8-char )
    //    UTF8-char   = UTF8-1 / UTF8-2 / UTF8-3 / UTF8-4
    //    UTF8-1      = %x00-7F
    //    UTF8-2      = %xC2-DF UTF8-tail
    while let Some((mut index, c)) = iter.next() {
        macro_rules! err {
            () => (return Err(Wtf8Error { valid_up_to: index, error_len: None }));
            ($error_len: expr) => (return Err(Wtf8Error { valid_up_to: index, error_len: NonZeroU8::new($error_len) }));
        }
        match c {
            0x00..=0x7F => continue,
            _ => err!(),
        }
    }

    Ok(())
}

impl<'a> From<&'a str> for &'a Wtf8Str {
    #[inline]
    fn from(s: &'a str) -> Self {
        Wtf8Str::new(s)
    }
}


