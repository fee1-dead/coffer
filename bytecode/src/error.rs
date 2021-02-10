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
use thiserror::Error;
use std::borrow::Cow;

/// The base error type.
#[derive(Debug, Error)]
pub enum ErrorBase {
    #[error(transparent)]
    IO(#[from] std::io::Error),

    #[error("Invalid {0}: {1}")]
    Invalid(&'static str, Cow<'static, str>),

    #[error(transparent)]
    MUTF(#[from] crate::mod_utf8::MUTFError),

    #[error("Attribute length mismatch: actual length ({0} bytes) is greater than length consumed ({1} bytes)")]
    AttributeLength(u32, u32),

    #[error("Conversion overflows")]
    ArithmeticOverflow(), // This is not unit variant because it has to look like a function call for `backtrace` to work.

    #[error(transparent)]
    Custom(#[from] Box<dyn std::error::Error>)
}

#[cfg(any(feature = "backtrace", test))]
mod backtrace {
    use std::backtrace::Backtrace;
    use std::borrow::Cow;
    use crate::error::ErrorBase;
    use std::error::Error;
    use std::fmt::Formatter;

    impl<T> From<T> for ErrorTrace where ErrorBase: From<T> {
        #[inline(always)]
        fn from(t: T) -> Self {
            Self {
                inner: t.into(),
                trace: Backtrace::force_capture()
            }
        }
    }

    #[derive(Debug)]
    pub struct ErrorTrace {
        pub inner: ErrorBase,
        trace: Backtrace
    }

    impl std::fmt::Display for ErrorTrace {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}\nBacktrace:\n{}", self.inner, self.trace)
        }
    }

    impl Error for ErrorTrace {
        fn source(&self) -> Option<&(dyn Error + 'static)> {
            self.inner.source()
        }

        fn backtrace(&self) -> Option<&Backtrace> {
            Some(&self.trace)
        }
    }
    macro_rules! functions {
        ($($i: ident($($arg_i: ident: $ty: ty),*)),*) => {
            $(
                #[inline(always)] // Don't want this function to be captured
                #[allow(non_snake_case)]
                pub fn $i($($arg_i: $ty),*) -> ErrorTrace {
                    ErrorTrace {
                        inner: ErrorBase::$i($($arg_i),*),
                        trace: Backtrace::force_capture()
                    }
                }
            )*
        };
    }

    impl ErrorTrace {
        functions!(
            IO(e: std::io::Error),
            Invalid(st: &'static str, cow: Cow<'static, str>),
            MUTF(e: crate::mod_utf8::MUTFError),
            AttributeLength(act: u32, exp: u32),
            ArithmeticOverflow(),
            Custom(b: Box<dyn std::error::Error>)
        );
    }
}

/// The error type.
///
/// When the `backtrace` feature is enabled, `ErrorTrace` is use instead of `ErrorBase`.
#[cfg(any(feature = "backtrace", test))]
pub type Error = backtrace::ErrorTrace;

/// The error type.
///
/// When the `backtrace` feature is enabled, `ErrorTrace` is use instead of `ErrorBase`.
#[cfg(not(any(feature = "backtrace", test)))]
pub type Error = ErrorBase;

pub type Result<T, E = Error> = std::result::Result<T, E>;