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
    /// I/O error. This indicates the underlying `Read`/`Write` instance has raised an error.
    #[error(transparent)]
    IO(#[from] std::io::Error),

    /// Error when it encountered invalid data. This can be an invalid tag for an enum (which is raised from the derive macro).
    #[error("Invalid {0}: {1}")]
    Invalid(&'static str, Cow<'static, str>),

    /// Error raised from the [`mod_utf8`] module.
    ///
    /// [`mod_utf8`]: crate::mod_utf8
    #[error(transparent)]
    MUTF(#[from] crate::mod_utf8::MUTFError),

    /// This error indicates that when reading the attribute, the data didn't conform to the set of the fields,
    /// therefore resulting in parts of the data not transformed to actual information.
    ///
    /// This error is raised from the derive macro.
    #[error("Attribute length mismatch: actual length ({0} bytes) is greater than length consumed ({1} bytes)")]
    AttributeLength(u32, u32),

    /// A custom error type.
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

    /// The error type, but contains a backtrace. Useful when debugging.
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
                #[inline]
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
            Custom(b: Box<dyn std::error::Error>)
        );
    }
}

/// The error type.
///
/// When the `backtrace` feature is enabled, `ErrorTrace` is used instead of `ErrorBase`.
#[cfg(any(feature = "backtrace", test))]
pub type Error = backtrace::ErrorTrace;

/// The error type.
///
/// When the `backtrace` feature is enabled, `ErrorTrace` is used instead of `ErrorBase`.
#[cfg(not(any(feature = "backtrace", test)))]
pub type Error = ErrorBase;

/// The Result type. The default error type is [`crate::Error`].
///
/// [`crate::Error`]: crate::Error
pub type Result<T, E = Error> = std::result::Result<T, E>;