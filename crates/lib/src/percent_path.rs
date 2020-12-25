use std::borrow::Cow;
use std::fmt;
use std::path::Path;
use std::str::Utf8Error;

use crate::utils::{self, convert_to_str};
use percent_encoding::{percent_decode_str, utf8_percent_encode, AsciiSet, CONTROLS};
use snafu::{ResultExt, Snafu};

macro_rules! ascii_set {
    ($($byte:expr),+) => {
        const ASCII_SET_SLICE: &'static [u8] = &[$($byte),+];

        pub const ASCII_SET: &AsciiSet = &CONTROLS$(
            .add($byte)
        )*;
    };
}

ascii_set! {
    b' ',
    b'<',
    b'>',
    b'#',
    b'%',
    b'"',
    b'{',
    b'}',
    b'|',
    b'\\',
    b'^',
    b'[',
    b']',
    b'`'
}

#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("The decoded form of the string `{}` was not well formed in utf-8", s,))]
    Decode { s: String, source: Utf8Error },

    #[snafu(display("Failed to convert path to a string to be able to percent encode it"))]
    ConvertPathDecode { source: utils::Error },
}

type Result<T, E = Error> = ::std::result::Result<T, E>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PercentPath(String);

impl PercentPath {
    /// Create a new percent path from a str. Will encode all characters from the string.
    #[allow(clippy::should_implement_trait)]
    pub fn from_str(s: &str) -> Self {
        Self(utf8_percent_encode(s, ASCII_SET).to_string())
    }

    /// Use only if the string is already percent encoded because does not check if the string is
    /// percent encoded. Use inside of the parser because the
    /// trash info file should have already encoded the path.
    pub(crate) fn new_unchecked(s: &str) -> Self {
        Self(s.to_string())
    }

    pub(crate) fn from_path(path: impl AsRef<Path>) -> Result<Self> {
        let path = path.as_ref();
        let s = convert_to_str(path).context(ConvertPathDecode)?;
        Ok(Self(utf8_percent_encode(s, ASCII_SET).to_string()))
    }

    pub fn encoded(&self) -> &str {
        &self.0
    }

    pub fn decoded(&self) -> Result<Cow<'_, str>> {
        percent_decode_str(&self.0)
            .decode_utf8()
            .context(Decode { s: &self.0 })
    }
}

impl fmt::Display for PercentPath {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    fn contains_ascii_set(s: &str) -> bool {
        ASCII_SET_SLICE
            .iter()
            // make sure that % is not there, it will be percent encoded
            .filter(|b| **b as char != '%')
            .any(|&b| s.contains(b as char))
    }

    proptest! {
        #[test]
        fn test_precent_path(s in "\\PC*") {
            let percent_path = PercentPath::from_str(&s);
            prop_assert!(!contains_ascii_set(percent_path.encoded()));
        }
    }
}
