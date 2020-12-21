use std::convert::TryInto;
use std::result::Result as StdResult;

use chrono::NaiveDateTime;
use log::debug;
use snafu::{ResultExt, Snafu};

use crate::percent_path::PercentPath;
use crate::trash_info::TrashInfo;

pub const TRASH_DATETIME_FORMAT: &str = "%Y-%m-%dT%H:%M:%S";

type ParseResult<I, O, E = ParseError> = StdResult<(I, O), E>;
type Result<T, E = Error> = StdResult<T, E>;

#[derive(Debug, Snafu)]
pub enum Error {
    Parse {
        input: String,
        source: ParseError,
    },
    Chrono {
        source: chrono::format::ParseError,
        date: String,
    },
}

#[derive(Debug, Snafu, PartialEq)]
pub enum ParseError {
    Tag { tag: String },
    TagEmpty,
    Char { c: String },
    Eof,
}

/// Will panic if the tag is empty
fn tag(tag: &str) -> impl Fn(&str) -> ParseResult<&str, &str> + '_ {
    move |i| {
        if i.is_empty() {
            Ok(("", ""))
        } else if tag.is_empty() {
            Ok((i, ""))
        } else if i.starts_with(tag) {
            let idx = tag.len();
            Ok((&i[idx..], &i[..idx]))
        } else {
            Tag { tag }.fail()
        }
    }
}

fn char(c: char) -> impl Fn(&str) -> ParseResult<&str, char> {
    move |i| {
        if i.starts_with(c) {
            Ok((&i[1..], c))
        } else {
            Char { c }.fail()
        }
    }
}

fn is_not(not: char) -> impl Fn(&str) -> ParseResult<&str, &str> {
    move |i| {
        let end = i
            .char_indices()
            .find_map(|(idx, c)| if c == not { Some(idx) } else { None })
            .unwrap_or_else(|| i.len());

        Ok((&i[end..], &i[..end]))
    }
}

fn all_consuming<F>(f: F) -> impl Fn(&str) -> ParseResult<&str, &str>
where
    F: Fn(&str) -> ParseResult<&str, &str>,
{
    move |i| {
        let (i, o) = f(i)?;
        if i.is_empty() {
            Ok((i, o))
        } else {
            Eof.fail()
        }
    }
}

fn parse_header(i: &str) -> ParseResult<&str, &str> {
    tag("[Trash Info]")(i)
}

fn parse_deletion_date(i: &str) -> ParseResult<&str, &str> {
    let (i, _) = tag("DeletionDate")(i)?;
    let (i, _) = char('=')(i)?;
    let (i, o) = is_not('\n')(i)?;
    Ok((i, o))
}

fn parse_path(i: &str) -> ParseResult<&str, &str> {
    let (i, _) = tag("Path")(i)?;
    let (i, _) = char('=')(i)?;
    let (i, o) = is_not('\n')(i)?;
    Ok((i, o))
}

#[derive(Debug, Clone, PartialEq)]
struct TrashInfoStr<'a, 'b> {
    path: &'a str,
    deletion_date: &'b str,
}

impl<'a, 'b> TryInto<TrashInfo> for TrashInfoStr<'a, 'b> {
    type Error = Error;

    fn try_into(self: TrashInfoStr<'a, 'b>) -> Result<TrashInfo> {
        let percent_path = PercentPath::new(self.path);
        let deletion_date =
            NaiveDateTime::parse_from_str(&self.deletion_date, TRASH_DATETIME_FORMAT).context(
                Chrono {
                    date: self.deletion_date,
                },
            )?;

        Ok(TrashInfo::new(percent_path, Some(deletion_date)))
    }
}

fn parse_trash_info_str(i: &str) -> Result<TrashInfoStr, ParseError> {
    let (i, _) = parse_header(i)?;
    let (i, _) = char('\n')(i)?;
    let (i, path) = parse_path(i)?;
    let (i, _) = char('\n')(i)?;
    let (_, deletion_date) = all_consuming(parse_deletion_date)(i)?;
    Ok(TrashInfoStr {
        path,
        deletion_date,
    })
}

pub fn parse_trash_info(s: &str) -> Result<TrashInfo> {
    let trash_info_str = parse_trash_info_str(s).context(Parse { input: s })?;
    debug!("Trash info str part: {:?}", trash_info_str);
    let trash_info = trash_info_str.try_into()?;
    debug!("Trash info with proper types: {:?}", trash_info);
    Ok(trash_info)
}

#[cfg(test)]
mod tests {
    use super::*;
    use eyre::Result;
    use proptest::prelude::*;
    use std::str::FromStr;

    proptest! {
        #[test]
        fn doesnt_crash(s in "\\PC*") {
            let _ = s.parse::<TrashInfo>();
        }
    }

    proptest! {
        #[test]
        fn tag_same(s in "\\PC*") {
            assert_eq!(tag(&s)(&s), Ok(("", s.as_str())));
        }
    }

    proptest! {
        #[test]
        fn not_at_start(s1 in "\\PC*", s2 in "\\PC*") {
            prop_assume!(!s1.is_empty() && !s2.is_empty());

            let compound = format!("{}{}", s1, s2);
            prop_assert_eq!(tag(&s2)(&compound), Tag { tag: s2 }.fail());
        }
    }

    proptest! {
        #[test]
        fn at_start(s1 in "\\PC*", s2 in "\\PC*") {
            prop_assume!(!s1.is_empty() && !s2.is_empty());

            let compound = format!("{}{}", s1, s2);
            prop_assert_eq!(tag(&s1)(&compound), Ok((s2.as_str(), s1.as_str())));
        }
    }

    proptest! {
        #[test]
        fn empty_tag(s in "\\PC*") {
            assert_eq!(tag("")(&s), Ok((s.as_str(), "")));
        }
    }

    proptest! {
        #[test]
        fn empty_input(t in "\\PC*") {
            assert_eq!(tag(&t)(""), Ok(("", "")));
        }
    }

    /// Only returns chrono result because if parsing with nom has failed this will return an error
    /// message and panic instead of returning a result.
    fn test_parse_trash_info<'a>(trash_info_str: &'a str, actual: (&str, &str)) -> Result<()> {
        let expected = match TrashInfo::from_str(trash_info_str) {
            Err(e) => {
                eprintln!("{}", e);
                panic!("An error occurred");
            }
            Ok(trash_info) => trash_info,
        };

        let actual = TrashInfo::new(
            PercentPath::from_str(actual.0),
            Some(NaiveDateTime::parse_from_str(
                actual.1,
                TRASH_DATETIME_FORMAT,
            )?),
        );

        assert_eq!(expected, actual);

        Ok(())
    }

    fn test_parse_trash_info_run(trash_info_str: &str, actual: (&str, &str)) {
        match test_parse_trash_info(trash_info_str, actual) {
            Ok(()) => (),
            Err(e) => {
                eprintln!("{}", e);
                panic!("An error occurred when testing parse_trash_info");
            }
        }
    }

    #[test]
    fn tag_test() -> Result<()> {
        let s = "let a=1";
        assert_eq!(tag("let")(s), Ok((" a=1", "let")));
        Ok(())
    }

    #[test]
    fn tag2_test() {
        assert_eq!(tag("let")("leta=1"), Ok(("a=1", "let")));
    }

    #[test]
    fn tag_none_test() {
        assert_eq!(tag("let")("a=1"), Err(Tag { tag: "let" }.build()));
    }

    #[test]
    fn tag_end_test() {
        assert_eq!(tag("end")("thisistheend"), Err(Tag { tag: "end" }.build()));
    }

    #[test]
    fn char_test() {
        assert_eq!(char('=')("=hello"), Ok(("hello", '=')));
    }

    #[test]
    fn char_none_test() {
        assert_eq!(char('=')("a=b"), Err(Char { c: '=' }.build()));
    }

    #[test]
    fn is_not_test() {
        assert_eq!(is_not('=')("var=b"), Ok(("=b", "var")));
    }

    #[test]
    fn is_not_all_test() {
        assert_eq!(is_not('=')("variable"), Ok(("", "variable")));
    }

    #[test]
    fn all_consuming_test() {
        assert_eq!(all_consuming(tag("="))("=b"), Err(Eof.build()));
    }

    #[test]
    fn all_consuming2_test() {
        assert_eq!(
            all_consuming(is_not('\n'))("hello\nperson"),
            Err(Eof.build())
        );
    }
    #[test]

    fn parse_header_line_test() {
        assert_eq!(parse_header("[Trash Info]"), Ok(("", "[Trash Info]")));
    }

    #[test]
    fn tag_whitespace_test() {
        assert_eq!(tag("Trash Info")("Trash Info "), Ok((" ", "Trash Info")));
    }

    #[test]
    fn value_path_test() {
        assert_eq!(
            parse_path("Path=/home/brian/.config"),
            Ok(("", "/home/brian/.config"))
        );
    }

    #[test]
    fn value_path_whitespace_test() {
        assert_eq!(
            parse_path("Path= /home/brian/.config"),
            Ok(("", " /home/brian/.config"))
        );
    }

    #[ignore]
    #[test]
    fn trash_info_parse_test() {
        test_parse_trash_info_run(
            "[Trash Info]\nPath=/home/brian/dude.txt\nDeletionDate=2020-08-28T16:16:55",
            ("/home/brian/dude.txt", "2020-08-28T16:16:55"),
        )
    }
}
