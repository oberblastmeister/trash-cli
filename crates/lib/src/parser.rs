use std::convert::TryInto;
use std::result::Result as StdResult;
use std::str::FromStr;

use chrono::NaiveDateTime;
use percent_encoding::percent_decode_str;
use snafu::{ResultExt, Snafu};

use crate::percent_path::{self, PercentPath};
use crate::trash_info::TrashInfo;

pub const TRASH_DATETIME_FORMAT: &'static str = "%Y-%m-%dT%H:%M:%S";

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

#[derive(Debug, Snafu)]
pub enum ParseError {
    Tag { tag: String },
    Char { c: String },
    Eof,
}

fn tag(tag: &str) -> impl Fn(&str) -> ParseResult<&str, &str> + '_ {
    move |i| {
        if i.starts_with(tag) {
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
        let mut end = 1;
        for c in i.chars() {
            if c == not {
                break;
            }
            end += 1;
        }
        Ok((&i[end..], &i[..end]))
    }
}

fn all_consuming<F>(f: F) -> impl Fn(&str) -> ParseResult<&str, &str>
where
    F: Fn(&str) -> ParseResult<&str, &str>,
{
    move |i| {
        let (i, o) = f(i)?;
        if i.len() == 0 {
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

struct TrashInfoStr<'a, 'b> {
    path: &'a str,
    deletion_date: &'b str,
}

impl<'a, 'b> TryInto<TrashInfo> for TrashInfoStr<'a, 'b> {
    type Error = Error;

    fn try_into(self: TrashInfoStr<'a, 'b>) -> Result<TrashInfo> {
        let percent_path = PercentPath::from_str(self.path.as_ref());
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
    let (i, deletion_date) = all_consuming(parse_deletion_date)(i)?;
    Ok(TrashInfoStr {
        path,
        deletion_date,
    })
}

pub fn parse_trash_info(s: &str) -> Result<TrashInfo> {
    let trash_info_str = parse_trash_info_str(s).context(Parse { input: s })?;
    let trash_info = trash_info_str.try_into()?;
    Ok(trash_info)
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::{Context, Result};
    use std::str::FromStr;

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
    fn parse_header_line_test() {
        assert_eq!(parse_header_line("[Trash Info]"), Ok(("", "Trash Info")));
    }

    #[test]
    fn tag_whitespace_test() {
        assert_eq!(
            tag::<&str, &str, VerboseError<&str>>("Trash Info")("Trash Info "),
            Ok((" ", "Trash Info"))
        );
    }

    #[test]
    fn value_path_test() {
        assert_eq!(
            parse_path_line("Path=/home/brian/.config"),
            Ok(("", "/home/brian/.config"))
        );
    }

    #[test]
    fn value_path_whitespace_test() {
        assert_eq!(
            parse_path_line("Path= /home/brian/.config"),
            Ok(("", " /home/brian/.config"))
        );
    }

    #[ignore]
    #[test]
    fn trash_info_parse_test() {
        test_parse_trash_info_run(
            "[Trash Info]\nPath=/home/brian/dude.txt\nDeletionDate=2020-08-28T16:16:55",
            ("/home/brian/dude.txt", "2020-09-21T08:34:36"),
        )
    }
}