use std::cmp::Ordering;
use std::fs;
use std::path::Path;

use chrono::naive::NaiveDateTime;
use eyre::{eyre, Result};
use lscolors::{LsColors, Style};
use once_cell::sync::Lazy;
use prettytable::Cell;
use trash_lib::trash_entry::TrashEntry;
use trash_lib::trash_info::TrashInfo;
use trash_lib::HOME_DIR;

static LS_COLORS: Lazy<LsColors> = Lazy::new(|| LsColors::from_env().unwrap_or_default());

#[derive(Debug, PartialEq, Eq)]
pub struct Pair(pub TrashEntry, pub TrashInfo);

impl Pair {
    pub fn new(trash_entry: TrashEntry) -> Result<Pair> {
        let trash_info = trash_entry.parse_trash_info()?;
        let pair = Pair(trash_entry, trash_info);
        Ok(pair)
    }

    pub fn revert(self) -> TrashEntry {
        self.0
    }
}

impl PartialOrd for Pair {
    fn partial_cmp(&self, other: &Pair) -> Option<Ordering> {
        Some(self.1.cmp(&other.1))
    }
}

impl Ord for Pair {
    fn cmp(&self, other: &Pair) -> Ordering {
        self.1.cmp(&other.1)
    }
}

pub fn get_metadata(trash_entry: &TrashEntry) -> Result<fs::Metadata> {
    let metadata = fs::symlink_metadata(trash_entry.file_path())?;
    Ok(metadata)
}

pub fn sort_iterator<T>(iter: impl Iterator<Item = T>) -> impl Iterator<Item = T>
where
    T: Ord,
{
    let mut v: Vec<_> = iter.collect();
    v.sort_unstable();
    v.into_iter()
}

pub mod date {
    use super::*;

    pub fn format(date: NaiveDateTime) -> Vec<Cell> {
        let month = format!("{}", date.format("%b"));
        let day = format!("{}", date.format("%d"));
        let time = format!("{}", date.format("%H:%M:%S"));
        vec![Cell::new(&month), Cell::new(&day), Cell::new(&time)]
    }

    pub fn format_compact(date: NaiveDateTime) -> Vec<Cell> {
        let mm_dd = format!("{}", date.format("%m/%d"));
        let time = format!("{}", date.format("%H:%M:%S"));
        vec![Cell::new(&mm_dd), Cell::new(&time)]
    }
}

pub mod path {
    use super::*;

    pub fn colorize(path: &str, metadata: &fs::Metadata) -> String {
        let style = LS_COLORS.style_for_path_with_metadata(path, Some(metadata));
        let ansi_style = style.map(Style::to_ansi_term_style).unwrap_or_default();
        format!("{}", ansi_style.paint(path))
    }

    pub fn shorten<'a, T>(path: T) -> Result<String>
    where
        T: AsRef<Path> + 'a,
    {
        let path = path.as_ref();
        let path_str = path.to_str().ok_or_else(|| eyre!("Failed"))?;
        let home_dir = HOME_DIR.to_string_lossy();

        Ok(match path_str.find(&*home_dir) {
            Some(start_idx) if start_idx == 0 => {
                format!("{}{}", "~", &path_str[home_dir.len()..])
            }
            _ => path.to_string_lossy().into_owned(),
        })
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use proptest::prelude::*;
        use std::{borrow::Cow, path::PathBuf};

        proptest! {
            #[test]
            fn shorten_path_doesnt_crash(s in "\\PC*") {
                let _ = path::shorten(s);
            }
        }

        proptest! {
            #[test]
            fn shorten_only_first_home(times in 1..=5) {
                let shortened = {
                    let mut path = PathBuf::new();
                    for i in 1..=times {
                        let seg = &*HOME_DIR;
                        path.push(seg);
                    }
                    path::shorten(&path).unwrap()
                };

                let expected = {
                    let mut path = PathBuf::from("~");
                    for i in 1..=times - 1 {
                        path.push(&*HOME_DIR);
                    }
                    path
                };

                prop_assert_eq!(shortened, expected.into_os_string().into_string().unwrap());
            }
        }

        // #[test]
        // fn shorten_path_test() {
        //     assert_eq!(
        //         path::shorten(&format!("{}/project/brian", HOME_DIR.to_str().unwrap())).unwrap(),
        //         Cow::from("~/project/brian")
        //     );
        // }

        #[test]
        fn short_path_not_beginning_test() {
            assert_eq!(
                path::shorten(&format!(
                    "{}/project/{}/code",
                    HOME_DIR.to_str().unwrap(),
                    HOME_DIR.to_str().unwrap()
                ))
                .unwrap(),
                format!("~/project/{}/code", HOME_DIR.to_str().unwrap())
            );
        }

        #[test]
        fn shorten_path_none_test() {
            let path = &format!("projects/{}/code", HOME_DIR.to_str().unwrap());
            assert_eq!(path::shorten(path).unwrap(), Cow::from(path));
        }
    }
}
