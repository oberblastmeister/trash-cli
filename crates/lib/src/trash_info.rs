use std::cmp::Ordering;
use std::fmt;
use std::fs;
use std::fs::OpenOptions;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::str::FromStr;

// use chrono::{Local, NaiveDateTime};
use crate::utils;
use chrono::prelude::*;
use fs::File;
use log::debug;
use once_cell::sync::Lazy;
use snafu::{ResultExt, Snafu};

use super::parser::{self, parse_trash_info, TRASH_DATETIME_FORMAT};
use crate::percent_path::PercentPath;
use crate::utils::{add_trash_info_ext, to_directory};
use crate::{TRASH_INFO_DIR, TRASH_INFO_EXT};

static OPEN_OPTIONS: Lazy<OpenOptions> = Lazy::new(|| {
    let mut open_options = OpenOptions::new();
    open_options
        .read(false) // read access false
        .write(true) // write access true
        .append(false) // do not append to file
        .truncate(true) // truncate the file if it exists
        .create(true) // create a new file or open existing
        .create_new(false); // don't fail if there is an existing file
    open_options
});

#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("Failed to open file with path {}", path.display()))]
    FileOpen { source: io::Error, path: PathBuf },

    #[snafu(display("Failed to write to trash info file"))]
    TrashInfoWrite { source: io::Error },

    #[snafu(display("Failed to read path {} to a string", path.display()))]
    ReadToStr { path: PathBuf, source: io::Error },

    #[snafu(display("Failed to parse trash info file from str"))]
    ParseTrashInfoFromStr { source: parser::Error },

    #[snafu(display("Wrong extension for path {}", path.display()))]
    WrongExtension { path: PathBuf },

    #[snafu(display("The path {} does not exist", path.display()))]
    NonExistentPath { path: PathBuf },

    #[snafu(display("Failed to get a new path in dir `{}` from path `{}`", dir.display(), path.display()))]
    ToDirectory {
        path: PathBuf,
        dir: PathBuf,
        source: utils::Error,
    },
}

type Result<T, E = Error> = ::std::result::Result<T, E>;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TrashInfo {
    percent_path: PercentPath,
    deletion_date: NaiveDateTime,
}

impl TrashInfo {
    pub(super) fn new(percent_path: PercentPath, deletion_date: Option<NaiveDateTime>) -> Self {
        let deletion_date = deletion_date.unwrap_or_else(|| Local::now().naive_local());

        TrashInfo {
            percent_path,
            deletion_date,
        }
    }

    /// saves the name with the extension .trashinfo
    pub(super) fn save(self, name: impl AsRef<Path>) -> Result<()> {
        let path = get_trash_info_path(name)?;

        let mut trash_info_file = OPEN_OPTIONS.open(&path).context(FileOpen { path })?;
        save_trash_info(&mut trash_info_file, self)?;
        Ok(())
    }

    pub(crate) fn parse_from_path(path: impl AsRef<Path>) -> Result<Self> {
        let path = path.as_ref();
        validate_path(path)?;
        let contents = fs::read_to_string(path).context(ReadToStr { path })?;
        debug!(
            "Contens of trash info file {}:\n {}",
            path.display(),
            contents
        );
        let trimmed = contents.trim_end_matches('\n');

        let trash_info = trimmed.parse::<TrashInfo>()?;
        Ok(trash_info)
    }

    /// Returns the path as a percent encoded string
    pub fn percent_path(&self) -> &PercentPath {
        &self.percent_path
    }

    /// Gets the deletion date
    pub fn deletion_date(&self) -> NaiveDateTime {
        self.deletion_date
    }

    /// Gets the deletions date as a string formated using the trash_info_format
    pub fn deletion_date_string_format(&self) -> String {
        format(self.deletion_date)
    }
}

fn format(date: NaiveDateTime) -> String {
    format!("{}", date.format(TRASH_DATETIME_FORMAT))
}

impl FromStr for TrashInfo {
    type Err = Error;

    fn from_str(s: &str) -> Result<TrashInfo> {
        let trash_info = parse_trash_info(s).context(ParseTrashInfoFromStr)?;
        Ok(trash_info)
    }
}

impl fmt::Display for TrashInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[Trash Info]\nPath={}\nDeletionDate={}",
            self.percent_path,
            self.deletion_date_string_format(),
        )
    }
}

impl Ord for TrashInfo {
    fn cmp(&self, other: &Self) -> Ordering {
        self.deletion_date.cmp(&other.deletion_date)
    }
}

impl PartialOrd for TrashInfo {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn get_trash_info_path(name: impl AsRef<Path>) -> Result<PathBuf> {
    let path = to_directory(&name, &*TRASH_INFO_DIR).with_context(|| ToDirectory {
        path: name.as_ref(),
        dir: &*TRASH_INFO_DIR,
    })?;
    Ok(add_trash_info_ext(path))
}

fn save_trash_info(file: &mut File, trash_info: TrashInfo) -> Result<()> {
    file.write_all(trash_info.to_string().as_bytes())
        .context(TrashInfoWrite)?;

    Ok(())
}

/// Checks if the extension is correct or no extension
fn check_extension(path: impl AsRef<Path>) -> bool {
    let path = path.as_ref();
    matches!(path.extension(), Some(ext) if ext == TRASH_INFO_EXT)
}

fn validate_path(path: impl AsRef<Path>) -> Result<()> {
    let path = path.as_ref();
    if !check_extension(path) {
        WrongExtension { path }.fail()
    } else if !path.exists() {
        NonExistentPath { path }.fail()
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::HOME_DIR;
    use eyre::Result;
    use std::io::{Read, Seek, SeekFrom, Write};
    use tempfile::{tempfile_in, Builder, NamedTempFile};

    #[test]
    fn get_trash_info_path_test() -> Result<()> {
        assert_eq!(
            get_trash_info_path("this_is_a_name")?,
            HOME_DIR.join(".local/share/Trash/info/this_is_a_name.trashinfo")
        );
        Ok(())
    }

    #[test]
    fn get_trash_info_path_already_extnesion_test() -> Result<()> {
        assert_eq!(
            get_trash_info_path("already_extension.trashinfo")?,
            HOME_DIR.join(".local/share/Trash/info/already_extension.trashinfo.trashinfo")
        );
        Ok(())
    }

    #[test]
    fn trash_format_test() {
        let time = Local
            .ymd(2014, 7, 8)
            .and_hms_milli(9, 10, 11, 12)
            .naive_local();
        let s = format(time);
        assert_eq!(s, "2014-07-08T09:10:11");
    }

    #[test]
    fn trash_info_display_test() {
        let time = Local
            .ymd(2020, 4, 9)
            .and_hms_nano(9, 11, 10, 12_000_000)
            .naive_local();
        let percent_path = PercentPath::from_str("/a/directory");
        let trash_info = TrashInfo::new(percent_path.clone(), Some(time));
        assert_eq!(
            trash_info.to_string(),
            format!(
                "[Trash Info]\nPath={}\nDeletionDate={}",
                percent_path,
                format(time)
            ),
        );
    }

    #[ignore]
    #[test]
    fn save_trash_info_test_test() -> Result<()> {
        let trash_info = TrashInfo::new(PercentPath::from_str("this/is/a/path"), None);

        let mut temp_trash_info_file = tempfile_in(&*TRASH_INFO_DIR)?;

        save_trash_info(&mut temp_trash_info_file, trash_info.clone())?;
        temp_trash_info_file.seek(SeekFrom::Start(0))?;

        let mut contents = String::new();
        temp_trash_info_file.read_to_string(&mut contents)?;

        assert_eq!(trash_info.to_string(), contents);

        Ok(())
    }

    #[test]
    fn parse_from_path_test() -> Result<()> {
        let s = "[Trash Info]
Path=/home/brian/.stardict
DeletionDate=2020-09-21T08:34:36";
        let mut temp = Builder::new()
            .prefix("parse_from_path_test")
            .suffix(".trashinfo")
            .tempfile()?;
        temp.write_all(s.as_bytes())?;
        let temp_path = temp.path();
        assert_eq!(TrashInfo::parse_from_path(temp_path)?.to_string(), s);
        Ok(())
    }

    #[ignore]
    #[test]
    fn read_to_string_test() -> Result<()> {
        assert_eq!(
            "\
[Trash Info]
Path=/home/brian/projects/trash/crates/cli
DeletionDate=2020-09-21T10:40:17",
            std::fs::read_to_string("/home/brian/.local/share/Trash/info/cli.trashinfo")?
        );
        Ok(())
    }
}
