mod parser;
pub mod percent_path;
pub mod trash_entry;
pub mod trash_info;
mod utils;

use std::io;

use fs_extra::dir;
use fs_extra::file;
use log::debug;
use log::error;
use once_cell::sync::Lazy;
use snafu::{ResultExt, Snafu};

#[cfg(feature = "rayon")]
use {
    rayon::prelude::*,
    std::sync::{Arc, Mutex},
};

use std::path::{Path, PathBuf};

use trash_entry::{read_dir_trash_entries, TrashEntry};

macro_rules! dir_error {
    ($dir:expr) => {
        concat!("Fatal error, could not find ", $dir, " directory")
    };
}

pub static HOME_DIR: Lazy<PathBuf> =
    Lazy::new(|| dirs_next::home_dir().expect(dir_error!("home")));

pub(crate) static DATA_DIR: Lazy<PathBuf> = 
    Lazy::new(|| dirs_next::data_dir().expect(dir_error!("data")));

pub static TRASH_DIR: Lazy<PathBuf> = Lazy::new(|| DATA_DIR.join("Trash"));

pub static TRASH_INFO_DIR: Lazy<PathBuf> = Lazy::new(|| TRASH_DIR.join("info"));

pub static TRASH_FILE_DIR: Lazy<PathBuf> = Lazy::new(|| TRASH_DIR.join("files"));

pub const TRASH_INFO_EXT: &'_ str = "trashinfo";

pub const FILE_COPY_OPT: file::CopyOptions = file::CopyOptions {
    overwrite: true,
    skip_exist: false,
    buffer_size: 64000,
};

pub const DIR_COPY_OPT: dir::CopyOptions = dir::CopyOptions {
    overwrite: true,
    skip_exist: false,
    buffer_size: 64000,
    copy_inside: false,
    content_only: false,
    depth: 0,
};

#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("Failed to create new trash entry struct"))]
    TrashEntryNew { source: trash_entry::Error },

    #[snafu(display(
        "Failed to create a new trash entry by moving a file and creating a trash info file"
    ))]
    TrashEntryCreation { source: trash_entry::Error },

    #[snafu(display("Failed to read an iterator of trash entries"))]
    ReadDirTrashEntry { source: trash_entry::Error },

    #[snafu(display("Failed to restore trash entry"))]
    TrashEntryRestore { source: trash_entry::Error },

    #[snafu(display("Failed to remove trash entry"))]
    TrashEntryRemove { source: trash_entry::Error },

    #[snafu(context(false))]
    #[snafu(display("Utils error"))]
    Utils { source: utils::Error },

    #[snafu(context(false))]
    ReadDirPath { source: io::Error },

    #[snafu(display("The stray path {} was found that could not be made into a trash entry", path.display()))]
    StrayPath { path: PathBuf },
}

type Result<T, E = Error> = ::std::result::Result<T, E>;

#[macro_export]
macro_rules! ok_log {
    ($res:expr => $log_macro:ident!) => {
        match $res {
            Ok(t) => Some(t),
            Err(e) => {
                $log_macro!("{}", e);
                None
            }
        }
    };
    ($res:expr => $print_func:ident) => {
        match $res {
            Ok(t) => Some(t),
            Err(e) => {
                $print_func(format!("{}", e));
                None
            }
        }
    };
}

/// Helper function
pub fn restore(name: impl AsRef<Path>) -> Result<()> {
    Ok(TrashEntry::new(name)
        .context(TrashEntryNew)?
        .restore()
        .context(TrashEntryRestore)?)
}

/// Helper function
pub fn remove(name: impl AsRef<Path>) -> Result<()> {
    Ok(TrashEntry::new(name)
        .context(TrashEntryNew)?
        .remove()
        .context(TrashEntryRemove)?)
}

/// Removes all trash_entries and optionally removes stray files.
pub fn empty(keep_stray: bool) -> Result<()> {
    // remove the the correct trash_entries
    if !keep_stray {
        utils::empty_dirs(&[&*TRASH_FILE_DIR, &*TRASH_INFO_DIR])?;
        return Ok(());
    }

    read_dir_trash_entries()
        .context(ReadDirTrashEntry)?
        .map(|trash_entry| trash_entry.remove().context(TrashEntryRemove))
        .filter_map(|res| ok_log!(res => error!))
        .for_each(|_| ());

    Ok(())
}

/// Same as empty but removes everything in parallel
#[cfg(feature = "rayon")]
pub fn empty_parallel(keep_stray: bool) -> Result<()> {
    // just remove everything
    if !keep_stray {
        utils::empty_dir_parallel(&[&*TRASH_FILE_DIR, &*TRASH_INFO_DIR])?;
        return Ok(());
    }

    let entries: Vec<_> = read_dir_trash_entries()
        .context(ReadDirTrashEntry)?
        .collect();
    entries
        .into_par_iter()
        .map(|trash_entry| trash_entry.remove().context(TrashEntryRemove))
        .filter_map(|res| ok_log!(res => error!))
        .for_each(|_| ());

    Ok(())
}

/// Put a list of paths into the trash and returns the newly created trash_entries. Will panic if the
/// paths are empty!
pub fn put(paths: &[impl AsRef<Path>]) -> Result<Vec<TrashEntry>> {
    if paths.is_empty() {
        panic!("Attempting to put empty paths");
    }

    let mut existing: Vec<_> = read_dir_trash_entries()
        .context(ReadDirTrashEntry)?
        .collect();

    // in case there are no existing trash entries there, prevent integer overflow
    let old_trash_entries_end = existing.len().checked_sub(1);
    debug!("Old trash entries end: {:?}", old_trash_entries_end);

    for path in paths {
        let trash_entry = TrashEntry::create(path, &existing).context(TrashEntryCreation)?;
        existing.push(trash_entry)
    }

    // in case integer overflow happened
    if let Some(old_trash_entries_end) = old_trash_entries_end {
        existing.drain(..old_trash_entries_end);
    }

    Ok(existing)
}

// pub fn put_parallel(paths: &[impl AsRef<Path> + Sized]) -> Result<Vec<TrashEntry>> {
//     if paths.is_empty() {
//         panic!("Attempting to put empty paths");
//     }

//     let existing: Vec<_> = read_dir_trash_entries()
//         .context(ReadDirTrashEntry)?
//         .collect();

//     // in case there are no existing trash entries there, prevent integer overflow
//     let old_trash_entries_end = existing.len().checked_sub(1);
//     debug!("Old trash entries end: {:?}", old_trash_entries_end);

//     let existing = Arc::new(Mutex::new(existing));

//     paths.into_par_iter()
//         .for_each(move |p| {
//             let existing = Arc::clone(&existing);
//             let existing = existing.lock();
//             let trash_entry = TrashEntry::create(path, &existing).context(TrashEntryCreation)?;
//             existing.push(trash_entry)
//         });

//     // in case integer overflow happened
//     if let Some(old_trash_entries_end) = old_trash_entries_end {
//         existing.drain(..old_trash_entries_end);
//     }

//     Ok(existing)
// }

#[cfg(test)]
mod tests {
    use super::*;
    use eyre::Result;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[should_panic]
    #[test]
    fn put_test_nothing_test() {
        let nothing: [&str; 0] = [];
        let _ = put(&nothing);
    }

    #[ignore]
    #[test]
    fn put_test_single() -> Result<()> {
        let mut tempfile = NamedTempFile::new()?;
        tempfile.write_all(b"this is for the put_test_single")?;
        put(&[tempfile.path()])?;
        // tempfile.close()?; // TODO: fix this failure

        Ok(())
    }
}
