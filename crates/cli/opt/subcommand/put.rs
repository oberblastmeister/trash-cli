use std::path::PathBuf;

use clap::Clap;
use eyre::{eyre, Result};
use trash_lib::trash_entry::TrashEntry;

#[derive(Clap, Debug, PartialEq)]
pub struct Opt {
    #[clap(parse(from_os_str))]
    pub paths: Vec<PathBuf>,

    // compatibility
    /// ignored (for GNU rm compatibility)
    #[clap(short, long)]
    pub directory: bool,

    /// ignored (for GNU rm compatibility)
    #[clap(short, long)]
    pub force: bool,

    /// ignored (for GNU rm compatibility)
    #[clap(short, long)]
    pub interactive: bool,

    /// ignored (for GNU rm compatibility)
    #[clap(short, long = "R")]
    pub recursive: bool,

    #[clap(long = "recursive")]
    pub recursive_long: bool,
}

pub fn put(opt: Opt) -> Result<Vec<TrashEntry>> {
    let paths = &opt.paths;
    if paths.is_empty() {
        return Err(eyre!("No paths were specified to trash!"));
    }
    trash_lib::put(paths).map_err(Into::into);
}
