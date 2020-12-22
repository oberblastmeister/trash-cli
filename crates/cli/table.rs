use std::borrow::Cow;

use clap::{ArgEnum, Clap};
use eyre::{eyre, Result};
use log::{debug, trace};
use prettytable::{cell, row, Cell, Row, Table};
use terminal_size::{terminal_size, Width};

use crate::border::Border;
use crate::utils::{date, path};
use crate::utils::{get_metadata, Pair};

pub struct SizedTable {
    opt: Opt,
    table: Table,
}

impl SizedTable {
    pub fn new(opt: Opt) -> Result<Self> {
        let title_row = if !opt.no_title {
            Some(opt.size.get_title_row())
        } else {
            None
        };

        let table = create_table(title_row, opt.border);
        let sized_table = SizedTable { opt, table };
        Ok(sized_table)
    }

    pub fn add_row(&mut self, pair: &Pair) -> Result<()> {
        let row = self.get_row(pair)?;
        self.table.add_row(row);
        Ok(())
    }

    fn get_row(&self, pair: &Pair) -> Result<Row> {
        let Pair(ref trash_entry, ref trash_info) = pair;

        let path = trash_info.percent_path();
        trace!("Path before decoded: {}", path);
        let path = path.decoded()?;
        trace!("After decoded path: {}", path);

        let mut displayed_path = if !self.opt.absolute {
            let path = path.as_ref();
            Cow::from(path::shorten(path).unwrap())
        } else {
            path
        };
        trace!("After displayed path (shorten stuff): {}", displayed_path);

        displayed_path = if !self.opt.dont_colorize {
            let metadata = get_metadata(&trash_entry)?;
            Cow::from(path::colorize(displayed_path.as_ref(), &metadata))
        } else {
            displayed_path
        };
        trace!("After colorized path: {}", displayed_path);

        trace!("Add adding size {:?} row", self.opt.size);
        let row = match self.opt.size {
            TableSize::Minimal => row![displayed_path],
            TableSize::Compact => {
                let mut res = date::format(trash_info.deletion_date());
                res.push(Cell::new(&displayed_path));
                Row::new(res)
            }
            TableSize::Full => {
                let mut res = date::format(trash_info.deletion_date());
                res.push(Cell::new(&displayed_path));
                Row::new(res)
            }
        };
        Ok(row)
    }

    pub fn print(&self) -> usize {
        self.table.printstd()
    }
}

pub struct IndexedTable(SizedTable);

impl IndexedTable {
    pub fn new(opt: Opt) -> Result<Self> {
        let title_row = if !opt.no_title {
            Some(opt.size.get_title_row_index())
        } else {
            None
        };
        let table = create_table(title_row, opt.border);
        Ok(IndexedTable(SizedTable { opt, table }))
    }

    pub fn add_row(&mut self, pair: &Pair) -> Result<()> {
        let mut row = self.0.get_row(pair)?;
        // insert the index
        let index = self.0.table.len() + 1;
        debug!("current index (1 based): {}", index);
        row.insert_cell(0, cell!(index));
        self.0.table.add_row(row);
        Ok(())
    }

    pub fn print(&self) -> usize {
        self.0.print()
    }
}

fn get_terminal_width() -> Result<Width> {
    let width = terminal_size()
        .ok_or_else(|| eyre!("Unable to get terminal size"))?
        .0;
    Ok(width)
}

#[derive(Debug, Copy, Clone, ArgEnum)]
pub enum TableSize {
    /// only displays path
    Minimal,

    /// displays all informations in compact size
    Compact,

    /// displays all information fully
    Full,
}

impl From<Width> for TableSize {
    fn from(value: Width) -> TableSize {
        let Width(w) = value;
        match w {
            0..=45 => TableSize::Minimal,
            46..=90 => TableSize::Compact,
            _ => TableSize::Full,
        }
    }
}

impl TableSize {
    fn try_new() -> Result<TableSize> {
        Ok(get_terminal_width()?.into())
    }

    fn get_title_row(self) -> Row {
        match self {
            TableSize::Minimal => row!["Path"],
            TableSize::Compact => row!["Month", "Date", "Time", "Path"],
            TableSize::Full => row!["Month", "Day", "Time", "Path"],
        }
    }

    fn get_title_row_index(self) -> Row {
        let mut row = self.get_title_row();
        row.insert_cell(0, Cell::new("Index"));
        row
    }

    fn create_table(self, border: Border) -> Table {
        let mut table = Table::new();
        table.set_titles(self.get_title_row());
        table.set_format(border.into());
        table
    }
}

fn create_table(title_row: Option<Row>, border: Border) -> Table {
    let mut table = Table::new();
    if let Some(title) = title_row {
        table.set_titles(title);
    }
    table.set_format(border.into());
    table
}

#[derive(Clap, Debug)]
pub struct Opt {
    /// The border to use
    #[clap(
        arg_enum,
        short,
        long,
        default_value = "Sharp",
        case_insensitive = true,
        env = "TRASHY_BORDER"
    )]
    border: Border,

    /// The size of the table to use
    #[clap(
        arg_enum,
        default_value = "Full",
        case_insensitive = true,
        short,
        long,
        env = "TRASHY_SIZE"
    )]
    size: TableSize,

    /// Wheather to colorize output
    #[clap(short = 'c', long)]
    dont_colorize: bool,

    /// Wheater to get the absolute path instead of shortening the output
    #[clap(short, long)]
    absolute: bool,

    #[clap(short = 't', long, env = "TRASHY_NO_TITLE")]
    no_title: bool,
}
