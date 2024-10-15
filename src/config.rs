// use config::{Config, Environment, File};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;
use std::str::FromStr;
use std::{fs, io};

use crate::types::{CTabSet, ErrorKind};

#[derive(Debug, Deserialize, Serialize)]
pub struct HighConfig {
    pub gobble: u16,
    pub break_after: BreakAfterType,
    pub break_indent: u16,
    pub tab_to_spaces: bool,
    pub tabs_len: u16,
    pub replace_space: bool,
    pub replace_tab: bool,
    pub lines: [u16; 2],
    #[serde(skip_serializing_if = "LexerRangeType::is_empty")]
    pub ranges: LexerRangeType,
}
impl Default for HighConfig {
    fn default() -> Self {
        HighConfig {
            gobble: 0,
            break_after: vec![' ', '\t'].into(),
            break_indent: 2,
            tab_to_spaces: true,
            tabs_len: 2,
            replace_space: false,
            replace_tab: false,
            lines: [0, 0],
            ranges: LexerRangeType::default(),
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(transparent)]
pub struct BreakAfterType(Vec<char>);
impl From<Vec<char>> for BreakAfterType {
    fn from(value: Vec<char>) -> Self {
        BreakAfterType(value)
    }
}
// #[derive(Debug, Serialize, Deserialize)]
// #[serde(tag = "Type")]
// pub enum BreakAfterType {
//     CS, // control sequence
//     RegTEx(String), // tex regular expression
//     RegEx(String),  // regular expression
//     Chars(Vec<char>), // char list
// }

#[derive(Debug, Deserialize, Serialize)]
#[serde(transparent)]
pub struct LexerRangeType(HashMap<String, [[u16; 2]; 2]>);
impl LexerRangeType {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}
impl Default for LexerRangeType {
    fn default() -> Self {
        LexerRangeType(HashMap::new())
    }
}

pub struct THConfig {
    pub ctabs: CTabSet,
    pub high_config: HighConfig,
    pub cache_dir: String,
    pub use_kpathsea: bool,
    pub debug: bool,
    pub force: bool,
}

impl THConfig {
    pub fn new() -> Self {
        THConfig {
            ctabs: CTabSet::new(),
            high_config: HighConfig::default(),
            cache_dir: String::from("./texhigh-cache"),
            use_kpathsea: false,
            debug: false,
            force: false,
        }
    }

    pub fn set_ctabset_from_str(&mut self, s: &str) -> Result<(), ErrorKind> {
        let ctabs = CTabSet::from_str(s)?;
        self.ctabs.extend(ctabs);
        Ok(())
    }

    pub fn set_ctabset_from_file<F: AsRef<Path>>(&mut self, f: F) -> Result<(), ErrorKind> {
        let f = fs::File::open(f.as_ref()).or(Err(ErrorKind::InvalidPath))?;
        let ctabs_str = io::read_to_string(f).or(Err(ErrorKind::InvalidFileContent))?;
        let ctabs = CTabSet::from_str(&ctabs_str)?;
        self.ctabs.extend(ctabs);
        Ok(())
    }
}
