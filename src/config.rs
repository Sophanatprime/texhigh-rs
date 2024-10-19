use serde::{Deserialize, Serialize};
use serde_with::{DeserializeFromStr, SerializeDisplay};
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::path::Path;
use std::str::FromStr;
use std::{fs, io};
use regex::Regex;

use crate::types::{CTabSet, ErrorKind};

#[derive(Debug, DeserializeFromStr, SerializeDisplay)]
pub struct HighRegex(Regex);
impl HighRegex {
    pub fn is_match(&self, s: &str) -> bool {
        self.0.is_match(s)
    }
}
impl Display for HighRegex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0.as_str())
    }
}
impl FromStr for HighRegex {
    type Err = ErrorKind;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match Regex::new(s) {
            Ok(re) => Ok(HighRegex(re)),
            Err(_) => Err(ErrorKind::RegexError),
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum Category {
    CS(HashSet<String>),
    Regex(HighRegex),
}
impl Category {
    pub fn contains(&self, s: &str) -> bool {
        match self {
            Category::CS(cs) => cs.contains(s),
            Category::Regex(re) => re.is_match(s),
        }
    }
}
#[derive(Debug, Deserialize, Serialize)]
pub struct CSCategories(pub HashMap<String, Category>);
impl CSCategories {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    pub fn categories(&self, s: &str) -> Option<&str> {
        for (k, v) in self.0.iter() {
            if v.contains(s) {
                return Some(k);
            }
        }
        None
    }
}
impl Default for CSCategories {
    fn default() -> Self {
        Self(HashMap::new())
    }
}

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
    #[serde(skip_serializing_if = "CSCategories::is_empty")]
    pub cs_categories: CSCategories,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    pub ctabs_fallback: HashMap<String, Vec<String>>,
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
            cs_categories: CSCategories::default(),
            ctabs_fallback: HashMap::new(),
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
pub struct LexerRangeType(pub HashMap<String, [[u16; 2]; 2]>);
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
