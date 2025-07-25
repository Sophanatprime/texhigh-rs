use compact_str::CompactString;
use hashers::fx_hash::FxHasher;
use indexmap::IndexMap;
use log::warn;
use regex::{Regex, RegexSet};
use serde::{Deserialize, Serialize, Serializer};
use smallvec::SmallVec;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::fmt::Display;
use std::hash::BuildHasherDefault;
use std::ops::Deref;
use std::path::Path;
use std::slice::SliceIndex;
use std::str::FromStr;
use std::{fs, io};

use crate::regtex::{RegTEx, RegTExSet};
use crate::tex::{args_parser, CatCode};
use crate::tokenlist::SourcedTokenList;
use crate::types::{
    CTabSet, ErrorKind, Position, Token, TokenListBytes, TokenListBytesRef,
};

#[derive(Debug, Clone)]
pub enum Input<'i> {
    Char(char),
    Span([u32; 2]),
    String(&'i str),
    Tokens(&'i [Token]),
    Bytes(&'i [u8]),
    TokenListBytes(&'i TokenListBytes),
    TokenListBytesRef(&'i TokenListBytesRef<'i>),
    SourcedTokenList(&'i SourcedTokenList),
}
impl From<char> for Input<'_> {
    fn from(value: char) -> Self {
        Self::Char(value)
    }
}
impl From<[u32; 2]> for Input<'_> {
    fn from(value: [u32; 2]) -> Self {
        Self::Span(value)
    }
}
impl<'i> From<&'i str> for Input<'i> {
    fn from(value: &'i str) -> Self {
        Self::String(value)
    }
}
impl<'i> From<&'i [Token]> for Input<'i> {
    fn from(value: &'i [Token]) -> Self {
        Self::Tokens(value)
    }
}
impl<'i> From<&'i [u8]> for Input<'i> {
    fn from(value: &'i [u8]) -> Self {
        Self::Bytes(value)
    }
}
impl<'i> From<&'i TokenListBytes> for Input<'i> {
    fn from(value: &'i TokenListBytes) -> Self {
        Self::TokenListBytes(value)
    }
}
impl<'i> From<&'i TokenListBytesRef<'i>> for Input<'i> {
    fn from(value: &'i TokenListBytesRef) -> Self {
        Self::TokenListBytesRef(value)
    }
}
impl<'i> From<&'i SourcedTokenList> for Input<'i> {
    fn from(value: &'i SourcedTokenList) -> Self {
        Self::SourcedTokenList(value)
    }
}

pub trait CategorySpan:
    SliceIndex<str, Output = str>
    + SliceIndex<[u8], Output = [u8]>
    + SliceIndex<[usize], Output = [usize]>
    + SliceIndex<[Token], Output = [Token]>
    + Clone
{
}
impl<
        T: SliceIndex<str, Output = str>
            + SliceIndex<[u8], Output = [u8]>
            + SliceIndex<[usize], Output = [usize]>
            + SliceIndex<[Token], Output = [Token]>
            + Clone,
    > CategorySpan for T
{
}

#[derive(Debug)]
pub enum Category {
    None,
    Any,
    Span([u32; 2]),
    String(String),
    Regex(Regex),
    RegTEx(RegTEx),
}

impl PartialEq for Category {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::None => other == &Self::None,
            Self::Any => other == &Self::Any,
            Self::Span(span) => other == &Self::Span(*span),
            Self::String(s) => {
                if let Self::String(s_o) = other {
                    s == s_o
                } else {
                    false
                }
            }
            Self::Regex(re) => {
                if let Self::Regex(re_o) = other {
                    re.as_str() == re_o.as_str()
                } else {
                    false
                }
            }
            Self::RegTEx(re) => {
                if let Self::RegTEx(re_o) = other {
                    re.as_str() == re_o.as_str()
                } else {
                    false
                }
            }
        }
    }
}
impl Display for Category {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (name, value) = match self {
            Self::None => ("Empty", "".to_string()),
            Self::Any => ("Any", "".to_string()),
            Self::Span(span) => {
                ("Span", format!("(row: {}, column: {})", span[0], span[1]))
            }
            Self::String(s) => ("String", format!("({})", s)),
            Self::Regex(re) => ("Regex", format!("({})", re.as_str())),
            Self::RegTEx(re) => ("RegTEx", format!("({})", re.as_str())),
        };
        write!(f, "Category::{}{}", name, value)
    }
}

impl Category {
    pub fn is_match(&self, pos: &Position<'_>) -> bool {
        match self {
            Category::None => false,
            Category::Any => true,
            Category::Span([row, col]) => pos.row == *row && pos.col == *col,
            Category::String(s) => {
                pos.source.is_some_and(|v| v.starts_with(s))
            }
            Category::Regex(re) => pos.source.is_some_and(|v| re.is_match(v)),
            Category::RegTEx(re) => {
                pos.bytes.is_some_and(|v| unsafe { re.is_match_bytes(v) })
            }
        }
    }
    pub fn is_match_at(
        &self,
        pos: &Position<'_>,
        span: impl CategorySpan,
    ) -> bool {
        match self {
            Category::None => false,
            Category::Any => false,
            Category::Span([row, col]) => pos.row == *row && pos.col == *col,
            Category::String(s) => {
                pos.source.is_some_and(|v| v[span].starts_with(s))
            }
            Category::Regex(re) => {
                pos.source.is_some_and(|v| re.is_match(&v[span]))
            }
            Category::RegTEx(re) => pos
                .bytes
                .is_some_and(|v| unsafe { re.is_match_bytes(&v[span]) }),
        }
    }
    pub fn match_len(&self, pos: &Position<'_>) -> Option<usize> {
        match self {
            Category::None => None,
            Category::Any => Some(0),
            Category::Span([row, col]) => {
                (pos.row == *row && pos.col == *col).then_some(0)
            }
            Category::String(s) => {
                pos.source.and_then(|v| v.starts_with(s).then_some(s.len()))
            }
            Category::Regex(re) => {
                pos.source.and_then(|v| re.find(v).and_then(|m| Some(m.len())))
            }
            Category::RegTEx(re) => pos.bytes.and_then(|v| {
                unsafe { re.find_bytes(v) }.and_then(|m| Some(m.len()))
            }),
        }
    }
    pub fn match_len_at(
        &self,
        pos: &Position<'_>,
        span: impl CategorySpan,
    ) -> Option<usize> {
        match self {
            Category::None => None,
            Category::Any => Some(0),
            Category::Span([row, col]) => {
                (pos.row == *row && pos.col == *col).then_some(0)
            }
            Category::String(s) => pos
                .source
                .and_then(|v| v[span].starts_with(s).then_some(s.len())),
            Category::Regex(re) => pos
                .source
                .and_then(|v| re.find(&v[span]).and_then(|m| Some(m.len()))),
            Category::RegTEx(re) => pos.bytes.and_then(|v| {
                unsafe { re.find_bytes(&v[span]) }.and_then(|m| Some(m.len()))
            }),
        }
    }
}

impl Serialize for Category {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Category::None => serializer.collect_str(""),
            Category::Any => serializer.collect_str("\\L"),
            Category::Span(span) => serializer.collect_seq(span),
            Category::String(s) => {
                serializer.collect_str(&format!("\\L{}", s))
            }
            Category::Regex(re) => {
                serializer.collect_str(&format!("\\I{}", re.as_str()))
            }
            Category::RegTEx(re) => {
                serializer.collect_str(&format!("\\T{}", re.as_str()))
            }
        }
    }
}
impl<'de> Deserialize<'de> for Category {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum StringOrSpan {
            SpanRC([u32; 2]),
            Span(u32),
            String(String),
        }
        let data = StringOrSpan::deserialize(deserializer)?;
        let item = match data {
            StringOrSpan::String(s) => {
                if s.is_empty() {
                    Category::None
                } else if s.starts_with("\\L") {
                    if s[2 ..].is_empty() {
                        Category::Any
                    } else {
                        Category::String(s[2 ..].to_owned())
                    }
                } else if s.starts_with("\\I") {
                    Category::Regex(
                        Regex::new(&s[2 ..])
                            .map_err(serde::de::Error::custom)?,
                    )
                } else if s.starts_with("\\T") {
                    Category::RegTEx(
                        RegTEx::new(&s[2 ..])
                            .map_err(serde::de::Error::custom)?,
                    )
                } else {
                    Category::RegTEx(
                        RegTEx::new(&s).map_err(serde::de::Error::custom)?,
                    )
                }
            }
            StringOrSpan::SpanRC(span) => Category::Span(span),
            StringOrSpan::Span(row) => {
                if row == u32::MAX {
                    Category::Any
                } else {
                    Category::Span([row, 0])
                }
            }
        };
        Ok(item)
    }
}

#[derive(Debug)]
pub struct Category2 {
    pub strings: HashSet<String, BuildHasherDefault<FxHasher>>,
    pub regexset: RegexSet,
}

impl Category2 {
    pub fn contains<'a>(&self, s: impl Into<Input<'a>>) -> bool {
        match s.into() {
            Input::String(s) => {
                self.strings.contains(s) || self.regexset.is_match(s)
            }
            _ => false,
        }
    }
}

impl Serialize for Category2 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut res = BTreeSet::new();

        // Add strings with \L prefix
        for s in self.strings.iter() {
            res.insert(format!("\\L{}", s));
        }

        // Add regex patterns with \I prefix
        for pattern in self.regexset.patterns() {
            res.insert(format!("\\I{}", pattern));
        }

        serializer.collect_seq(res)
    }
}
impl<'de> Deserialize<'de> for Category2 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum StringOrVec {
            String(String),
            Vec(Vec<String>),
        }

        let data = StringOrVec::deserialize(deserializer)?;
        let mut strings = HashSet::default();
        let mut regex_patterns = Vec::new();

        match &data {
            StringOrVec::String(s) => {
                regex_patterns.push(s.as_str());
            }
            StringOrVec::Vec(vec) => {
                for s in vec {
                    if s.is_empty() {
                        continue;
                    } else if s.starts_with("\\L") {
                        strings.insert(s[2 ..].to_owned());
                    } else if s.starts_with("\\I") {
                        regex_patterns.push(&s[2 ..]);
                    } else {
                        strings.insert(s.to_owned());
                    }
                }
            }
        }

        let regexset = RegexSet::new(&regex_patterns)
            .map_err(serde::de::Error::custom)?;

        Ok(Category2 { strings, regexset })
    }
}

#[derive(Debug)]
pub struct CategoryC {
    pub chars: HashSet<char, BuildHasherDefault<FxHasher>>,
    pub regexset: RegexSet,
    pub regtexset: RegTExSet,
}

impl Serialize for CategoryC {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut res = BTreeSet::new();

        // Add strings with \L prefix
        for s in self.chars.iter() {
            res.insert(format!("{}", s));
        }

        // Add regex patterns with \I prefix
        for pattern in self.regexset.patterns() {
            res.insert(format!("\\I{}", pattern));
        }

        // Add regex patterns with \T prefix
        for pattern in self.regtexset.patterns() {
            res.insert(format!("\\T{}", pattern));
        }

        serializer.collect_seq(res)
    }
}
impl<'de> Deserialize<'de> for CategoryC {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum StringOrVec {
            String(CompactString),
            Vec(Vec<CompactString>),
        }

        let data = StringOrVec::deserialize(deserializer)?;
        let mut chars = HashSet::default();
        let mut regex_patterns = SmallVec::<[&str; 8]>::new();
        let mut regtex_pattern = SmallVec::<[&str; 8]>::new();

        match &data {
            StringOrVec::String(s) => {
                regex_patterns.push(&s);
            }
            StringOrVec::Vec(vec) => {
                for s in vec {
                    if s.is_empty() {
                        continue;
                    } else if s.starts_with("\\L") {
                        let chr = s[2 ..].chars().next().ok_or(
                            serde::de::Error::custom("Illegal \\L mark"),
                        )?;
                        chars.insert(chr);
                    } else if s.starts_with("\\I") {
                        regex_patterns.push(&s[2 ..]);
                    } else if s.starts_with("\\T") {
                        regtex_pattern.push(&s[2 ..]);
                    } else {
                        let mut iter = s.chars();
                        let chr = iter.next().unwrap(); // we can ensure s.len > 0
                        if iter.next().is_none() {
                            chars.insert(chr);
                        } else {
                            regex_patterns.push(&s);
                        }
                    }
                }
            }
        }

        let regexset = RegexSet::new(&regex_patterns)
            .map_err(serde::de::Error::custom)?;
        let regtexset = RegTExSet::new(&regtex_pattern)
            .map_err(serde::de::Error::custom)?;

        Ok(CategoryC { chars, regexset, regtexset })
    }
}

#[derive(Debug)]
pub struct Category3 {
    pub strings: HashSet<String, BuildHasherDefault<FxHasher>>,
    pub regexset: RegexSet,
    pub regtexset: RegTExSet,
}

impl Category3 {
    pub fn is_empty(&self) -> bool {
        self.strings.is_empty()
            && self.regexset.is_empty()
            && self.regtexset.is_empty()
    }
    pub fn contains<'a>(&self, s: impl Into<Input<'a>>) -> bool {
        match s.into() {
            Input::Char(c) => {
                let mut buf = [0u8; 4];
                let s = c.encode_utf8(&mut buf);
                self.strings.contains(s) || self.regexset.is_match(s)
            }
            Input::Span(_) => false,
            Input::String(s) => {
                self.strings.contains(s) || self.regexset.is_match(s)
            }
            Input::Bytes(b) => unsafe { self.regtexset.is_match_bytes(b) },
            Input::Tokens(_) => false,
            Input::TokenListBytes(b) => self.regtexset.is_match(b),
            Input::TokenListBytesRef(b) => unsafe {
                self.regtexset.is_match_bytes(&b.bytes)
            },
            Input::SourcedTokenList(b) => {
                self.regexset.is_match(b.sources())
                    || unsafe { self.regtexset.is_match_bytes(b.bytes()) }
            }
        }
    }
    pub fn contains_at<'a>(
        &self,
        s: impl Into<Input<'a>>,
        span: impl CategorySpan,
    ) -> bool {
        match s.into() {
            Input::Char(c) => {
                let mut buf = [0u8; 4];
                let s = c.encode_utf8(&mut buf);
                self.strings.contains(s) || self.regexset.is_match(s)
            }
            Input::Span(_) => false,
            Input::String(s) => {
                self.strings.contains(&s[span.clone()])
                    || self.regexset.is_match(&s[span])
            }
            Input::Bytes(b) => unsafe {
                self.regtexset.is_match_bytes(&b[span])
            },
            Input::Tokens(_) => false,
            Input::TokenListBytes(b) => unsafe {
                self.regtexset.is_match_bytes(&b.values[span])
            },
            Input::TokenListBytesRef(b) => unsafe {
                self.regtexset.is_match_bytes(&b.bytes[span])
            },
            Input::SourcedTokenList(b) => {
                b.source_get(span.clone())
                    .is_some_and(|v| self.regexset.is_match(v))
                    || b.bytes_get(span).is_some_and(|v| unsafe {
                        self.regtexset.is_match_bytes(v)
                    })
            }
        }
    }
}

impl Serialize for Category3 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut res = BTreeSet::new();

        // Add strings with \L prefix
        for s in self.strings.iter() {
            res.insert(format!("\\L{}", s));
        }

        // Add Regex patterns with \I prefix
        for pattern in self.regexset.patterns() {
            res.insert(format!("\\I{}", pattern));
        }

        // Add RegTEx patterns with \T prefix
        for pattern in self.regtexset.patterns() {
            res.insert(format!("\\T{}", pattern));
        }

        serializer.collect_seq(res)
    }
}
impl<'de> Deserialize<'de> for Category3 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum StringOrVec {
            String(String),
            Vec(Vec<String>),
        }

        let data = StringOrVec::deserialize(deserializer)?;
        let mut strings = HashSet::default();
        let mut regex_patterns = Vec::new();
        let mut regtex_patterns = Vec::new();

        match &data {
            StringOrVec::String(s) => {
                if s.starts_with("\\I") {
                    regex_patterns.push(&s[2 ..]);
                } else if s.starts_with("\\T") {
                    regtex_patterns.push(&s[2 ..]);
                } else {
                    regex_patterns.push(s.as_str());
                }
            }
            StringOrVec::Vec(vec) => {
                for s in vec {
                    if s.is_empty() {
                        continue;
                    } else if s.starts_with("\\L") {
                        strings.insert(s[2 ..].to_owned());
                    } else if s.starts_with("\\I") {
                        regex_patterns.push(&s[2 ..]);
                    } else if s.starts_with("\\T") {
                        regtex_patterns.push(&s[2 ..]);
                    } else {
                        strings.insert(s.to_owned());
                    }
                }
            }
        }

        let regexset = RegexSet::new(&regex_patterns)
            .map_err(serde::de::Error::custom)?;
        let regtexset = RegTExSet::new(&regtex_patterns)
            .map_err(serde::de::Error::custom)?;

        Ok(Category3 { strings, regexset, regtexset })
    }
}

#[derive(Debug, Default, Deserialize, Serialize)]
pub struct CharCategories(pub IndexMap<String, Category3>);
impl CharCategories {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    pub fn categories<'i>(&self, s: impl Into<Input<'i>>) -> Option<&str> {
        let s = s.into();
        self.0
            .iter()
            .find_map(|(k, v)| v.contains(s.clone()).then_some(k.as_str()))
    }
}

#[derive(Debug, Default, Deserialize, Serialize)]
pub struct CSCategories(pub IndexMap<String, Category2>);
impl CSCategories {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    pub fn categories(&self, s: &str) -> Option<&str> {
        self.0
            .iter() // Using par_iter() make worse performance, why?
            .find_map(|(k, v)| v.contains(s).then_some(k.as_str()))
    }
}

#[derive(Debug, Serialize, PartialEq, Eq, PartialOrd, Ord)]
#[serde(transparent)]
pub struct ShortInt(pub i16);

impl Deref for ShortInt {
    type Target = i16;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'de> Deserialize<'de> for ShortInt {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum IntOrStr {
            Int(i16),
            Str(String),
        }
        match IntOrStr::deserialize(deserializer)? {
            IntOrStr::Int(i) => Ok(ShortInt(i)),
            IntOrStr::Str(s) => {
                if s == "auto" {
                    Ok(ShortInt(-1))
                } else {
                    let i =
                        i16::from_str(&s).map_err(serde::de::Error::custom)?;
                    Ok(ShortInt(i))
                }
            }
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct HighConfig {
    pub gobble: ShortInt,
    pub break_at: BreakAtType,
    pub break_indent: u16,
    pub replace_tab: bool,
    pub tabs_len: ShortInt,
    pub lines: [u32; 2],
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub do_not_break: Vec<Category3>,
    #[serde(skip_serializing_if = "LexerType::is_empty")]
    pub lexer: LexerType,
    #[serde(skip_serializing_if = "CharsType::is_empty")]
    pub char_replacements: CharsType,
    #[serde(skip_serializing_if = "HashSet::is_empty")]
    pub enable_ranges: HashSet<String, BuildHasherDefault<FxHasher>>,
    #[serde(skip_serializing_if = "RangeType::is_empty")]
    pub ranges: RangeType,
    #[serde(skip_serializing_if = "CharCategories::is_empty")]
    pub char_categories: CharCategories,
    #[serde(skip_serializing_if = "CSCategories::is_empty")]
    pub cs_categories: CSCategories,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    pub ctabs_fallback:
        HashMap<String, Vec<String>, BuildHasherDefault<FxHasher>>,
}

impl HighConfig {
    pub fn new() -> Self {
        HighConfig::default()
    }

    pub fn reorganize(&mut self) {
        self.ranges.0.retain(|k, r| {
            let contain = self.enable_ranges.contains(k);
            let contain_s =
                self.enable_ranges.contains(HighConfig::sanitize_name(k));
            let args = match r {
                RangeItem::Escape { arguments, .. } => arguments,
                RangeItem::Normal { arguments, .. } => arguments,
            };
            let arg_check = if args.len() <= 9 {
                true
            } else {
                warn!(
                    target: "argument counts",
                    "Too many arguments for {:?} ({} arguments found)",
                    args.args_specs(), args.len()
                );
                false
            };
            let res = (contain || contain_s) && arg_check;
            if !res {
                log::info!("range '{}' has been removed", k);
            }
            res
        });
    }

    pub fn sanitize_name(key: &str) -> &str {
        if key.len() <= 1 || key.as_bytes()[0] != b'+' {
            return key;
        }
        if key.as_bytes()[1] == b'[' {
            match key.as_bytes().iter().position(|&v| v == b']') {
                Some(idx) => {
                    if idx + 1 < key.len() {
                        key.get(idx + 1 ..).unwrap()
                    } else {
                        key
                    }
                }
                None => key.get(1 ..).unwrap(),
            }
        } else {
            &key[1 ..]
        }
    }

    pub fn real_name(key: &str) -> &str {
        Self::sanitize_name(key)
    }
}
impl Default for HighConfig {
    fn default() -> Self {
        HighConfig {
            gobble: ShortInt(0),
            break_at: BreakAtType(Vec::new()),
            break_indent: 2,
            replace_tab: false,
            tabs_len: ShortInt(2),
            lines: [0, 0],
            do_not_break: Vec::new(),
            lexer: LexerType::default(),
            char_replacements: CharsType::default(),
            enable_ranges: HashSet::default(),
            ranges: RangeType::default(),
            char_categories: CharCategories::default(),
            cs_categories: CSCategories::default(),
            ctabs_fallback: HashMap::default(),
        }
    }
}

#[derive(Debug, Default, Deserialize, Serialize)]
#[serde(transparent)]
pub struct BreakAtType(pub Vec<char>);
impl BreakAtType {
    pub fn contains(&self, chr: char) -> bool {
        self.0.contains(&chr)
    }
}

#[derive(Debug, Default, Deserialize, Serialize)]
#[serde(transparent)]
pub struct LexerType(pub Vec<(Category, Category, LexerAction)>);
impl LexerType {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

#[non_exhaustive]
#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "action", content = "value")]
pub enum LexerAction {
    CatCode(LexerCatCodeKind),
    EndLine(u32),
}
#[derive(Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum LexerCatCodeKind {
    Char(Vec<(char, CatCode)>),
    CTab(String),
}

#[derive(Debug, Default, Serialize)]
#[serde(transparent)]
pub struct CharsType(HashSet<char, BuildHasherDefault<FxHasher>>);
impl CharsType {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    pub fn contains(&self, c: &char) -> bool {
        self.0.contains(c)
    }
}
impl<'de> Deserialize<'de> for CharsType {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum VecOrStr {
            Vec(HashSet<char, BuildHasherDefault<FxHasher>>),
            Str(String),
        }

        match VecOrStr::deserialize(deserializer)? {
            VecOrStr::Vec(v) => Ok(CharsType(v)),
            VecOrStr::Str(s) => {
                let mut v =
                    HashSet::<char, BuildHasherDefault<FxHasher>>::default();
                v.extend(s.chars());
                Ok(CharsType(v))
            }
        }
    }
}

#[derive(Debug, Default, Deserialize, Serialize)]
#[serde(transparent)]
pub struct RangeType(
    IndexMap<String, RangeItem, BuildHasherDefault<FxHasher>>,
);
impl RangeType {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}
impl Deref for RangeType {
    type Target = IndexMap<String, RangeItem, BuildHasherDefault<FxHasher>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum RangeItem {
    Escape {
        start: Category,
        arguments: RangeItemArgs,
        remove_start: bool,
        #[serde(default)]
        use_argument: bool,
        #[serde(default)]
        insert_ending: bool,
        #[serde(default)]
        in_comments: RangeComments,
    },
    Normal {
        start: Category,
        arguments: RangeItemArgs,
        #[serde(default)]
        insert_ending: bool,
        #[serde(default)]
        in_comments: RangeComments,
    },
}

pub struct RangeItemArgs {
    specs: String,
    used_spec_names: Vec<u8>,
    finder: args_parser::ArgFinder,
}
impl RangeItemArgs {
    pub fn len(&self) -> usize {
        self.finder.len()
    }
    pub fn args_specs(&self) -> &str {
        &self.specs
    }
    pub fn spec_names(&self) -> &[u8] {
        &self.used_spec_names
    }
    pub fn find_all(
        &self,
        tokens: &[Token],
    ) -> Result<Vec<args_parser::Argument>, args_parser::ErrorKind> {
        self.finder.find_all(tokens)
    }
}
impl std::fmt::Debug for RangeItemArgs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RangeItemArgs")
            .field("spec", &self.specs)
            .field("finder", &format!("[<{} function(s)>]", self.finder.len()))
            .finish()
    }
}
impl Serialize for RangeItemArgs {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.specs)
    }
}
impl<'de> Deserialize<'de> for RangeItemArgs {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use crate::tex::args_parser::ArgFinder;
        use crate::tex::TokenList;
        use crate::types::CTab;

        let ref catcode = CTab::document();
        let specs = String::deserialize(deserializer)?;
        let spec_tl = TokenList::parse(&specs, catcode);
        let (finder, names) =
            ArgFinder::parse_with_specs(&spec_tl).map_err(|_| {
                serde::de::Error::custom(format!(
                    "Invalid arguments spec '{}'",
                    &specs
                ))
            })?;
        Ok(RangeItemArgs { finder, used_spec_names: names, specs })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum RangeComments {
    #[serde(alias = "must")]
    Required,
    #[serde(alias = "never", alias = "prohibited")]
    Forbidden,
    #[serde(alias = "dontcare", alias = "any")]
    Irrelevant,
}
impl Default for RangeComments {
    fn default() -> Self {
        Self::Irrelevant
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

    pub fn set_ctabset_from_file<F: AsRef<Path>>(
        &mut self,
        f: F,
    ) -> Result<(), ErrorKind> {
        let f = fs::File::open(f.as_ref()).or(Err(ErrorKind::InvalidPath))?;
        let ctabs_str =
            io::read_to_string(f).or(Err(ErrorKind::InvalidFileContent))?;
        let ctabs = CTabSet::from_str(&ctabs_str)?;
        self.ctabs.extend(ctabs);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore]
    // stack overflow, why?
    fn category() {
        #[derive(Deserialize)]
        struct TestStruct {
            key: Vec<Category>,
        }
        let t = r###"
        key = [ '', 'a', '\Ta', '\Ia', '\La', '\L', 0, 0xFFFFFFFF, [0, 0], [0xFFFFFFFF, 0] ]
        "###;

        let result = &[
            Category::None,
            Category::RegTEx(RegTEx::new("a").unwrap()),
            Category::RegTEx(RegTEx::new("a").unwrap()),
            Category::Regex(Regex::new("a").unwrap()),
            Category::String(String::from("a")),
            Category::Any,
            Category::Span([0, 0]),
            Category::Any,
            Category::Span([0, 0]),
            Category::Span([u32::MAX, 0]),
        ];
        let categories: TestStruct = toml::from_str(t).unwrap();
        assert_eq!(&categories.key, result);
    }

    #[test]
    fn sanitize() {
        assert_eq!(HighConfig::sanitize_name(""), "");
        assert_eq!(HighConfig::sanitize_name("@"), "@");
        assert_eq!(HighConfig::sanitize_name("@+"), "@+");
        assert_eq!(HighConfig::sanitize_name("EH"), "EH");
        assert_eq!(HighConfig::sanitize_name("@aaa"), "@aaa");
        assert_eq!(HighConfig::sanitize_name("好aaa"), "好aaa");
        assert_eq!(HighConfig::sanitize_name(" aaa"), " aaa");
        assert_eq!(HighConfig::sanitize_name(" +aaa"), " +aaa");
        assert_eq!(HighConfig::sanitize_name("+"), "+");
        assert_eq!(HighConfig::sanitize_name("+aaa"), "aaa");
        assert_eq!(HighConfig::sanitize_name("+好aaa"), "好aaa");
        assert_eq!(HighConfig::sanitize_name("+[aaa]"), "+[aaa]");
        assert_eq!(HighConfig::sanitize_name("+[]aaa"), "aaa");
        assert_eq!(HighConfig::sanitize_name("+[123]aaa"), "aaa");
        assert_eq!(HighConfig::sanitize_name("+[123]]aaa"), "]aaa");
        assert_eq!(HighConfig::sanitize_name("+[[123]]aaa"), "]aaa");
        assert_eq!(HighConfig::sanitize_name("+[123][456]aaa"), "[456]aaa");
        assert_eq!(HighConfig::sanitize_name("+[123aaa"), "[123aaa");
    }
}
