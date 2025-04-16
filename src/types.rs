use anyhow::anyhow;
use bitflags::bitflags;
use compact_str::{
    format_compact, CompactString, CompactStringExt, ToCompactString,
};
use core::{fmt, str};
use enum_dispatch::enum_dispatch;
use hashers::fx_hash::FxHasher;
use indexmap::{map::IntoIter as IndexMapIntoIter, IndexMap};
use log::{info, trace, warn};
use serde::{Deserialize, Deserializer, Serialize};
use smallvec::SmallVec;
use std::cell::{Ref, RefCell, RefMut};
use std::convert::{Into, TryInto};
use std::default::Default;
use std::fmt::Debug;
use std::hash::{BuildHasherDefault, Hash, Hasher};
use std::iter::{Iterator, Step};
use std::ops::{Deref, Index, IndexMut, RangeInclusive};
use std::rc::{Rc, Weak};
use std::str::{Chars, FromStr};
use std::{io, vec};
use unicode_properties::{GeneralCategoryGroup, UnicodeGeneralCategory};

use crate::config::{Category, LexerAction, LexerCatCodeKind, LexerType};
use crate::range::{
    parse_num, parse_num_range, try_get_char, MinMaxValue, NumberSpan,
};
use crate::unicode::{
    get_char_range_from_block_name as get_from_block,
    get_cjk_ideographs_blocks,
};

#[derive(Debug, PartialEq)]
#[non_exhaustive]
pub enum ErrorKind {
    InvalidInput,
    EmptyInput,
    InvalidNumber,
    CharFromNumber,
    InvalidString,
    InvalidPath,
    InvalidFileContent,
    InvalidPattern,
    IOError,
    FormatError,
    RegexError,
    RegTExError,
}
impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}
impl From<ErrorKind> for Result<(), ErrorKind> {
    fn from(value: ErrorKind) -> Self {
        Err(value)
    }
}
impl From<io::Error> for ErrorKind {
    fn from(_: io::Error) -> Self {
        ErrorKind::IOError
    }
}
impl From<fmt::Error> for ErrorKind {
    fn from(_: fmt::Error) -> Self {
        ErrorKind::FormatError
    }
}

#[derive(Debug)]
pub struct CTab {
    chars: Vec<NumberSpan<char>>,
    catcodes: Vec<CatCode>,
    escape: u32,
    endline: u32,
}

impl CTab {
    pub fn new() -> Self {
        CTab {
            chars: Vec::new(),
            catcodes: Vec::new(),
            escape: '\\' as u32,
            endline: '\r' as u32,
        }
    }
    pub fn is_empty(&self) -> bool {
        self.catcodes.is_empty()
    }
    pub fn len(&self) -> usize {
        self.catcodes.len()
    }

    pub fn others() -> Self {
        let mut ctab = CTab::new();
        ctab.emplace_item(char::MIN ..= char::MAX, CatCode::Other);
        ctab.endline = u32::MAX;
        ctab
    }
    pub fn strs() -> Self {
        let mut ctab = CTab::new();
        ctab.emplace_item(char::MIN ..= '\x1F', CatCode::Other);
        ctab.emplace_item('\x20', CatCode::Space);
        ctab.emplace_item('\x21' ..= char::MAX, CatCode::Other);
        ctab.endline = u32::MAX;
        ctab
    }
    pub fn initex() -> Self {
        let mut ctab = CTab::new();
        ctab.emplace_item(13u8 as char, CatCode::EndLine); // <return>
        ctab.emplace_item(32u8 as char, CatCode::Space); // <space>
        ctab.emplace_item(0u8 as char, CatCode::Ignored); // <null>
        ctab.emplace_item(127u8 as char, CatCode::Invalid); // <del>
        ctab.emplace_item('A' ..= 'Z', CatCode::Letter);
        ctab.emplace_item('a' ..= 'z', CatCode::Letter);
        ctab.emplace_item('%', CatCode::Comment);
        ctab.emplace_item('\\', CatCode::Escape);
        ctab
    }
    pub fn document() -> Self {
        let mut ctab = CTab::initex();
        ctab.emplace_item('{', CatCode::BeginGroup);
        ctab.emplace_item('}', CatCode::EndGroup);
        ctab.emplace_item('$', CatCode::MathShift);
        ctab.emplace_item('&', CatCode::Alignment);
        ctab.emplace_item('#', CatCode::Parameter);
        ctab.emplace_item('^', CatCode::Superscript);
        ctab.emplace_item('_', CatCode::Subscript);
        ctab.emplace_item('~', CatCode::Active);
        ctab
    }
    pub fn package() -> Self {
        let mut ctab = CTab::document();
        ctab.emplace_item('@', CatCode::Letter);
        ctab
    }
    pub fn latex3() -> Self {
        let mut ctab = CTab::document();
        ctab.emplace_item(' ', CatCode::Ignored);
        ctab.emplace_item('_', CatCode::Letter);
        ctab.emplace_item(':', CatCode::Letter);
        ctab.endline = 32;
        ctab
    }
    pub fn latex3package() -> Self {
        let mut ctab = CTab::document();
        ctab.emplace_item('@', CatCode::Letter);
        ctab.emplace_item(' ', CatCode::Ignored);
        ctab.emplace_item('_', CatCode::Letter);
        ctab.emplace_item(':', CatCode::Letter);
        ctab.endline = 32;
        ctab
    }
    pub fn cjk_ideographs(catcode: CatCode) -> Self {
        const CJK_LEN: usize = get_cjk_ideographs_blocks().len();
        let mut chars: Vec<NumberSpan<char>> = Vec::with_capacity(CJK_LEN);
        let catcodes = Vec::from_iter([catcode; CJK_LEN]);
        unsafe {
            for &block in get_cjk_ideographs_blocks() {
                let start = char::from_u32_unchecked(block.start());
                let end = char::from_u32_unchecked(block.end());
                chars.push((start ..= end).into());
            }
        }
        chars.sort_by_key(|ns| ns.start()); // may not be sorted!
        Self { chars, catcodes, escape: '\\' as u32, endline: '\n' as u32 }
    }

    pub fn binary_search(&self, item: char) -> Result<usize, usize> {
        self.chars.binary_search_by(|v| v.cmp(&item))
    }
    pub fn get(&self, item: char) -> Option<CatCode> {
        match self.binary_search(item) {
            Ok(v) => Some(self.catcodes[v]),
            Err(_) => None,
        }
    }
    pub fn set(&mut self, item: char, catcode: CatCode) {
        self.emplace_item(NumberSpan::from(item), catcode);
    }
    pub fn remove(&mut self, item: char) -> Option<CatCode> {
        match self.binary_search(item) {
            Ok(index) => unsafe {
                let (start, end) = {
                    let s = self.chars.get_unchecked(index);
                    (s.start(), s.end())
                };
                if start == end {
                    self.chars.remove(index);
                    Some(self.catcodes.remove(index))
                } else {
                    if item == start {
                        self.chars[index] =
                            (Step::forward_unchecked(start, 1) ..= end).into();
                    } else if item == end {
                        self.chars[index] = (start
                            ..= Step::backward_unchecked(end, 1))
                            .into();
                    } else {
                        self.catcodes.insert(index, self.catcodes[index]);
                        self.chars[index] =
                            (Step::forward_unchecked(item, 1) ..= end).into();
                        self.chars.insert(
                            index,
                            (start ..= Step::backward_unchecked(item, 1))
                                .into(),
                        );
                    }
                    Some(*self.catcodes.get_unchecked(index))
                }
            },
            Err(_) => None,
        }
    }
    pub fn get_escape_char(&self) -> Option<char> {
        unsafe {
            if self.escape > char::MAX as u32 {
                None
            } else {
                Some(char::from_u32_unchecked(self.escape))
            }
        }
    }
    /// Set escape char.
    /// Value must be a valid char,
    /// or value > char::MAX, this implies empty escape char.
    pub fn set_escape_char(&mut self, value: u32) -> bool {
        if char::from_u32(value).is_some() {
            self.escape = value;
            true
        } else if value > char::MAX as u32 {
            self.escape = u32::MAX;
            true
        } else {
            false
        }
    }
    pub fn get_endline_char(&self) -> Option<char> {
        unsafe {
            if self.endline > char::MAX as u32 {
                None
            } else {
                Some(char::from_u32_unchecked(self.endline))
            }
        }
    }
    /// Set endline char.
    /// Value must be a valid char,
    /// or value > char::MAX, this implies empty endline char.
    pub fn set_endline_char(&mut self, value: u32) -> bool {
        if char::from_u32(value).is_some() {
            self.endline = value;
            true
        } else if value > char::MAX as u32 {
            self.endline = u32::MAX;
            true
        } else {
            false
        }
    }
    pub fn emplace_item<T: Into<NumberSpan<char>>>(
        &mut self,
        item: T,
        catcode: CatCode,
    ) {
        let span_item: NumberSpan<char> = item.into();

        if self.is_empty() {
            self.chars.push(span_item);
            self.catcodes.push(catcode);
            return;
        }

        let item_start = span_item.start();
        let item_end = span_item.end();
        match self.binary_search(item_start) {
            Ok(start) => {
                match self.binary_search(span_item.end()) {
                    Ok(end) => {
                        if start == end {
                            if self.catcodes[start] == catcode {
                                // nothing need to do
                            } else {
                                if item_start == item_end {
                                    let (_, added) = self.split_at(item_start);
                                    self.catcodes[start + added as usize] =
                                        catcode;
                                } else {
                                    let index = start
                                        + self.split_before(item_start).1
                                            as usize;
                                    self.split_after(item_end); // split_after.1 === 0
                                    self.chars[index] = span_item;
                                    self.catcodes[index] = catcode;
                                }
                            }
                        } else {
                            let index = start
                                + self.split_before(item_start).1 as usize;
                            self.split_after(item_end);
                            self.chars[index] = span_item;
                            self.catcodes[index] = catcode;
                        }
                    }
                    Err(_) => {
                        let (_, added) = self.split_at(item_start);
                        self.chars[start + added as usize] = span_item;
                    }
                }
            }
            Err(start) => {
                self.chars.insert(start, span_item);
                self.catcodes.insert(start, catcode);
            }
        }
        self._normalize();
    }

    /// Split CTab item at char, return (new_len - old_len, new_index - old_index)
    fn split_at(&mut self, at: char) -> (u8, u8) {
        match self.binary_search(at) {
            Ok(index) => {
                let iv_start = self.chars[index].start();
                let iv_end = self.chars[index].end();
                if iv_start < iv_end {
                    if at == iv_start {
                        self.catcodes.insert(index, self.catcodes[index]);
                        unsafe {
                            // iv_start < iv_end, then iv_start < char::MAX, can be forward safely
                            self.chars[index] =
                                (Step::forward_unchecked(at, 1) ..= iv_end)
                                    .into();
                        }
                        self.chars.insert(index, at.into());
                        return (1, 0);
                    } else if at == iv_end {
                        self.catcodes.insert(index, self.catcodes[index]);
                        self.chars[index] = at.into();
                        unsafe {
                            // iv_end > char::MIN, can be backward safely
                            self.chars.insert(
                                index,
                                (iv_start ..= Step::backward_unchecked(at, 1))
                                    .into(),
                            );
                        }
                        return (1, 1);
                    } else {
                        self.catcodes.insert(index, self.catcodes[index]);
                        self.catcodes.insert(index, self.catcodes[index]);
                        unsafe {
                            // iv_start < at < iv_end, at can be backward and forward safely
                            self.chars[index] =
                                (Step::forward_unchecked(at, 1) ..= iv_end)
                                    .into();
                            self.chars.insert(index, at.into());
                            self.chars.insert(
                                index,
                                (iv_start ..= Step::backward_unchecked(at, 1))
                                    .into(),
                            );
                        }
                        return (2, 1);
                    }
                }
            }
            _ => {}
        }
        return (0, 0);
    }
    /// Split CTab item before char, return (new_len - old_len, new_index - old_index)
    fn split_before(&mut self, at: char) -> (u8, u8) {
        match self.binary_search(at) {
            Ok(index) => {
                let iv_start = self.chars[index].start();
                if iv_start < at {
                    self.catcodes.insert(index, self.catcodes[index]);
                    self.chars[index] =
                        (at ..= self.chars[index].end()).into();
                    unsafe {
                        self.chars.insert(
                            index,
                            (iv_start ..= Step::backward_unchecked(at, 1))
                                .into(),
                        );
                    }
                    return (1, 1);
                }
            }
            _ => {}
        }
        return (0, 0);
    }
    /// Split CTab item at char, return (new_len - old_len, new_index - old_index)
    fn split_after(&mut self, at: char) -> (u8, u8) {
        match self.binary_search(at) {
            Ok(index) => {
                let iv_start = self.chars[index].start();
                let iv_end = self.chars[index].end();
                if at < iv_end {
                    self.catcodes.insert(index, self.catcodes[index]);
                    unsafe {
                        self.chars[index] =
                            (Step::forward_unchecked(at, 1) ..= iv_end).into();
                    }
                    self.chars.insert(index, (iv_start ..= at).into());
                    return (1, 0);
                }
            }
            _ => {}
        }
        return (0, 0);
    }
    #[allow(unused)]
    /// Remove the head of a item, including 'at', return len_new < len_old, iff at==item.end
    fn item_trim_before(&mut self, at: char) -> bool {
        match self.binary_search(at) {
            Ok(index) => {
                let iv_end = self.chars[index].end();
                if at == iv_end {
                    self.chars.remove(index);
                    self.catcodes.remove(index);
                    return true;
                } else {
                    unsafe {
                        self.chars[index] =
                            (Step::forward_unchecked(at, 1) ..= iv_end).into();
                    }
                    return false;
                }
            }
            _ => {}
        }
        return false;
    }

    fn _normalize(&mut self) {
        // we have chars[i].start <= chars[j].start, for all i < j < len
        let mut index = 0usize;
        let mut next_index = 0usize;
        let data_len = self.len();
        'o: loop {
            next_index += 1;
            if next_index >= data_len {
                index += 1;
                break 'o;
            }
            let curr_end = self.chars[index].end();
            while !(curr_end <= self.chars[next_index].start()
                || (curr_end >= self.chars[next_index].start()
                    && curr_end < self.chars[next_index].end()))
            {
                next_index += 1;
                if next_index >= data_len {
                    index += 1;
                    break 'o;
                }
            }
            // we have chars[next_index].start <= chars[index].end < chars[next_index].end
            if self.catcodes[index] == self.catcodes[next_index] {
                if curr_end >= self.chars[next_index].start() {
                    self.chars[index] = (self.chars[index].start()
                        ..= self.chars[next_index].end())
                        .into();
                } else {
                    let curr_end_forward =
                        unsafe { Step::forward_unchecked(curr_end, 1) };
                    if curr_end_forward == self.chars[next_index].start() {
                        self.chars[index] = (self.chars[index].start()
                            ..= self.chars[next_index].end())
                            .into();
                    } else {
                        index += 1;
                        self.chars[index] = self.chars[next_index].clone();
                        // self.catcodes[index] = self.catcodes[next_index]; // no need
                    }
                }
            } else {
                let curr_start = unsafe {
                    Step::forward_unchecked(self.chars[index].end(), 1)
                };
                index += 1;
                if curr_end >= self.chars[next_index].start() {
                    self.chars[next_index] =
                        (curr_start ..= self.chars[next_index].end()).into();
                }
                if index < next_index {
                    // index < next_index < len
                    self.chars[index] = self.chars[next_index].clone();
                    self.catcodes[index] = self.catcodes[next_index];
                }
            }
        }
        self.chars.truncate(index);
        self.catcodes.truncate(index);
    }

    pub unsafe fn from_iter_unchecked<I, S>(iter: I) -> Self
    where
        I: IntoIterator<Item = (S, CatCode)>,
        S: Into<NumberSpan<char>>,
    {
        let mut chars = vec![];
        let mut catcodes = vec![];
        for (chr, catcode) in iter {
            chars.push(chr.into());
            catcodes.push(catcode);
        }
        CTab { chars, catcodes, escape: u32::MAX, endline: u32::MAX }
    }
    /// There is `iter`, no `iter_mut`.
    pub fn iter(&self) -> CTabIter {
        CTabIter { next: 0, ctab: self }
    }
}

pub struct CTabIter<'a> {
    next: usize,
    ctab: &'a CTab,
}
impl<'a> Iterator for CTabIter<'a> {
    type Item = (&'a NumberSpan<char>, CatCode);
    fn next(&mut self) -> Option<Self::Item> {
        if self.next < self.ctab.len() {
            let index = self.next;
            self.next += 1;
            Some((&self.ctab.chars[index], self.ctab.catcodes[index]))
        } else {
            None
        }
    }
}

pub struct CTabIntoIter {
    chars: std::vec::IntoIter<NumberSpan<char>>,
    catcodes: std::vec::IntoIter<CatCode>,
}
impl CTabIntoIter {
    fn new(ctab: CTab) -> Self {
        CTabIntoIter {
            chars: ctab.chars.into_iter(),
            catcodes: ctab.catcodes.into_iter(),
        }
    }
}
impl Iterator for CTabIntoIter {
    type Item = (NumberSpan<char>, CatCode);
    fn next(&mut self) -> Option<Self::Item> {
        match (self.chars.next(), self.catcodes.next()) {
            (Some(chr), Some(catcode)) => Some((chr, catcode)),
            _ => None,
        }
    }
}
impl IntoIterator for CTab {
    type IntoIter = CTabIntoIter;
    type Item = (NumberSpan<char>, CatCode);
    fn into_iter(self) -> Self::IntoIter {
        CTabIntoIter::new(self)
    }
}

impl Extend<(NumberSpan<char>, CatCode)> for CTab {
    fn extend<T: IntoIterator<Item = (NumberSpan<char>, CatCode)>>(
        &mut self,
        iter: T,
    ) {
        for (item, catcode) in iter {
            self.emplace_item(item, catcode);
        }
    }
}

pub struct CTabSet {
    ctabs: IndexMap<String, Rc<RefCell<CTab>>, BuildHasherDefault<FxHasher>>,
}

impl CTabSet {
    pub fn new_empty() -> CTabSet {
        let ctabs =
            IndexMap::with_hasher(BuildHasherDefault::<FxHasher>::default());
        CTabSet { ctabs }
    }

    pub fn new() -> CTabSet {
        let mut ctabs =
            IndexMap::with_hasher(BuildHasherDefault::<FxHasher>::default());
        let ctab = unsafe {
            CTab::from_iter_unchecked([
                (char::MIN ..= '\x1F', CatCode::Other),
                (' ' ..= ' ', CatCode::Space),
                ('\x21' ..= char::MAX, CatCode::Other),
            ])
        };
        ctabs.insert(String::from("str"), Rc::new(RefCell::new(ctab)));
        CTabSet { ctabs }
    }

    /// if name is in ctabset, return false, or put ctab in ctabset, return true
    pub fn add(&mut self, name: &str, ctab: CTab) -> bool {
        if matches!(name, "str" | "other" | "current")
            || self.ctabs.contains_key(name)
        {
            return false;
        }
        self.ctabs.insert(name.to_string(), Rc::new(RefCell::new(ctab)));
        true
    }

    pub fn put(&mut self, name: &str, ctab: CTab) -> bool {
        if matches!(name, "str" | "other" | "current") {
            return false;
        }
        self.ctabs.insert(name.to_string(), Rc::new(RefCell::new(ctab)));
        true
    }
    /// Put but detect special cases, for special cases, return false.
    /// name = other, ctab = empty, all char = CatCode::Other;
    /// name = CJK, ctab = empty, => ALL CJK IDEOGRAPHS = catcode;
    pub fn put_detect_specials(
        &mut self,
        name: &str,
        mut ctab: CTab,
        catcode: CatCode,
    ) -> bool {
        if matches!(name, "other" | "current") && !ctab.is_empty() {
            return false;
        }
        match name {
            "other" => {
                ctab.emplace_item(char::MIN ..= char::MAX, CatCode::Other);
                ctab.escape = ctab.escape;
                ctab.endline = ctab.endline;
                self.ctabs
                    .insert(name.to_string(), Rc::new(RefCell::new(ctab)));
                false
            }
            "CJK" if ctab.is_empty() => {
                let mut ctab = CTab::cjk_ideographs(catcode);
                ctab.escape = ctab.escape;
                ctab.endline = ctab.endline;
                self.ctabs
                    .insert(name.to_string(), Rc::new(RefCell::new(ctab)));
                false
            }
            _ => {
                self.ctabs
                    .insert(name.to_string(), Rc::new(RefCell::new(ctab)));
                true
            }
        }
    }

    // Clone and return the pointer of ctabs[name].
    pub fn clone_of(&self, name: &str) -> Option<Rc<RefCell<CTab>>> {
        match self.ctabs.get(name) {
            Some(value) => Some(Rc::clone(value)),
            None => None,
        }
    }

    // Create and return a weak pointer to the ctabs[name].
    pub fn downgrade_of(&self, name: &str) -> Option<Weak<RefCell<CTab>>> {
        match self.ctabs.get(name) {
            Some(value) => Some(Rc::downgrade(value)),
            None => None,
        }
    }

    // Immutably borrow the ctabs[name].
    pub fn get_by_name(&self, name: &str) -> Option<Ref<CTab>> {
        match self.ctabs.get(name) {
            Some(value) => Some(value.borrow()),
            None => None,
        }
    }

    // Mutably borrow the ctabs[name].
    pub fn get_by_name_mut(&self, name: &str) -> Option<RefMut<CTab>> {
        match self.ctabs.get(name) {
            Some(value) => Some(value.borrow_mut()),
            None => None,
        }
    }

    pub fn get_catcode_value(&self, name: &str, at: char) -> Option<CatCode> {
        self.ctabs.get(name)?.borrow().get(at)
    }

    pub fn get_escape_char(&self, name: &str) -> Option<char> {
        self.ctabs.get(name)?.borrow().get_escape_char()
    }

    pub fn get_endline_char(&self, name: &str) -> Option<char> {
        self.ctabs.get(name)?.borrow().get_endline_char()
    }

    // Return true if an equivalent to key exists in the ctabs.
    pub fn contains_key(&self, name: &str) -> bool {
        self.ctabs.contains_key(name)
    }

    pub fn iter(&self) -> indexmap::map::Iter<'_, String, Rc<RefCell<CTab>>> {
        self.ctabs.iter()
    }
    pub fn iter_mut(
        &mut self,
    ) -> indexmap::map::IterMut<'_, String, Rc<RefCell<CTab>>> {
        self.ctabs.iter_mut()
    }

    // TODO: to be clean
    fn _parse(s: &str) -> Result<Self, ErrorKind> {
        fn set_e_e_char(
            key_str: &str,
            val_str: &str,
            ctab: &mut CTab,
            ignored_lines: &mut usize,
        ) {
            match parse_num(val_str) {
                Ok(num) => {
                    let chr_o = try_get_char(num);
                    let chr = if chr_o.is_some() {
                        chr_o.unwrap() as u32
                    } else if num < 0 {
                        u32::MAX - 1
                    } else {
                        u32::MAX
                    };
                    if chr != u32::MAX {
                        let key_str = key_str.to_ascii_lowercase();
                        if matches!(key_str.as_str(), "escapechar") {
                            ctab.set_escape_char(chr);
                        } else if matches!(key_str.as_str(), "endlinechar") {
                            ctab.set_endline_char(chr);
                        } else {
                            *ignored_lines += 1;
                        }
                    } else {
                        *ignored_lines += 1;
                    }
                }
                Err(_) => {
                    *ignored_lines += 1;
                }
            }
        }
        let mut ctabset = CTabSet::new();
        let mut chars = s.chars();
        let mut char_next = None;
        let mut ctab = CTab::new();

        let mut in_name = false;
        let mut in_sep = false;
        let mut in_key = false;
        let mut in_value = false;
        let mut newline = true;
        let mut escape = false;

        let mut ignored_lines = 0usize;
        let mut key_o: Option<NumberSpan<char>> = None;
        let mut name = CompactString::new("");
        let mut key_str = CompactString::new("");
        let mut val_str = CompactString::new("");

        loop {
            let c = if char_next.is_some() {
                (char_next.unwrap(), {
                    char_next = None;
                })
                    .0
            } else {
                match chars.next() {
                    Some(c) => c,
                    None => break,
                }
            };
            match c {
                '[' => {
                    if escape && in_key {
                        key_str.push(c);
                        escape = false;
                    } else if newline {
                        // if key_o.is_some() && !val_str.is_empty() {
                        //     match CatCode::try_from(&val_str) {
                        //         Ok(v) => {
                        //             ctab.emplace_item(key_o.unwrap(), v);
                        //         }
                        //         Err(_) => {
                        //             ignored_lines += 1;
                        //         }
                        //     }
                        // }
                        // key_o = None;
                        // key_str.clear();
                        // val_str.clear();
                        if !name.is_empty() {
                            ctabset.put(&name.trim_ascii_end(), ctab);
                            name.clear();
                            ctab = CTab::new();
                        }
                        newline = false;
                        in_name = true;
                    } else if in_key {
                        if key_str.is_empty() && !name.is_empty() {
                            ctabset.put(&name.trim_ascii_end(), ctab);
                            name.clear();
                            ctab = CTab::new();
                            in_name = true;
                            in_key = false;
                        } else if in_name {
                            ignored_lines += 1;
                        } else {
                            key_str.push(c);
                        }
                    } else if in_value {
                        val_str.push(c);
                    }
                }
                ']' => {
                    if in_name {
                        newline = false;
                        in_name = false;
                        in_key = true;
                    } else if in_key {
                        if key_str.is_empty() {
                            ignored_lines += 1;
                        } else {
                            if escape {
                                escape = false;
                            }
                            key_str.push(c);
                        }
                    } else if in_value {
                        val_str.push(c);
                    }
                }
                '\\' => {
                    if in_key {
                        if key_str.len() >= 1 {
                            let prev_char =
                                key_str.as_bytes()[key_str.len() - 1];
                            if prev_char == '`' as u8
                                || prev_char == '\\' as u8
                            {
                                key_str.push(c);
                                if escape {
                                    escape = false;
                                } else {
                                    escape = true;
                                }
                            }
                        } else {
                            key_str.push(c);
                            escape = !escape;
                        }
                    } else if in_name {
                        name.push(c);
                    } else if in_value {
                        val_str.push(c);
                    }
                }
                '=' => {
                    if in_name {
                        name.push(c);
                    } else if in_key {
                        if key_str.is_empty() || escape {
                            key_str.push(c);
                            escape = false;
                        } else {
                            in_key = false;
                            in_sep = false;
                            in_value = true;
                        }
                    } else if in_sep {
                        in_sep = false;
                        in_value = true;
                    } else if in_value {
                        val_str.push(c);
                    }
                }
                '/' => {
                    let take_len = find_comment_len(chars.clone());
                    if take_len > 0 {
                        chars.advance_by(take_len).unwrap();
                        char_next = Some(' ');
                    } else {
                        if in_name {
                            name.push(c);
                        } else if in_key {
                            key_str.push(c);
                            if escape {
                                escape = false;
                            }
                        } else if in_value {
                            val_str.push(c);
                        }
                    }
                }
                _ if c.is_ascii_whitespace() => {
                    if in_name {
                        if !name.is_empty() {
                            name.push(' ');
                        }
                    } else if in_key {
                        if !key_str.is_empty() {
                            let prev_char =
                                key_str.as_bytes()[key_str.len() - 1];
                            if prev_char == '`' as u8
                                || (prev_char == '\\' as u8 && escape)
                            {
                                key_str.push(c);
                                if escape {
                                    escape = false;
                                }
                            } else {
                                in_key = false;
                                in_sep = true;
                            }
                        }
                    } else if in_sep {
                    } else if in_value {
                        if !val_str.is_empty() {
                            if &val_str == "`" || &val_str == "`\\" {
                                val_str.push(' ');
                            } else {
                                key_o = match parse_num_range(&key_str, "..") {
                                    Ok(v) => v.try_into().ok(),
                                    Err(_) => match get_from_block(&key_str) {
                                        Some(v) => Some(v),
                                        None => None,
                                    },
                                };
                                if key_o.is_some() {
                                    match CatCode::try_from(&val_str) {
                                        Ok(v) => {
                                            ctab.emplace_item(
                                                key_o.unwrap(),
                                                v,
                                            );
                                        }
                                        Err(_) => {
                                            ignored_lines += 1;
                                        }
                                    }
                                } else {
                                    set_e_e_char(
                                        &key_str,
                                        &val_str,
                                        &mut ctab,
                                        &mut ignored_lines,
                                    );
                                }
                                key_o = None;
                                key_str.clear();
                                val_str.clear();
                                in_value = false;
                                in_key = true;
                                newline = true;
                            }
                        }
                    }
                }
                _ => {
                    if in_name {
                        name.push(c);
                    } else if in_key {
                        key_str.push(c);
                        if escape {
                            escape = false;
                        }
                    } else if in_value {
                        val_str.push(c);
                    } else {
                        ignored_lines += 1;
                    }
                }
            }
        }

        if !key_str.is_empty() {
            key_o = match parse_num_range(&key_str, "..") {
                Ok(v) => v.try_into().ok(),
                Err(_) => match get_from_block(&key_str) {
                    Some(v) => Some(v),
                    None => None,
                },
            };
        }
        if key_o.is_some() {
            match CatCode::try_from(&val_str) {
                Ok(v) => {
                    ctab.emplace_item(key_o.unwrap(), v);
                }
                Err(_) => {
                    ignored_lines += 1;
                }
            }
        } else {
            if !key_str.is_empty() {
                set_e_e_char(
                    &key_str,
                    &val_str,
                    &mut ctab,
                    &mut ignored_lines,
                );
            }
        }

        if !name.is_empty() {
            ctabset.put(&name.trim_ascii_end(), ctab);
        }

        if in_name {
            ignored_lines += 1;
        }
        if ignored_lines > 0 {
            warn!(target: "Parsing CTabSet", "Ignoring {} line(s).", ignored_lines);
        } else {
            info!(target: "Parsing CTabSet", "Ignoring {} line(s).", ignored_lines);
        }
        Ok(ctabset)
    }
}

impl Default for CTabSet {
    fn default() -> Self {
        Self::new_empty()
    }
}

pub struct CTabSetIntoIter {
    ctabset_iter: IndexMapIntoIter<String, Rc<RefCell<CTab>>>,
}
impl CTabSetIntoIter {
    pub fn new(ctabset: CTabSet) -> Self {
        CTabSetIntoIter { ctabset_iter: ctabset.ctabs.into_iter() }
    }
}
impl Iterator for CTabSetIntoIter {
    type Item = (String, CTab);
    fn next(&mut self) -> Option<Self::Item> {
        match self.ctabset_iter.next() {
            Some((name, ctab)) => {
                Some((name, Rc::into_inner(ctab).unwrap().into_inner()))
            }
            None => None,
        }
    }
}

impl IntoIterator for CTabSet {
    type IntoIter = CTabSetIntoIter;
    type Item = (String, CTab);
    fn into_iter(self) -> Self::IntoIter {
        CTabSetIntoIter::new(self)
    }
}

impl Extend<(String, CTab)> for CTabSet {
    fn extend<T: IntoIterator<Item = (String, CTab)>>(&mut self, iter: T) {
        for (name, ctab) in iter.into_iter() {
            self.ctabs.insert(name, Rc::new(RefCell::new(ctab)));
        }
    }
}

impl FromStr for CTabSet {
    type Err = ErrorKind;

    /// the ctabset format:
    /// ```txt
    /// [ctabname]
    /// <char num range> = catcode
    /// <char num range> = <char num>..<char num> | <char num> | <block name>
    /// <char num> := `<char> | `\<char> | '0o'[0-7]+ | '0x'[0-F]+ | [0-9]+
    /// <block name> := UNICODE BLOCK NAME
    /// 0b100 = 12
    /// 0x20 = 10
    /// 032 = 10
    /// `a = 11
    /// `\\ = 0
    /// CJK_UNIFIED_IDEOGRAPHS = 11
    /// ```
    /// Comment: /* comment, can be /* nested */, will be converted to a space */
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        CTabSet::_parse(s)
    }
}

fn find_comment_len(mut chars: Chars) -> usize {
    let (mut depth, mut len) = match chars.next() {
        Some(first) if first == '*' => (1, 1),
        _ => {
            return 0;
        }
    };
    while let Some(c) = chars.next() {
        len += 1;
        if c == '/' {
            if let Some(nc) = chars.next() {
                len += 1;
                if nc == '*' {
                    depth += 1;
                }
            } else {
                break;
            }
        } else if c == '*' {
            if let Some(nc) = chars.next() {
                len += 1;
                if nc == '/' {
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
            } else {
                break;
            }
        }
    }
    len
}

bitflags! {
    #[allow(non_upper_case_globals)]
    #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
    pub struct CatCodeSet: u16 {
        const Escape = 0x0001;
        const BeginGroup = 0x0002;
        const EndGroup = 0x0004;
        const MathShift = 0x0008;
        const Alignment = 0x0010;
        const EndLine = 0x0020;
        const Parameter = 0x0040;
        const Superscript = 0x0080;
        const Subscript = 0x0100;
        const Ignored = 0x0200;
        const Space = 0x0400;
        const Letter = 0x0800;
        const Other = 0x1000;
        const Active = 0x2000;
        const Comment = 0x4000;
        const Invalid = 0x8000;
    }
}
impl CatCodeSet {
    pub fn new() -> Self {
        CatCodeSet::empty()
    }
    pub fn new_with_catcode(catcode: CatCode) -> Self {
        catcode.into()
    }
    pub fn insert_catcode(&mut self, catcode: CatCode) {
        self.insert(catcode.into());
    }
    pub fn insert_many(&mut self, catcodes: impl Into<CatCodeSet>) {
        self.insert(catcodes.into());
    }
    pub fn remove_catcode(&mut self, catcode: CatCode) -> CatCode {
        self.remove(catcode.into());
        catcode
    }
    pub fn remove_many(&mut self, catcodes: impl Into<CatCodeSet>) {
        self.remove(catcodes.into());
    }
    pub fn set_catcode(&mut self, catcode: CatCode, value: bool) {
        self.set(catcode.into(), value);
    }
    pub fn contains_catcode(&self, catcode: CatCode) -> bool {
        self.contains(catcode.into())
    }
    /// Return the minimum CatCode of the set, if present.
    ///
    /// Suppose `set = [2,3,4,5,9,11,12,13]`, then `first()` will return `2`.
    pub fn first(&self) -> Option<CatCode> {
        if self.bits().count_ones() > 0 {
            let start = self.bits().trailing_zeros();
            Some(unsafe { CatCode::from_u8_unchecked(start as u8) })
        } else {
            None
        }
    }
    /// Return a RangeInclusive starting from the minimum value.
    ///
    /// Suppose `set = [2,3,4,5,9,11,12,13]`, then `first_range()` will return `2..=5`.
    pub fn first_range(&self) -> Option<RangeInclusive<CatCode>> {
        if self.bits().count_ones() > 0 {
            let start = self.bits().trailing_zeros();
            let len = (self.bits() >> start).trailing_ones(); // >= 1
            unsafe {
                let cat_start = CatCode::from_u8_unchecked(start as u8);
                let cat_end =
                    CatCode::from_u8_unchecked((start + len - 1) as u8);
                Some(cat_start ..= cat_end)
            }
        } else {
            None
        }
    }
    /// Return the maximum CatCode of the set, if present.
    ///
    /// Suppose `set = [2,3,4,5,9,11,12,13]`, then `last()` will return `13`.
    pub fn last(&self) -> Option<CatCode> {
        if self.bits().count_ones() >= 1 {
            let end = 15 - self.bits().leading_zeros();
            Some(unsafe { CatCode::from_u8_unchecked(end as u8) })
        } else {
            None
        }
    }
    /// Return a RangeInclusive ending with the maximum value.
    ///
    /// Suppose `set = [2,3,4,5,9,11,12,13]`, then `last()` will return `11..=13`.
    pub fn last_range(&self) -> Option<RangeInclusive<CatCode>> {
        if self.bits().count_ones() > 0 {
            let lz = self.bits().leading_zeros();
            let end = 15 - lz;
            let len = (self.bits() << lz).leading_ones();
            unsafe {
                let cat_start =
                    CatCode::from_u8_unchecked((end - len + 1) as u8);
                let cat_end = CatCode::from_u8_unchecked(end as u8);
                Some(cat_start ..= cat_end)
            }
        } else {
            None
        }
    }
    pub fn len(&self) -> usize {
        self.bits().count_ones() as usize
    }
    pub fn is_full(&self) -> bool {
        self.bits() == u16::MAX
    }
    pub fn into_catcode_iter(self) -> CatCodeSetIter {
        CatCodeSetIter(self)
    }
    pub fn into_catcode_range_iter(self) -> CatCodeSetRangeIter {
        CatCodeSetRangeIter(self)
    }
}
impl TryInto<CatCode> for CatCodeSet {
    type Error = anyhow::Error;
    fn try_into(self) -> Result<CatCode, Self::Error> {
        match self.bits().count_ones() {
            0 => Err(anyhow::anyhow!("None of CatCode is set")),
            1 => {
                let high_bit = 15 - self.bits().leading_zeros();
                Ok(unsafe { CatCode::from_u8_unchecked(high_bit as u8) })
            }
            _ => Err(anyhow::anyhow!("More than one CatCode are set")),
        }
    }
}

pub struct CatCodeSetIter(CatCodeSet);
impl Iterator for CatCodeSetIter {
    type Item = CatCode;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.first().and_then(|v| Some(self.0.remove_catcode(v)))
    }
}
impl DoubleEndedIterator for CatCodeSetIter {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.last().and_then(|v| Some(self.0.remove_catcode(v)))
    }
}

pub struct CatCodeSetRangeIter(CatCodeSet);
impl Iterator for CatCodeSetRangeIter {
    type Item = RangeInclusive<CatCode>;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(rng) = self.0.first_range() {
            let zero_len = *rng.end() as u16 + 1;
            if zero_len > 15 {
                self.0 = CatCodeSet::empty();
            } else {
                self.0 &= CatCodeSet::from_bits_retain(u16::MAX << zero_len);
            }
            Some(rng)
        } else {
            None
        }
    }
}
impl DoubleEndedIterator for CatCodeSetRangeIter {
    fn next_back(&mut self) -> Option<Self::Item> {
        if let Some(rng) = self.0.last_range() {
            let zero_len = 16 - *rng.start() as u16;
            self.0 &= CatCodeSet::from_bits_retain(u16::MAX >> zero_len);
            Some(rng)
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[derive(Serialize)]
#[repr(u8)]
pub enum CatCode {
    Escape,
    BeginGroup,
    EndGroup,
    MathShift,
    Alignment,
    EndLine,
    Parameter,
    Superscript,
    Subscript,
    Ignored,
    Space,
    Letter,
    Other,
    Active,
    Comment,
    Invalid,
}

impl CatCode {
    /// Return the catcode from a `u8`, whose range is `0..=15`.
    pub const unsafe fn from_u8_unchecked(catcode: u8) -> CatCode {
        std::mem::transmute(catcode)
    }
    /// Return the ASCII uppercase expression of a CatCode.
    pub const fn char(&self) -> u8 {
        match self {
            CatCode::Escape => b'X',
            CatCode::BeginGroup => b'B',
            CatCode::EndGroup => b'E',
            CatCode::MathShift => b'M',
            CatCode::Alignment => b'T',
            CatCode::EndLine => b'J',
            CatCode::Parameter => b'P',
            CatCode::Superscript => b'U',
            CatCode::Subscript => b'D',
            CatCode::Ignored => b'I',
            CatCode::Space => b'S',
            CatCode::Letter => b'L',
            CatCode::Other => b'O',
            CatCode::Active => b'A',
            CatCode::Comment => b'C',
            CatCode::Invalid => b'V',
        }
    }
    pub fn from_char(c: char) -> Option<CatCode> {
        match c {
            'X' => Some(CatCode::Escape),
            'B' => Some(CatCode::BeginGroup),
            'E' => Some(CatCode::EndGroup),
            'M' => Some(CatCode::MathShift),
            'T' => Some(CatCode::Alignment),
            'J' => Some(CatCode::EndLine),
            'P' => Some(CatCode::Parameter),
            'U' => Some(CatCode::Superscript),
            'D' => Some(CatCode::Subscript),
            'I' => Some(CatCode::Ignored),
            'S' => Some(CatCode::Space),
            'L' => Some(CatCode::Letter),
            'O' => Some(CatCode::Other),
            'A' => Some(CatCode::Active),
            'C' => Some(CatCode::Comment),
            'V' => Some(CatCode::Invalid),
            _ => None,
        }
    }
    pub fn from_ascii_char(c: u8) -> Option<CatCode> {
        match c {
            b'X' => Some(CatCode::Escape),
            b'B' => Some(CatCode::BeginGroup),
            b'E' => Some(CatCode::EndGroup),
            b'M' => Some(CatCode::MathShift),
            b'T' => Some(CatCode::Alignment),
            b'J' => Some(CatCode::EndLine),
            b'P' => Some(CatCode::Parameter),
            b'U' => Some(CatCode::Superscript),
            b'D' => Some(CatCode::Subscript),
            b'I' => Some(CatCode::Ignored),
            b'S' => Some(CatCode::Space),
            b'L' => Some(CatCode::Letter),
            b'O' => Some(CatCode::Other),
            b'A' => Some(CatCode::Active),
            b'C' => Some(CatCode::Comment),
            b'V' => Some(CatCode::Invalid),
            _ => None,
        }
    }
}

impl Step for CatCode {
    fn steps_between(start: &Self, end: &Self) -> (usize, Option<usize>) {
        let start = (*start) as u8;
        let end = (*end) as u8;
        if start > end {
            (0, None)
        } else {
            let len = (end - start) as usize;
            (len, Some(len))
        }
    }
    fn forward_checked(start: Self, count: usize) -> Option<Self> {
        let start = start as u8;
        if count <= 15 {
            let sum = start + count as u8;
            if sum <= 15 {
                Some(unsafe { std::mem::transmute(sum) })
            } else {
                None
            }
        } else {
            None
        }
    }
    unsafe fn forward_unchecked(start: Self, count: usize) -> Self {
        std::mem::transmute(start as u8 + count as u8)
    }
    fn backward_checked(start: Self, count: usize) -> Option<Self> {
        let start = start as u8;
        if count <= start as u8 as usize {
            let diff = start - count as u8;
            Some(unsafe { std::mem::transmute(diff) })
        } else {
            None
        }
    }
    unsafe fn backward_unchecked(start: Self, count: usize) -> Self {
        std::mem::transmute(start as u8 - count as u8)
    }
}
impl MinMaxValue for CatCode {
    const MIN: Self = CatCode::Escape;
    const MAX: Self = CatCode::Invalid;
}

impl Default for CatCode {
    fn default() -> Self {
        CatCode::Other
    }
}

impl Into<CatCodeSet> for CatCode {
    fn into(self) -> CatCodeSet {
        CatCodeSet::from_bits_retain(1u16 << self as u16)
    }
}
impl<I, T> From<T> for CatCodeSet
where
    T: IntoIterator<Item = CatCode, IntoIter = I>,
    I: Iterator<Item = CatCode>,
{
    fn from(value: T) -> Self {
        let mut cc = CatCodeSet::new();
        for cat in value.into_iter() {
            cc.insert_catcode(cat);
        }
        cc
    }
}

impl TryInto<CatCode> for i64 {
    type Error = ErrorKind;
    fn try_into(self) -> Result<CatCode, Self::Error> {
        match self {
            0 => Ok(CatCode::Escape),
            1 => Ok(CatCode::BeginGroup),
            2 => Ok(CatCode::EndGroup),
            3 => Ok(CatCode::MathShift),
            4 => Ok(CatCode::Alignment),
            5 => Ok(CatCode::EndLine),
            6 => Ok(CatCode::Parameter),
            7 => Ok(CatCode::Superscript),
            8 => Ok(CatCode::Subscript),
            9 => Ok(CatCode::Ignored),
            10 => Ok(CatCode::Space),
            11 => Ok(CatCode::Letter),
            12 => Ok(CatCode::Other),
            13 => Ok(CatCode::Active),
            14 => Ok(CatCode::Comment),
            15 => Ok(CatCode::Invalid),
            _ => Err(ErrorKind::InvalidNumber),
        }
    }
}
impl TryInto<CatCode> for u8 {
    type Error = ErrorKind;
    fn try_into(self) -> Result<CatCode, Self::Error> {
        match self {
            0 => Ok(CatCode::Escape),
            1 => Ok(CatCode::BeginGroup),
            2 => Ok(CatCode::EndGroup),
            3 => Ok(CatCode::MathShift),
            4 => Ok(CatCode::Alignment),
            5 => Ok(CatCode::EndLine),
            6 => Ok(CatCode::Parameter),
            7 => Ok(CatCode::Superscript),
            8 => Ok(CatCode::Subscript),
            9 => Ok(CatCode::Ignored),
            10 => Ok(CatCode::Space),
            11 => Ok(CatCode::Letter),
            12 => Ok(CatCode::Other),
            13 => Ok(CatCode::Active),
            14 => Ok(CatCode::Comment),
            15 => Ok(CatCode::Invalid),
            _ => Err(ErrorKind::InvalidNumber),
        }
    }
}

impl TryFrom<&str> for CatCode {
    type Error = ErrorKind;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if value.is_empty() {
            return Err(ErrorKind::InvalidInput);
        }

        if let Ok(num) = value.parse::<i64>() {
            TryInto::<CatCode>::try_into(num)
        } else {
            if value.len() == 1 {
                let c = *value.as_bytes().first().unwrap();
                if let Some(catcode) = CatCode::from_ascii_char(c) {
                    return Ok(catcode);
                }
            }

            match value.to_ascii_lowercase().as_str() {
                "\\" | "escape" => Ok(CatCode::Escape),
                "{" | "begingroup" => Ok(CatCode::BeginGroup),
                "}" | "endgroup" => Ok(CatCode::EndGroup),
                "$" | "math" | "mathshift" => Ok(CatCode::MathShift),
                "&" | "tab" | "alignment" => Ok(CatCode::Alignment),
                "\n" | "endline" => Ok(CatCode::EndLine),
                "#" | "parameter" => Ok(CatCode::Parameter),
                "^" | "superscript" => Ok(CatCode::Superscript),
                "_" | "subscript" => Ok(CatCode::Subscript),
                "ignored" => Ok(CatCode::Ignored),
                " " | "space" => Ok(CatCode::Space),
                "letter" => Ok(CatCode::Letter),
                "other" => Ok(CatCode::Other),
                "~" | "active" => Ok(CatCode::Active),
                "%" | "comment" => Ok(CatCode::Comment),
                "invalid" => Ok(CatCode::Invalid),
                _ => Err(ErrorKind::InvalidInput),
            }
        }
    }
}

impl TryFrom<&String> for CatCode {
    type Error = ErrorKind;
    fn try_from(value: &String) -> Result<Self, Self::Error> {
        value.as_str().try_into()
    }
}
impl TryFrom<&CompactString> for CatCode {
    type Error = ErrorKind;
    fn try_from(value: &CompactString) -> Result<Self, Self::Error> {
        value.as_str().try_into()
    }
}

impl<'de> Deserialize<'de> for CatCode {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum NumOrString {
            Number(i64),
            String(String),
        }

        let data = NumOrString::deserialize(deserializer)?;
        let catcode = match &data {
            NumOrString::Number(num) => (*num).try_into().map_err(|_| {
                serde::de::Error::custom(format!(
                    "Invalid value for a CatCode: {}, should be at [{}, {}]",
                    num,
                    CatCode::MIN as u8,
                    CatCode::MAX as u8,
                ))
            })?,
            NumOrString::String(s) => CatCode::try_from(s).map_err(|_| {
                serde::de::Error::custom(format!(
                    "Invalid CatCode name: {}",
                    s
                ))
            })?,
        };

        Ok(catcode)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Position<'a> {
    pub row: u32,
    pub col: u32,
    pub tl: Option<&'a [Token]>,
    pub bytes: Option<&'a [u8]>,
    pub source: Option<&'a str>,
}

#[enum_dispatch]
pub trait CatCodeGetter {
    fn catcode_value(&self, at: char) -> Option<CatCode>;
    fn escape_char(&self) -> Option<char>;
    fn endline_char(&self) -> Option<char>;
}
impl<C: CatCodeGetter, T: Deref<Target = C>> CatCodeGetter for T {
    fn catcode_value(&self, at: char) -> Option<CatCode> {
        self.deref().catcode_value(at)
    }
    fn escape_char(&self) -> Option<char> {
        self.deref().escape_char()
    }
    fn endline_char(&self) -> Option<char> {
        self.deref().endline_char()
    }
}

pub struct CTabFallbackable<T1: CatCodeGetter, T2: CatCodeGetter> {
    ctab: T1,
    fallback: Option<T2>,
}

impl<T1: CatCodeGetter, T2: CatCodeGetter> CTabFallbackable<T1, T2> {
    pub fn new(ctab: T1, fallback: Option<T2>) -> Self {
        CTabFallbackable { ctab, fallback }
    }
    /// Return fallback.
    pub fn pop(self) -> Option<T2> {
        self.fallback
    }
}

impl CatCodeGetter for Character {
    fn catcode_value(&self, at: char) -> Option<CatCode> {
        if self.charcode == at {
            Some(self.catcode)
        } else {
            None
        }
    }
    fn escape_char(&self) -> Option<char> {
        None
    }
    fn endline_char(&self) -> Option<char> {
        None
    }
}
impl CatCodeGetter for CTab {
    fn catcode_value(&self, at: char) -> Option<CatCode> {
        self.get(at)
    }
    fn escape_char(&self) -> Option<char> {
        self.get_escape_char()
    }
    fn endline_char(&self) -> Option<char> {
        self.get_endline_char()
    }
}
impl CatCodeGetter for [(char, CatCode)] {
    fn catcode_value(&self, at: char) -> Option<CatCode> {
        self.iter().find(|v| v.0 == at).and_then(|v| Some(v.1))
    }
    fn endline_char(&self) -> Option<char> {
        None
    }
    fn escape_char(&self) -> Option<char> {
        None
    }
}
impl<T1: CatCodeGetter, T2: CatCodeGetter> CatCodeGetter
    for CTabFallbackable<T1, T2>
{
    fn catcode_value(&self, at: char) -> Option<CatCode> {
        match self.ctab.catcode_value(at) {
            Some(value) => Some(value),
            None => self.fallback.as_ref().or(None)?.catcode_value(at),
            // None => match &self.fallback {
            //     Some(fallback) => fallback.catcode_value(at),
            //     None => None,
            // },
        }
    }
    fn escape_char(&self) -> Option<char> {
        self.ctab.escape_char()
    }
    fn endline_char(&self) -> Option<char> {
        self.ctab.endline_char()
    }
}

pub enum CatCodeStackItem<'a> {
    Char(Character),
    CharVec(&'a [(char, CatCode)]),
    CTab(CTab),
    CTabRef(Ref<'a, CTab>),
    CTabRefMut(RefMut<'a, CTab>),
    EndLine(u32),
    Stack(&'a CatCodeStack<'a>),
}
impl CatCodeGetter for CatCodeStackItem<'_> {
    fn catcode_value(&self, at: char) -> Option<CatCode> {
        match self {
            Self::Char(c) => (c.charcode == at).then_some(c.catcode),
            Self::CharVec(chars) => chars.catcode_value(at),
            Self::CTab(ctab) => ctab.catcode_value(at),
            Self::CTabRef(ctab) => ctab.catcode_value(at),
            Self::CTabRefMut(ctab) => ctab.catcode_value(at),
            Self::EndLine(_) => None,
            Self::Stack(s) => s.catcode_value(at),
        }
    }

    fn endline_char(&self) -> Option<char> {
        match self {
            Self::Char(_) | Self::CharVec(_) => None,
            Self::CTab(ctab) => ctab.endline_char(),
            Self::CTabRef(ctab) => ctab.endline_char(),
            Self::CTabRefMut(ctab) => ctab.endline_char(),
            Self::EndLine(endline) => char::from_u32(*endline),
            Self::Stack(s) => s.endline_char(),
        }
    }

    fn escape_char(&self) -> Option<char> {
        match self {
            Self::Char(_) | Self::CharVec(_) => None,
            Self::CTab(ctab) => ctab.escape_char(),
            Self::CTabRef(ctab) => ctab.escape_char(),
            Self::CTabRefMut(ctab) => ctab.escape_char(),
            Self::EndLine(_) => None,
            Self::Stack(s) => s.escape_char(),
        }
    }
}

impl Into<CatCodeStackItem<'_>> for Character {
    fn into(self) -> CatCodeStackItem<'static> {
        CatCodeStackItem::Char(self)
    }
}
impl<'a> Into<CatCodeStackItem<'a>> for &'a [(char, CatCode)] {
    fn into(self) -> CatCodeStackItem<'a> {
        CatCodeStackItem::CharVec(self)
    }
}
impl<'a> Into<CatCodeStackItem<'a>> for CTab {
    fn into(self) -> CatCodeStackItem<'static> {
        CatCodeStackItem::CTab(self)
    }
}
impl<'a> Into<CatCodeStackItem<'a>> for Ref<'a, CTab> {
    fn into(self) -> CatCodeStackItem<'a> {
        CatCodeStackItem::CTabRef(self)
    }
}
impl<'a> Into<CatCodeStackItem<'a>> for RefMut<'a, CTab> {
    fn into(self) -> CatCodeStackItem<'a> {
        CatCodeStackItem::CTabRefMut(self)
    }
}
impl<'a> Into<CatCodeStackItem<'a>> for u32 {
    fn into(self) -> CatCodeStackItem<'a> {
        CatCodeStackItem::EndLine(self)
    }
}
impl<'a, 'b: 'a> Into<CatCodeStackItem<'a>> for &'a CatCodeStack<'b> {
    fn into(self) -> CatCodeStackItem<'a> {
        CatCodeStackItem::Stack(self)
    }
}

pub struct CatCodeStack<'a> {
    pub data: Vec<CatCodeStackItem<'a>>,
}
impl<'a> CatCodeStack<'a> {
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }
    pub fn new_with<'i: 'a, T: Into<CatCodeStackItem<'i>>>(item: T) -> Self {
        Self { data: vec![item.into()] }
    }
    pub fn push<T: Into<CatCodeStackItem<'a>>>(
        &mut self,
        item: T,
    ) -> &CatCodeStackItem {
        let index = self.len();
        self.data.push(item.into());
        unsafe { self.data.get_unchecked(index) }
    }
    pub fn pop(&mut self) -> Option<CatCodeStackItem> {
        self.data.pop()
    }
    pub fn remove(&mut self, index: usize) -> CatCodeStackItem {
        self.data.remove(index)
    }
    pub fn remove_of(
        &mut self,
        item: &CatCodeStackItem,
    ) -> Option<CatCodeStackItem> {
        if self.len() > 0 {
            unsafe {
                let item = item as *const CatCodeStackItem;
                assert!(
                    item >= self.data.as_ptr()
                        && item
                            <= self.data.get_unchecked(self.data.len() - 1)
                );
                let offset = item.offset_from(self.data.as_ptr()) as usize;
                Some(self.data.remove(offset))
            }
        } else {
            None
        }
    }
    pub fn truncate(&mut self, len: usize) {
        self.data.truncate(len);
    }
    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn update<'c, 'l: 'a + 'c>(
        &mut self,
        (lexer, ctabset): (&'l LexerType, &'l CTabSet),
        pos: &Position,
        mut categories_cache: RefMut<'c, Vec<&'c Category>>,
    ) -> anyhow::Result<()> {
        if lexer.is_empty() {
            return Ok(());
        }

        let mut done = false;
        while let Some(category) = categories_cache.last() {
            if category.is_match(pos) {
                trace!(
                    target: "Lexer",
                    "Matched ending category: {} at [{}, {}]",
                    category,
                    pos.row,
                    pos.col
                );
                self.pop();
                categories_cache.pop();
                done = true;
            } else {
                break;
            }
        }
        if done {
            return Ok(());
        }

        let mut unknown_ctabs = Vec::new();
        for (cs, ce, action) in &lexer.0 {
            if cs.is_match(pos) {
                trace!(
                    target: "Lexer",
                    "Matching starting category: {} at [{}, {}]; ending: {}",
                    cs,
                    pos.row,
                    pos.col,
                    ce
                );
                match action {
                    LexerAction::CatCode(cat) => match cat {
                        LexerCatCodeKind::Char(chars) => {
                            self.push(chars.as_slice());
                        }
                        LexerCatCodeKind::CTab(ctab_name) => {
                            if let Some(ctab) = ctabset.get_by_name(ctab_name)
                            {
                                self.push(ctab);
                            } else {
                                unknown_ctabs.push(ctab_name);
                            }
                        }
                    },
                    LexerAction::EndLine(endline) => {
                        self.push(*endline);
                    }
                };
                categories_cache.push(ce);
            }
        }

        if unknown_ctabs.is_empty() {
            Ok(())
        } else {
            Err(anyhow!("Unknown ctab name: {:?}", unknown_ctabs))
        }
    }
}
impl<'a> Index<usize> for CatCodeStack<'a> {
    type Output = CatCodeStackItem<'a>;
    fn index(&self, index: usize) -> &Self::Output {
        &self.data[index]
    }
}
impl<'a> IndexMut<usize> for CatCodeStack<'a> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.data[index]
    }
}
impl CatCodeGetter for CatCodeStack<'_> {
    fn catcode_value(&self, at: char) -> Option<CatCode> {
        for item in self.data.iter().rev() {
            match item {
                CatCodeStackItem::Char(c) => {
                    if c.charcode == at {
                        return Some(c.catcode);
                    }
                }
                CatCodeStackItem::CharVec(chars) => {
                    match chars.catcode_value(at) {
                        Some(cat) => return Some(cat),
                        None => continue,
                    }
                }
                CatCodeStackItem::CTab(ctab) => match ctab.get(at) {
                    Some(cat) => return Some(cat),
                    None => continue,
                },
                CatCodeStackItem::CTabRef(ctab) => match ctab.get(at) {
                    Some(cat) => return Some(cat),
                    None => continue,
                },
                CatCodeStackItem::CTabRefMut(ctab) => match ctab.get(at) {
                    Some(cat) => return Some(cat),
                    None => continue,
                },
                CatCodeStackItem::EndLine(_) => continue,
                CatCodeStackItem::Stack(s) => match s.catcode_value(at) {
                    Some(cat) => return Some(cat),
                    None => continue,
                },
            }
        }
        None
    }
    fn escape_char(&self) -> Option<char> {
        for item in self.data.iter().rev() {
            match item {
                CatCodeStackItem::Char(_) | CatCodeStackItem::CharVec(_) => {
                    continue
                }
                CatCodeStackItem::CTab(ctab) => return ctab.get_escape_char(),
                CatCodeStackItem::CTabRef(ctab) => {
                    return ctab.get_escape_char()
                }
                CatCodeStackItem::CTabRefMut(ctab) => {
                    return ctab.get_escape_char()
                }
                CatCodeStackItem::EndLine(endline) => {
                    return char::from_u32(*endline)
                }
                CatCodeStackItem::Stack(s) => return s.escape_char(),
            }
        }
        None
    }
    fn endline_char(&self) -> Option<char> {
        for item in self.data.iter().rev() {
            match item {
                CatCodeStackItem::Char(_) | CatCodeStackItem::CharVec(_) => {
                    continue
                }
                CatCodeStackItem::CTab(ctab) => {
                    return ctab.get_endline_char()
                }
                CatCodeStackItem::CTabRef(ctab) => {
                    return ctab.get_endline_char()
                }
                CatCodeStackItem::CTabRefMut(ctab) => {
                    return ctab.get_endline_char()
                }
                CatCodeStackItem::EndLine(_) => continue,
                CatCodeStackItem::Stack(s) => return s.endline_char(),
            }
        }
        None
    }
}

#[derive(Clone)]
pub struct ControlSequence {
    csname: String,
    pub escape_char: Option<char>,
}
impl Debug for ControlSequence {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let tag = unsafe { *self.csname.as_bytes().get_unchecked(0) };
        f.write_fmt(format_args!(
            "ControlSequence {{ csname: '{}', tag: {} }}",
            self.get_csname(),
            match tag {
                0 => "ControlWord",
                1 => "ControlSymbol",
                2 => "ControlSpace",
                _ => unreachable!(),
            }
        ))
    }
}
impl PartialEq for ControlSequence {
    fn eq(&self, other: &Self) -> bool {
        self.get_csname() == other.get_csname()
    }
}
impl Eq for ControlSequence {}
impl ControlSequence {
    /// Control Word.
    pub fn new_cwo<T: AsRef<str>>(s: T) -> Self {
        if s.as_ref() == " " {
            ControlSequence::new_csp()
        } else {
            ControlSequence {
                csname: format!("{}{}", '\x00', s.as_ref()),
                escape_char: None,
            }
        }
    }
    /// Control Symbol.
    pub fn new_csy(s: char) -> Self {
        if s == ' ' {
            ControlSequence::new_csp()
        } else {
            ControlSequence {
                csname: format!("{}{}", '\x01', s),
                escape_char: None,
            }
        }
    }
    /// Control Space.
    pub fn new_csp() -> Self {
        ControlSequence { csname: "\x02 ".to_string(), escape_char: None }
    }
    pub fn set_escape_char(&mut self, escape_char: u32) {
        self.escape_char = char::from_u32(escape_char);
    }
    pub fn with_escaped_char(mut self, escape_char: char) -> Self {
        self.escape_char = Some(escape_char);
        self
    }
    pub fn get_csname(&self) -> &str {
        unsafe { self.csname.get_unchecked(1 ..) }
    }
    pub fn get_csname_escaped(&self, e: u8) -> CompactString {
        escape_string(self.get_csname(), e)
    }
    pub fn cs_with_escape_char(
        &self,
        escape_char: Option<char>,
    ) -> CompactString {
        match escape_char {
            Some(chr) => format_compact!("{}{}", chr, self.get_csname()),
            None => self.get_csname().to_compact_string(),
        }
    }
    /// Write cs to stream, but do not process invisible and unprintable char.
    pub unsafe fn write<T: io::Write>(
        &self,
        escape_char: Option<char>,
        stream: &mut T,
    ) -> io::Result<()> {
        let escape_char = self.escape_char.or(escape_char);
        match escape_char {
            Some(chr) => write!(stream, "{}", chr)?,
            _ => {}
        };
        write!(stream, "{}", self.get_csname())?;
        Ok(())
    }
}

impl Hash for ControlSequence {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.get_csname().hash(state);
    }
}

pub fn escape_string<T: AsRef<str>>(s: T, e: u8) -> CompactString {
    let mut v = Vec::with_capacity(8);
    for c in s.as_ref().chars() {
        if c.is_alphanumeric() {
            v.push(format_compact!("{}", c));
        } else if c.is_control() {
            v.push(escape_string_small(&escape_control(c, e), e));
        } else {
            v.push(format_compact!("\"{:X} ", c as u32));
        }
    }
    v.concat_compact()
}

pub fn escape_string_filter<T: AsRef<str>>(
    s: T,
    e: u8,
    filter: impl Fn(char) -> bool,
    replacer: impl Fn(char) -> CompactString,
) -> CompactString {
    let mut v = Vec::with_capacity(8);
    for c in s.as_ref().chars() {
        if filter(c) {
            v.push(replacer(c));
        } else if c.is_alphanumeric() {
            v.push(format_compact!("{}", c));
        } else if c.is_control() {
            v.push(escape_string_small(&escape_control(c, e), e));
        } else {
            v.push(format_compact!("\"{:X} ", c as u32));
        }
    }
    v.concat_compact()
}

pub fn escape_string_small<T: AsRef<str>>(s: T, e: u8) -> CompactString {
    let mut v = SmallVec::<[CompactString; 4]>::new();
    for c in s.as_ref().chars() {
        if c.is_alphanumeric() {
            v.push(format_compact!("{}", c));
        } else if c.is_control() {
            v.push(escape_string_small(&escape_control(c, e), e));
        } else {
            v.push(format_compact!("\"{:X} ", c as u32));
        }
    }
    v.concat_compact()
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Character {
    pub charcode: char,
    pub catcode: CatCode,
}
impl From<(char, CatCode)> for Character {
    fn from(value: (char, CatCode)) -> Self {
        Self::new(value.0, value.1)
    }
}

impl Character {
    pub fn new(charcode: char, catcode: CatCode) -> Self {
        Character { charcode, catcode }
    }
    pub fn space() -> Self {
        Character { charcode: ' ', catcode: CatCode::Space }
    }
    pub fn get_pairs(&self) -> (char, CatCode) {
        (self.charcode, self.catcode)
    }
    pub fn is_punct(&self) -> bool {
        self.charcode.general_category_group()
            == GeneralCategoryGroup::Punctuation
    }
    pub unsafe fn write<T: io::Write>(
        &self,
        stream: &mut T,
    ) -> io::Result<()> {
        if self.charcode == '\r' {
            write!(stream, "\n")?;
        } else {
            write!(stream, "{}", self.charcode)?;
        }
        Ok(())
    }
    /// Transform ASCII control symbol to `^^.` form.
    pub fn escape_ascii_control(&self) -> Option<u8> {
        let chr = self.charcode as u32;
        if chr > 0x7F {
            return None;
        }
        if chr == 0x7F {
            return Some(63);
        }
        if chr < 0x20 {
            return Some(chr as u8 + 0x40);
        }
        return None;
    }
    /// Transform control symbol to `^^.` or `^^^^....` or `^^^^^^......` form.
    pub fn escape_control(&self, e: u8) -> CompactString {
        if self.charcode.is_control() {
            escape_control(self.charcode, e)
        } else {
            format_compact!("{}", self.charcode)
        }
    }
}

pub fn escape_control(c: char, e: u8) -> CompactString {
    let e_chr = e as char;
    assert!(
        !e_chr.is_ascii_control(),
        "Cannot be an ASCII control character."
    );
    match c {
        '\0' .. '\x20' => {
            format_compact!("{e_chr}{e_chr}{}", (c as u8 + 0x40) as char)
        }
        '\x7f' => {
            format_compact!("{e_chr}{e_chr}{}", '\x3f')
        }
        _ => {
            if c <= 255 as char {
                format_compact!("{e_chr}{e_chr}{:02x}", c as u8)
            } else if c <= '\u{ffff}' {
                format_compact!("{e_chr}{e_chr}{e_chr}{e_chr}{:04x}", c as u32)
            } else {
                format_compact!(
                    "{e_chr}{e_chr}{e_chr}{e_chr}{e_chr}{e_chr}{:06x}",
                    c as u32
                )
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub(crate) enum TokenType {
    CS,
    Char,
    Any,
}
pub struct TokenBytes(SmallVec<[u8; 24]>);
impl Deref for TokenBytes {
    type Target = SmallVec<[u8; 24]>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    CS(ControlSequence),
    Char(Character),
    Any(u32),
}
impl Token {
    /// New `ControlSequence`, a control space or control word.
    pub fn new_cs(cs_name: &str) -> Token {
        Token::CS(ControlSequence::new_cwo(cs_name))
    }
    /// New `ControlSequence`, a control space or control symbol.
    pub fn new_csy(c: char) -> Token {
        Token::CS(ControlSequence::new_csy(c))
    }
    /// New `Character`.
    pub fn new_char(charcode: char, catcode: CatCode) -> Token {
        Token::Char(Character::new(charcode, catcode))
    }

    pub fn is_cs(&self, token: Option<&ControlSequence>) -> bool {
        if let Token::CS(cs) = self {
            token.is_none() || cs == token.unwrap()
        } else {
            false
        }
    }
    pub fn is_char(&self, token: Option<Character>) -> bool {
        if let Token::Char(chr) = self {
            token.is_none() || *chr == token.unwrap()
        } else {
            false
        }
    }
    pub fn is_any(&self, token: Option<u32>) -> bool {
        if let Token::Any(any) = self {
            token.is_none() || *any == token.unwrap()
        } else {
            false
        }
    }

    pub fn to_str_repr(&self) -> CompactString {
        match self {
            Token::Char(c) => {
                format_compact!("({:?}, {:?})", c.charcode, c.catcode)
            }
            Token::CS(cs) => format_compact!("({:?})", cs.get_csname()),
            Token::Any(any) => format_compact!("({:08x})", *any),
        }
    }

    pub fn to_string(&self) -> CompactString {
        match self {
            Token::Char(c) => c.charcode.to_compact_string(),
            Token::CS(cs) => {
                if let Some(escape) = cs.escape_char {
                    format_compact!(
                        "{}{}",
                        escape,
                        cs.get_csname_escaped(b'^')
                    )
                } else {
                    format_compact!("<{}>", cs.get_csname_escaped(b'^'))
                }
            }
            Token::Any(any) => format_compact!("<{:08x}>", *any),
        }
    }

    /// Convert Token to `&[u8]`, So it can be read from bin file,
    /// and can be used in `regex::bytes::Regex`.
    pub fn to_bytes(&self) -> TokenBytes {
        fn extract_high_bits(input: u32) -> u8 {
            let input = input.to_le_bytes();
            ((input[0] >> 7) & 1) << 3
                | ((input[1] >> 7) & 1) << 2
                | ((input[2] >> 7) & 1) << 1
                | ((input[3] >> 7) & 1)
        }
        let mut res = SmallVec::new();
        match self {
            Token::CS(cs) => {
                res.push(
                    0b1000_0000
                        | unsafe { cs.csname.as_bytes().get_unchecked(0) },
                );
                res.extend_from_slice(cs.get_csname().as_bytes());
            }
            Token::Char(chr) => {
                res.push(0b1001_0000 | chr.catcode as u8);
                let mut buf: [u8; 4] = [0; 4];
                res.extend_from_slice(
                    chr.charcode.encode_utf8(&mut buf).as_bytes(),
                );
            }
            Token::Any(any) => {
                res.push(0b1010_0000 | extract_high_bits(*any));
                res.extend_from_slice(&(*any & 0x7f7f7f7f).to_le_bytes());
            }
        }
        res.push(0b1111_1111);
        TokenBytes(res)
    }

    pub unsafe fn from_bytes_unchecked(bytes: &[u8]) -> Token {
        let len = bytes.len();
        assert!(len > 2);
        let len_prev = len - 1;
        assert_eq!(bytes[len_prev], 0xff);

        let first_byte = *bytes.get_unchecked(0);
        let mark = first_byte & 0b0000_1111;
        match first_byte & 0b1111_0000 {
            0b1000_0000 => {
                let cs_name =
                    std::str::from_utf8_unchecked(&bytes[1 .. len_prev]);
                let csname = format!("{}{}", mark, cs_name);
                Token::CS(ControlSequence { csname, escape_char: None })
            }
            0b1001_0000 => {
                let catcode: CatCode = mark.try_into().unwrap();
                let s = std::str::from_utf8_unchecked(&bytes[1 .. len_prev]);
                let charcode = s.chars().next().unwrap();
                Token::Char(Character { charcode, catcode })
            }
            0b1010_0000 => {
                let num = u32::from_le_bytes([
                    *bytes.get_unchecked(1) | ((mark << 4) & 0x80),
                    *bytes.get_unchecked(2) | ((mark << 5) & 0x80),
                    *bytes.get_unchecked(3) | ((mark << 6) & 0x80),
                    *bytes.get_unchecked(4) | ((mark << 7) & 0x80),
                ]);
                Token::Any(num)
            }
            _ => unreachable!(),
        }
    }

    pub fn try_from_bytes(bytes: &[u8]) -> Result<Token, ErrorKind> {
        unsafe {
            let len = bytes.len();
            let len_prev = len - 1;
            if !((len > 2)
                && (*bytes.get_unchecked(len - 1) == 0xff)
                && (bytes[0 .. len_prev].iter().all(|&v| v < 0xff)))
            {
                return Err(ErrorKind::InvalidInput);
            }
            let first_byte = *bytes.get_unchecked(0);
            let mark = first_byte & 0b0000_1111;
            match first_byte & 0b1111_0000 {
                0b1000_0000 => {
                    if let Ok(cs_name) =
                        std::str::from_utf8(&bytes[1 .. len_prev])
                    {
                        match mark {
                            0 => {
                                return Ok(Token::CS(
                                    ControlSequence::new_cwo(cs_name),
                                ))
                            }
                            1 => {
                                let mut chars = cs_name.chars();
                                if let Some(chr) = chars.next() {
                                    if chars.next().is_none() {
                                        return Ok(Token::CS(
                                            ControlSequence::new_csy(chr),
                                        ));
                                    } else {
                                        return Err(ErrorKind::InvalidInput);
                                    }
                                } else {
                                    return Err(ErrorKind::InvalidInput);
                                }
                            }
                            2 => {
                                if cs_name == " " {
                                    return Ok(Token::CS(
                                        ControlSequence::new_csp(),
                                    ));
                                } else {
                                    return Err(ErrorKind::InvalidInput);
                                }
                            }
                            _ => unreachable!(),
                        }
                    } else {
                        return Err(ErrorKind::InvalidInput);
                    }
                }
                0b1001_0000 => {
                    if len > 6 {
                        return Err(ErrorKind::InvalidInput);
                    }
                    match mark.try_into() {
                        Ok(catcode) => {
                            if let Ok(chr) =
                                std::str::from_utf8(&bytes[1 .. len_prev])
                            {
                                if let Some(charcode) = chr.chars().next() {
                                    return Ok(Token::Char(Character::new(
                                        charcode, catcode,
                                    )));
                                } else {
                                    return Err(ErrorKind::InvalidInput);
                                }
                            } else {
                                return Err(ErrorKind::InvalidInput);
                            }
                        }
                        Err(err) => return Err(err),
                    }
                }
                0b1010_0000 => {
                    if len != 6 {
                        return Err(ErrorKind::InvalidInput);
                    }
                    let num = u32::from_le_bytes([
                        *bytes.get_unchecked(1) | ((mark << 4) & 0x80),
                        *bytes.get_unchecked(2) | ((mark << 5) & 0x80),
                        *bytes.get_unchecked(3) | ((mark << 6) & 0x80),
                        *bytes.get_unchecked(4) | ((mark << 7) & 0x80),
                    ]);
                    return Ok(Token::Any(num));
                }
                _ => unreachable!(),
            }
        }
    }
}

impl From<ControlSequence> for Token {
    fn from(value: ControlSequence) -> Self {
        Token::CS(value)
    }
}
impl From<Character> for Token {
    fn from(value: Character) -> Self {
        Token::Char(value)
    }
}

#[derive(Debug, PartialEq)]
pub struct TokenList {
    pub(crate) values: Vec<Token>,
}
impl TokenList {
    pub fn new() -> Self {
        TokenList { values: Vec::new() }
    }
    pub fn parse<S: AsRef<str>, C: CatCodeGetter>(
        source: S,
        catcode: &C,
    ) -> Self {
        let endline = match catcode.endline_char() {
            Some(chr) => format_compact!("{}", chr),
            None => "".to_compact_string(),
        };
        let s = source.as_ref().lines().collect::<Vec<_>>().join(&endline);
        TokenList::_parse(s.chars(), catcode)
    }
    pub fn len(&self) -> usize {
        self.values.len()
    }
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }
    pub fn push<T: Into<Token>>(&mut self, token: T) {
        self.values.push(token.into());
    }
    pub fn iter(&self) -> std::slice::Iter<'_, Token> {
        self.values.iter()
    }
    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, Token> {
        self.values.iter_mut()
    }
    /// Write tokenlist to stream, but do not process invisible and unprintable char.
    pub unsafe fn write<T: io::Write>(
        &self,
        escape_char: Option<char>,
        stream: &mut T,
    ) -> io::Result<()> {
        for token in self.iter() {
            match token {
                Token::CS(cs) => cs.write(escape_char, stream)?,
                Token::Char(chr) => chr.write(stream)?,
                Token::Any(chr)
                    if *chr == '\n' as u32 || *chr == '\r' as u32 =>
                {
                    write!(stream, "\n")?
                }
                Token::Any(_) => unreachable!(),
            };
        }
        Ok(())
    }

    pub fn to_bytes(&self) -> TokenListBytes {
        let mut values = Vec::with_capacity(self.len() * 4);
        let mut lens = Vec::with_capacity(self.len() + 1);
        let mut index = 0usize;
        for token in self.iter() {
            let bytes = token.to_bytes();
            lens.push(index);
            index += bytes.len();
            values.extend_from_slice(&bytes);
        }
        lens.push(index);
        TokenListBytes { values, lens }
    }

    fn _parse<C: CatCodeGetter>(mut source: Chars, catcode: &C) -> TokenList {
        let mut res = Vec::new();

        let mut cs_name = String::new();
        let mut escaped_char = '\0';
        let mut collect_cs = false;

        while let Some(chr) = source.next() {
            let mut cat = catcode.catcode_value(chr).unwrap_or_default();
            let chr = if cat == CatCode::Superscript {
                let (chr, n) =
                    circumflex_mechanism(catcode, source.clone(), chr);
                if n > 0 {
                    source.advance_by(n).unwrap();
                    cat = catcode.catcode_value(chr).unwrap_or(cat);
                }
                chr
            } else {
                chr
            };
            if collect_cs {
                if cat == CatCode::Letter {
                    cs_name.push(chr);
                    continue;
                } else if cs_name.is_empty() {
                    res.push(Token::CS(
                        ControlSequence::new_csy(chr)
                            .with_escaped_char(escaped_char),
                    ));
                    collect_cs = false;
                    continue;
                } else {
                    res.push(Token::CS(
                        ControlSequence::new_cwo(&cs_name)
                            .with_escaped_char(escaped_char),
                    ));
                    cs_name.clear();
                    collect_cs = false;
                }
            }
            if cat == CatCode::Escape {
                collect_cs = true;
                escaped_char = chr;
            } else {
                res.push(Token::Char(Character {
                    charcode: chr,
                    catcode: cat,
                }));
            }
        }
        if !cs_name.is_empty() {
            let last_char = cs_name.chars().next_back().unwrap();
            if catcode.catcode_value(last_char) == Some(CatCode::Letter) {
                res.push(Token::CS(
                    ControlSequence::new_cwo(&cs_name)
                        .with_escaped_char(escaped_char),
                ));
            } else {
                res.push(Token::CS(
                    ControlSequence::new_csy(last_char)
                        .with_escaped_char(escaped_char),
                ));
            }
            cs_name.clear();
        }
        TokenList { values: res }
    }
}
impl IntoIterator for TokenList {
    type IntoIter = vec::IntoIter<Token>;
    type Item = Token;
    fn into_iter(self) -> Self::IntoIter {
        self.values.into_iter()
    }
}

#[derive(Debug, PartialEq)]
pub struct TokenListBytes {
    pub(crate) values: Vec<u8>,
    pub(crate) lens: Vec<usize>,
}
impl TokenListBytes {
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }
    /// Get numbers of tokens. O(1).
    pub fn tokens_len(&self) -> usize {
        self.lens.len() - 1
    }
    pub fn as_bytes(&self) -> &[u8] {
        &self.values
    }
    /// Return the index in bytes at pos.
    /// This panics when pos > tokens_len + 1.
    pub fn bytes_index(&self, pos: usize) -> usize {
        self.lens[pos]
    }
}

#[derive(Debug)]
pub struct TokenListBytesRef<'b> {
    pub(crate) tokens: &'b Vec<Token>,
    pub(crate) bytes: &'b Vec<u8>,
    pub(crate) indices: &'b Vec<usize>,
}
impl<'b> TokenListBytesRef<'b> {
    pub unsafe fn new(
        tokens: &'b Vec<Token>,
        bytes: &'b Vec<u8>,
        indices: &'b Vec<usize>,
    ) -> Self {
        Self { tokens, bytes, indices }
    }
    pub fn tokens(&self) -> &[Token] {
        self.tokens
    }
    pub fn bytes_indices(&self, pos: usize) -> usize {
        self.indices[pos]
    }
}

pub(crate) fn circumflex_mechanism<'c, C: CatCodeGetter>(
    catcode: &C,
    mut chars: Chars<'c>,
    mut chr: char,
) -> (char, usize) {
    let mut advance = 1;
    let mut last_char = None;

    fn more1(chr: &mut char, advance: &mut usize) {
        if matches!(chr, '0'..='9' | 'a'..='z') {
            *chr = char::from_u32(
                u32::from_str_radix(&format!("{}{}", *chr, *chr), 16).unwrap(),
            )
            .unwrap();
            *advance = 3;
        } else if (*chr as u32) < 128 {
            *chr = if (*chr as u8) < 64 {
                ((*chr as u8) + 0o100) as char
            } else {
                ((*chr as u8) - 0o100) as char
            };
            *advance = 2;
        } else {
            *advance = 0;
        }
    }
    fn more2(chr: &mut char, c1: char, c2: Option<char>, advance: &mut usize) {
        if matches!(c1, '0'..='9' | 'a'..='f') {
            match c2 {
                Some(c2) if matches!(c2, '0'..='9' | 'a'..='f') => {
                    *chr = char::from_u32(
                        u32::from_str_radix(&format!("{}{}", c1, c2), 16)
                            .unwrap(),
                    )
                    .unwrap();
                    *advance = 3;
                }
                _ => {
                    *chr = if (c1 as u32) < 64 {
                        ((c1 as u8) + 0o100) as char
                    } else {
                        ((c1 as u8) - 0o100) as char
                    };
                    *advance = 2;
                }
            }
        } else if (c1 as u32) < 128 {
            *chr = if (c1 as u8) < 64 {
                ((c1 as u8) + 0o100) as char
            } else {
                ((c1 as u8) - 0o100) as char
            };
            *advance = 2;
        } else {
            *advance = 0;
        }
    }

    while let Some(next_char) = chars.next() {
        if next_char == chr
            && catcode.catcode_value(chr) == Some(CatCode::Superscript)
        {
            advance += 1;
            if advance > 4 {
                last_char = Some(next_char);
                break;
            }
        } else {
            last_char = Some(next_char);
            break;
        }
    }
    match advance {
        0 | 1 => return (chr, 0),
        2 => match last_char {
            Some(last_char) => {
                more2(&mut chr, last_char, chars.next(), &mut advance)
            }
            None => advance = 0,
        },
        3 => {
            let c1 = chr;
            more2(&mut chr, c1, last_char, &mut advance)
        }
        _ => more1(&mut chr, &mut advance),
    }
    (chr, advance)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_find_comment_len() {
        let mut s = r#"*/"#;
        assert_eq!(find_comment_len(s.chars()), 2);

        s = r#"**/"#;
        assert_eq!(find_comment_len(s.chars()), 3);

        s = r#"* AB*/"#;
        assert_eq!(find_comment_len(s.chars()), 6);

        s = r#"AB*/"#;
        assert_eq!(find_comment_len(s.chars()), 0);

        s = "*";
        assert_eq!(find_comment_len(s.chars()), 1);

        s = r#"* d/* ds */ */"#;
        assert_eq!(find_comment_len(s.chars()), 14);

        s = r#"* d/* ds */ *//*~~*/"#;
        assert_eq!(find_comment_len(s.chars()), 14);

        s = r#"* d/* ds */ */*/"#;
        assert_eq!(find_comment_len(s.chars()), 14);

        s = r#"* d/* ds */ /* /* */"#;
        assert_eq!(find_comment_len(s.chars()), 20);
    }

    #[test]
    fn ctab_normalize() {
        use CatCode::*;
        let mut ctab = unsafe {
            CTab::from_iter_unchecked([
                ('#' ..= '#', Parameter),
                ('0' ..= '9', Other),
                ('A' ..= 'Z', Letter),
                ('L' ..= 'a', Letter),
                ('Y' ..= 'd', Other),
                ('e' ..= 'z', Letter),
                ('\u{4E00}' ..= '\u{9FA5}', Letter),
                ('\u{5E00}' ..= '\u{5F00}', Other),
                ('\u{9F00}' ..= '\u{9FFF}', Active),
            ])
        };
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('#' ..= '#'),
                NumberSpan::from('0' ..= '9'),
                NumberSpan::from('A' ..= 'Z'),
                NumberSpan::from('L' ..= 'a'),
                NumberSpan::from('Y' ..= 'd'),
                NumberSpan::from('e' ..= 'z'),
                NumberSpan::from('\u{4E00}' ..= '\u{9FA5}'),
                NumberSpan::from('\u{5E00}' ..= '\u{5F00}'),
                NumberSpan::from('\u{9F00}' ..= '\u{9FFF}')
            ]
        );
        assert_eq!(
            &ctab.catcodes,
            &vec![
                Parameter, Other, Letter, Letter, Other, Letter, Letter,
                Other, Active
            ]
        );
        ctab._normalize();
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('#' ..= '#'),
                NumberSpan::from('0' ..= '9'),
                NumberSpan::from('A' ..= 'a'),
                NumberSpan::from('b' ..= 'd'),
                NumberSpan::from('e' ..= 'z'),
                NumberSpan::from('\u{4E00}' ..= '\u{9FA5}'),
                NumberSpan::from('\u{9FA6}' ..= '\u{9FFF}')
            ]
        );
        assert_eq!(
            &ctab.catcodes,
            &vec![Parameter, Other, Letter, Other, Letter, Letter, Active]
        );

        ctab = unsafe {
            CTab::from_iter_unchecked([
                ('#' ..= '#', Parameter),
                ('0' ..= '9', Other),
                ('A' ..= 'Z', Letter),
                ('L' ..= 'a', Letter),
                ('Y' ..= 'd', Letter),
                ('e' ..= 'z', Letter),
                ('\u{4E00}' ..= '\u{9FA5}', Letter),
                ('\u{5E00}' ..= '\u{5F00}', Other),
                ('\u{9F00}' ..= '\u{9FFF}', Letter),
            ])
        };
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('#' ..= '#'),
                NumberSpan::from('0' ..= '9'),
                NumberSpan::from('A' ..= 'Z'),
                NumberSpan::from('L' ..= 'a'),
                NumberSpan::from('Y' ..= 'd'),
                NumberSpan::from('e' ..= 'z'),
                NumberSpan::from('\u{4E00}' ..= '\u{9FA5}'),
                NumberSpan::from('\u{5E00}' ..= '\u{5F00}'),
                NumberSpan::from('\u{9F00}' ..= '\u{9FFF}')
            ]
        );
        assert_eq!(
            &ctab.catcodes,
            &vec![
                Parameter, Other, Letter, Letter, Letter, Letter, Letter,
                Other, Letter
            ]
        );
        ctab._normalize();
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('#' ..= '#'),
                NumberSpan::from('0' ..= '9'),
                NumberSpan::from('A' ..= 'z'),
                NumberSpan::from('\u{4E00}' ..= '\u{9FFF}')
            ]
        );
        assert_eq!(&ctab.catcodes, &vec![Parameter, Other, Letter, Letter]);

        ctab = unsafe {
            CTab::from_iter_unchecked([
                ('#' ..= '#', Parameter),
                ('0' ..= '9', Other),
                ('A' ..= 'z', Letter),
                ('\u{4E00}' ..= '\u{9FA5}', Letter),
            ])
        };
        ctab._normalize();
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('#' ..= '#'),
                NumberSpan::from('0' ..= '9'),
                NumberSpan::from('A' ..= 'z'),
                NumberSpan::from('\u{4E00}' ..= '\u{9FA5}')
            ]
        );
        assert_eq!(&ctab.catcodes, &vec![Parameter, Other, Letter, Letter]);

        ctab = unsafe {
            CTab::from_iter_unchecked([
                ('#' ..= '#', Parameter),
                ('0' ..= '9', Other),
                ('A' ..= 'z', Letter),
                ('\u{4E00}' ..= '\u{10FFFF}', Letter),
                ('\u{10FFF0}' ..= '\u{10FFF0}', Letter),
            ])
        };
        ctab._normalize();
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('#' ..= '#'),
                NumberSpan::from('0' ..= '9'),
                NumberSpan::from('A' ..= 'z'),
                NumberSpan::from('\u{4E00}' ..= '\u{10FFFF}')
            ]
        );
        assert_eq!(&ctab.catcodes, &vec![Parameter, Other, Letter, Letter]);

        ctab = unsafe {
            CTab::from_iter_unchecked([
                ('#' ..= '#', Letter),
                ('\u{10FFFF}' ..= '\u{10FFFF}', Letter),
            ])
        };
        ctab._normalize();
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('#' ..= '#'),
                NumberSpan::from('\u{10FFFF}' ..= '\u{10FFFF}')
            ]
        );
        assert_eq!(&ctab.catcodes, &vec![Letter, Letter]);

        ctab = unsafe {
            CTab::from_iter_unchecked([(char::MIN ..= char::MAX, Other)])
        };
        assert_eq!(
            &ctab.chars,
            &vec![NumberSpan::from(char::MIN ..= char::MAX)]
        );
        assert_eq!(&ctab.catcodes, &vec![Other]);

        ctab = CTab::new();
        ctab._normalize();
        assert_eq!(&ctab.chars, &vec![]);
        assert_eq!(&ctab.catcodes, &vec![]);

        ctab = unsafe {
            CTab::from_iter_unchecked([
                ('#' ..= '#', Parameter),
                ('0' ..= '9', Other),
                ('A' ..= 'z', Letter),
                ('\u{2000}' ..= '\u{5000}', Other),
                ('\u{5001}' ..= '\u{8FFF}', Letter),
                ('\u{9000}' ..= '\u{10000}', Active),
                ('\u{9FA6}' ..= '\u{10000}', Other),
                ('\u{10001}' ..= '\u{10FFFF}', Other),
            ])
        };
        ctab._normalize();
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('#' ..= '#'),
                NumberSpan::from('0' ..= '9'),
                NumberSpan::from('A' ..= 'z'),
                NumberSpan::from('\u{2000}' ..= '\u{5000}'),
                NumberSpan::from('\u{5001}' ..= '\u{8FFF}'),
                NumberSpan::from('\u{9000}' ..= '\u{10000}'),
                NumberSpan::from('\u{10001}' ..= '\u{10FFFF}')
            ]
        );
        assert_eq!(
            &ctab.catcodes,
            &vec![Parameter, Other, Letter, Other, Letter, Active, Other]
        );
    }

    #[test]
    fn ctab_emplace_item() {
        use CatCode::*;
        let mut ctab = unsafe {
            CTab::from_iter_unchecked([
                ('#' ..= '#', Parameter),
                ('0' ..= '9', Other),
                ('A' ..= 'z', Letter),
                ('\u{4E00}' ..= '\u{10FFFF}', Other),
            ])
        };
        ctab.emplace_item('\u{4E00}' ..= '\u{9FA5}', Letter);
        assert_eq!(ctab.iter().count(), 5);
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('#' ..= '#'),
                NumberSpan::from('0' ..= '9'),
                NumberSpan::from('A' ..= 'z'),
                NumberSpan::from('\u{4E00}' ..= '\u{9FA5}'),
                NumberSpan::from('\u{9FA6}' ..= '\u{10FFFF}')
            ]
        );
        assert_eq!(
            &ctab.catcodes,
            &vec![Parameter, Other, Letter, Letter, Other]
        );

        ctab.emplace_item('\x5B' ..= '\x60', Other);
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('#' ..= '#'),
                NumberSpan::from('0' ..= '9'),
                NumberSpan::from('A' ..= 'Z'),
                NumberSpan::from('\x5B' ..= '\x60'),
                NumberSpan::from('a' ..= 'z'),
                NumberSpan::from('\u{4E00}' ..= '\u{9FA5}'),
                NumberSpan::from('\u{9FA6}' ..= '\u{10FFFF}')
            ]
        );
        assert_eq!(
            &ctab.catcodes,
            &vec![Parameter, Other, Letter, Other, Letter, Letter, Other]
        );

        ctab.emplace_item('\x5B' ..= '\x60', Letter);
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('#' ..= '#'),
                NumberSpan::from('0' ..= '9'),
                NumberSpan::from('A' ..= 'z'),
                NumberSpan::from('\u{4E00}' ..= '\u{9FA5}'),
                NumberSpan::from('\u{9FA6}' ..= '\u{10FFFF}')
            ]
        );
        assert_eq!(
            &ctab.catcodes,
            &vec![Parameter, Other, Letter, Letter, Other]
        );

        ctab.emplace_item('\u{2000}' ..= '\u{5000}', Other);
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('#' ..= '#'),
                NumberSpan::from('0' ..= '9'),
                NumberSpan::from('A' ..= 'z'),
                NumberSpan::from('\u{2000}' ..= '\u{5000}'),
                NumberSpan::from('\u{5001}' ..= '\u{9FA5}'),
                NumberSpan::from('\u{9FA6}' ..= '\u{10FFFF}')
            ]
        );
        assert_eq!(
            &ctab.catcodes,
            &vec![Parameter, Other, Letter, Other, Letter, Other]
        );
        ctab.emplace_item('\u{9000}' ..= '\u{10000}', Active);
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('#' ..= '#'),
                NumberSpan::from('0' ..= '9'),
                NumberSpan::from('A' ..= 'z'),
                NumberSpan::from('\u{2000}' ..= '\u{5000}'),
                NumberSpan::from('\u{5001}' ..= '\u{8FFF}'),
                NumberSpan::from('\u{9000}' ..= '\u{10000}'),
                NumberSpan::from('\u{10001}' ..= '\u{10FFFF}')
            ]
        );
        assert_eq!(
            &ctab.catcodes,
            &vec![Parameter, Other, Letter, Other, Letter, Active, Other]
        );

        assert_eq!(ctab.remove('?'), None);
        assert_eq!(ctab.remove('#'), Some(Parameter));
        assert_eq!(ctab.remove('\u{2000}'), Some(Other));
        assert_eq!(ctab.remove('\u{8FFF}'), Some(Letter));
        assert_eq!(ctab.remove('\u{9500}'), Some(Active));
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('0' ..= '9'),
                NumberSpan::from('A' ..= 'z'),
                NumberSpan::from('\u{2001}' ..= '\u{5000}'),
                NumberSpan::from('\u{5001}' ..= '\u{8FFE}'),
                NumberSpan::from('\u{9000}' ..= '\u{94FF}'),
                NumberSpan::from('\u{9501}' ..= '\u{10000}'),
                NumberSpan::from('\u{10001}' ..= '\u{10FFFF}')
            ]
        );
        assert_eq!(
            &ctab.catcodes,
            &vec![Other, Letter, Other, Letter, Active, Active, Other]
        );
    }

    #[test]
    fn ctabset_parse() {
        use CatCode::*;
        let ctabset_str = r#"
        [latexcode]
        `\\ = 0
        `\{ = 1
        `\} = 2
        `\$ = 3
        `\& = 4
        13  = 5
        `\# = 6
        `\^ = 7
        `\_ = 8
        0   = 9
        32  = 10
        `@  = 11
        `A..`Z=11
        `a..`z=11
        `\~ = 13
        `\% = 14
        16  = 15
        [main]
        `a..`z=13 /* cannot be `a .. `z=13 */
        `A..`Z=11
        `\  = 10
        `\# =6
        `\= =9
        `\\ =0
        `\[ =1
        `\] =2
        escapechar=-1
        endlinechar=-1
        /**  `? = 1/* ?? */5  <- this will cause an error  */
        0x4E00..0x9FA5=11
        [error]
        21=?
        [l3] `a..`z=11 `A..`Z=11 `:=11 `\_=11 `\ =9 `\~=10 ?=11 `\%=Comment
        [[[lk] /* only [lk], [[ are ignored */
        ] /* ignored */
        [ /* ignored */
        [empty]
        endlinechar=`\\
        escapechar=`\ 
        [cjk]
        UNKNOWN = 11
        CJK_UNIFIED_IDEOGRAPHS_EXTENSION_A = 11
        CJK_UNIFIED_IDEOGRAPHS = 11
        "#;
        let ctabset = CTabSet::from_str(&ctabset_str).unwrap();
        let ctab_main = ctabset.get_by_name("main").unwrap();
        assert_eq!(ctab_main.get(' ').unwrap_or_default(), Space);
        assert_eq!(ctab_main.get('c').unwrap_or_default(), Active);
        assert_eq!(ctab_main.get('=').unwrap_or_default(), Ignored);
        assert_eq!(ctab_main.get('[').unwrap_or_default(), BeginGroup);
        assert_eq!(ctab_main.get(']').unwrap_or_default(), EndGroup);
        assert_eq!(ctab_main.get('#').unwrap_or_default(), Parameter);
        assert_eq!(ctab_main.get('?').unwrap_or_default(), Other);
        assert_eq!(ctab_main.get('\\').unwrap_or_default(), Escape);
        assert_eq!(ctab_main.get('\u{4E00}').unwrap_or_default(), Letter);
        assert_eq!(ctab_main.get_escape_char(), None);
        assert_eq!(ctab_main.get_endline_char(), None);

        let ctab_latexcode = ctabset.get_by_name("latexcode").unwrap();
        assert_eq!(ctab_latexcode.get('[').unwrap_or_default(), Other);
        assert_eq!(ctab_latexcode.get(']').unwrap_or_default(), Other);
        assert_eq!(ctab_latexcode.get('#').unwrap_or_default(), Parameter);

        let ctab_l3 = ctabset.get_by_name("l3").unwrap();
        assert_eq!(ctab_l3.get(' ').unwrap_or_default(), Ignored);
        assert_eq!(ctab_l3.get('c').unwrap_or_default(), Letter);
        assert_eq!(ctab_l3.get('=').unwrap_or_default(), Other);
        assert_eq!(ctab_l3.get('~').unwrap_or_default(), Space);
        assert_eq!(ctab_l3.get('_').unwrap_or_default(), Letter);
        assert_eq!(ctab_l3.get(':').unwrap_or_default(), Letter);
        assert_eq!(ctab_l3.get('?').unwrap_or_default(), Other);
        assert_eq!(ctab_l3.get('%').unwrap_or_default(), Comment);

        let ctab_empty = ctabset.get_by_name("empty").unwrap();
        assert_eq!(ctab_empty.get_endline_char(), Some('\\'));
        assert_eq!(ctab_empty.get_escape_char(), Some(' '));
        assert!(ctabset.get_by_name("lk").is_some());

        assert_eq!(ctabset.get_catcode_value("main", ' '), Some(Space));
        assert_eq!(ctabset.get_escape_char("main"), None);
        assert_eq!(ctabset.get_endline_char("main"), None);
        assert_eq!(ctabset.get_escape_char("error"), Some('\\'));
        assert_eq!(ctabset.get_escape_char("empty"), Some(' '));
        assert_eq!(ctabset.get_endline_char("empty"), Some('\\'));

        let ctab_cjk = ctabset.get_by_name("cjk").unwrap();
        assert_eq!(ctab_cjk.get('\u{4E00}'), Some(Letter));
        assert_eq!(ctab_cjk.get('\u{3400}'), Some(Letter));
        assert_eq!(ctab_cjk.get('\u{20794}'), None);
        assert_eq!(ctab_cjk.get('\x20'), None);
    }

    #[test]
    fn circumflex() {
        let mut chars = r"^^65lax".chars();
        let chr = chars.next().unwrap();
        let (new_chr, advance) =
            circumflex_mechanism(&CTab::document(), chars, chr);
        assert_eq!(advance, 3);
        assert_eq!(new_chr, '\x65');

        let mut chars = r"^^Ilax".chars();
        let chr = chars.next().unwrap();
        let (new_chr, advance) =
            circumflex_mechanism(&CTab::document(), chars, chr);
        assert_eq!(advance, 2);
        assert_eq!(new_chr, '\t');

        let mut chars = r"^^elax".chars();
        let chr = chars.next().unwrap();
        let (new_chr, advance) =
            circumflex_mechanism(&CTab::document(), chars, chr);
        assert_eq!(advance, 2);
        assert_eq!(new_chr, '%');

        let mut chars = r"^^^^6500lax".chars();
        let chr = chars.next().unwrap();
        let (new_chr, advance) = circumflex_mechanism(&CTab::document(), chars, chr);
        assert_eq!(advance, 7);
        assert_eq!(new_chr, '\u{6500}');
    }

    #[test]
    fn ctab_fallbackable() {
        use CatCode::*;
        let ctabset = CTabSet::from_str(
            r#"
        [initex]
        13 = 5  /* <return> */
        32 = 10 /* <space> */
        0  = 9  /* <null> */
        16 = 15 /* <delete> */
        `A..`Z=11
        `a..`z=11
        `\% =14
        `\\ =0
        [latex]
        `\{ =1
        `\} =2
        `\$ =3
        `\& =4
        `\# =6
        `\^ =7
        `\_ =8
        `\~ =13
        [latex3]
        `\: =11
        `\_ =11
        `\  =9
        `\~ =10
        "#,
        )
        .unwrap();

        let cjk = RefCell::new(CTab::cjk_ideographs(Letter));

        assert_eq!(ctabset.get_escape_char("initex"), Some('\\'));
        assert_eq!(ctabset.get_endline_char("initex"), Some('\r'));
        {
            let mut cat_stack = CatCodeStack::new();
            cat_stack.push(cjk.borrow());
            cat_stack.push(
                ctabset
                    .get_by_name("initex")
                    .expect("parse error! no [initex]"),
            );
            cat_stack.push(
                ctabset.get_by_name("latex").expect("parse error! no [latex]"),
            );
            cat_stack.push(
                ctabset
                    .get_by_name("latex3")
                    .expect("parse error! no [latex3]"),
            );
            assert_eq!(cat_stack.catcode_value('_'), Some(Letter));
            assert_eq!(cat_stack.catcode_value(' '), Some(Ignored));
            assert_eq!(cat_stack.catcode_value('\u{4E00}'), Some(Letter)); // CJK_UNIFIED_IDEOGRAPHS
            assert_eq!(cat_stack.catcode_value('\u{3400}'), Some(Letter)); // CJK_UNIFIED_IDEOGRAPHS_EXTENSION_A
            assert_eq!(cat_stack.catcode_value('\u{20000}'), Some(Letter)); // CJK_UNIFIED_IDEOGRAPHS_EXTENSION_B
            assert_eq!(cat_stack.catcode_value('\u{2A700}'), Some(Letter)); // CJK_UNIFIED_IDEOGRAPHS_EXTENSION_C
            assert_eq!(cat_stack.catcode_value('\u{2B740}'), Some(Letter)); // CJK_UNIFIED_IDEOGRAPHS_EXTENSION_D
            assert_eq!(cat_stack.catcode_value('\u{2B820}'), Some(Letter)); // CJK_UNIFIED_IDEOGRAPHS_EXTENSION_E
            assert_eq!(cat_stack.catcode_value('\u{2CEB0}'), Some(Letter)); // CJK_UNIFIED_IDEOGRAPHS_EXTENSION_F
            assert_eq!(cat_stack.catcode_value('\u{30000}'), Some(Letter)); // CJK_UNIFIED_IDEOGRAPHS_EXTENSION_G
            assert_eq!(cat_stack.catcode_value('\u{31350}'), Some(Letter)); // CJK_UNIFIED_IDEOGRAPHS_EXTENSION_H
            assert_eq!(cat_stack.catcode_value('\u{2EBF0}'), Some(Letter)); // CJK_UNIFIED_IDEOGRAPHS_EXTENSION_I
            assert_eq!(cat_stack.catcode_value('\u{F900}'), Some(Letter)); // CJK_COMPATIBILITY_IDEOGRAPHS
            assert_eq!(cat_stack.catcode_value('\u{2F800}'), Some(Letter)); // CJK_COMPATIBILITY_IDEOGRAPHS_SUPPLEMENT

            assert_eq!(cat_stack[3].catcode_value('_'), Some(Letter));
        }

        {
            let initex = ctabset.get_by_name("initex").unwrap();
            let latex = ctabset.get_by_name("latex").unwrap();
            let latex3 = ctabset.get_by_name("latex3").unwrap();
            let initex_fb =
                CTabFallbackable::new(initex, Option::<Character>::None);
            let latex_fb = CTabFallbackable::new(latex, Some(&initex_fb));
            let latex3_fb = CTabFallbackable::new(latex3, Some(&latex_fb));

            assert_eq!(initex_fb.catcode_value('\r'), Some(EndLine));
            assert_eq!(initex_fb.catcode_value(' '), Some(Space));
            assert_eq!(initex_fb.catcode_value('\0'), Some(Ignored));
            assert_eq!(initex_fb.catcode_value('\x10'), Some(Invalid));
            assert_eq!(initex_fb.catcode_value('a'), Some(Letter));
            assert_eq!(initex_fb.catcode_value('%'), Some(Comment));
            assert_eq!(initex_fb.catcode_value('\\'), Some(Escape));
            assert_eq!(initex_fb.catcode_value('#'), None);
            assert_eq!(initex_fb.catcode_value('$'), None);
            assert_eq!(initex_fb.catcode_value('?'), None);

            assert_eq!(latex_fb.catcode_value('{'), Some(BeginGroup));
            assert_eq!(latex_fb.catcode_value('}'), Some(EndGroup));
            assert_eq!(latex_fb.catcode_value('$'), Some(MathShift));
            assert_eq!(latex_fb.catcode_value('&'), Some(Alignment));
            assert_eq!(latex_fb.catcode_value('#'), Some(Parameter));
            assert_eq!(latex_fb.catcode_value('^'), Some(Superscript));
            assert_eq!(latex_fb.catcode_value('_'), Some(Subscript));
            assert_eq!(latex_fb.catcode_value('~'), Some(Active));
            assert_eq!(latex_fb.catcode_value(':'), None);
            assert_eq!(latex_fb.catcode_value('a'), Some(Letter));
            assert_eq!(latex_fb.catcode_value('\r'), Some(EndLine));
            assert_eq!(latex_fb.catcode_value(' '), Some(Space));

            assert_eq!(latex3_fb.catcode_value(':'), Some(Letter));
            assert_eq!(latex3_fb.catcode_value('_'), Some(Letter));
            assert_eq!(latex3_fb.catcode_value(' '), Some(Ignored));

            assert_eq!(ctabset.get_escape_char("latex"), Some('\\'));
            assert_eq!(ctabset.get_endline_char("latex"), Some('\r'));
        }

        assert_eq!(ctabset.get_escape_char("latex3"), Some('\\'));
        assert_eq!(ctabset.get_endline_char("latex3"), Some('\r'));

        assert_eq!(ctabset.get_escape_char("None"), None);
        assert_eq!(ctabset.get_endline_char("None"), None);

        for (name, ctab) in ctabset {
            println!("Get: name={}, len={}", name, ctab.len());
        }
    }

    #[test]
    fn catcodeset() {
        use CatCode::*;
        let mut ccset = CatCodeSet::Escape;
        assert_eq!(ccset.bits(), 0x0001);
        ccset.remove_catcode(Escape);
        assert!(ccset.is_empty());
        assert_eq!(ccset.into_catcode_iter().next(), None);

        ccset = [Escape, MathShift, Alignment, Other, Letter, Comment].into();

        assert_eq!(ccset.len(), 6);
        assert_eq!(
            ccset.bits(),
            0x0001 + 0x0008 + 0x0010 + 0x1000 + 0x0800 + 0x4000
        );
        ccset.remove_catcode(Escape);
        ccset.remove_catcode(Comment);
        assert!(!ccset.contains_catcode(Escape));
        assert!(!ccset.contains_catcode(Comment));
        assert_eq!(ccset.first_range(), Some(MathShift ..= Alignment));
        assert_eq!(ccset.last_range(), Some(Letter ..= Other));

        ccset.insert_catcode(Escape);
        ccset.insert_catcode(Invalid);
        ccset.remove_catcode(Alignment);
        ccset.remove_catcode(Other);
        assert_eq!(ccset.len(), 4);
        assert_eq!(ccset.bits(), 0x0001 + 0x0008 + 0x0800 + 0x8000);
        assert_eq!(CatCodeSet::empty().into_catcode_iter().next(), None);
        assert_eq!(CatCodeSet::empty().into_catcode_iter().next_back(), None);
        let mut iter = ccset.into_catcode_iter();
        assert_eq!(iter.next(), Some(Escape));
        assert_eq!(iter.next_back(), Some(Invalid));
        assert_eq!(iter.next_back(), Some(Letter));
        assert_eq!(iter.next(), Some(MathShift));
        assert_eq!(iter.next(), None);
        assert_eq!(iter.next_back(), None);
        assert_eq!(ccset.last_range(), Some(Invalid ..= Invalid));

        assert_eq!(CatCodeSet::empty().into_catcode_range_iter().next(), None);
        assert_eq!(
            CatCodeSet::empty().into_catcode_range_iter().next_back(),
            None
        );
        ccset = [Escape, MathShift, Alignment, Other, Letter, Invalid].into();
        let mut iter = ccset.into_catcode_range_iter();
        assert_eq!(iter.next(), Some(Escape ..= Escape));
        assert_eq!(iter.next_back(), Some(Invalid ..= Invalid));
        assert_eq!(iter.next_back(), Some(Letter ..= Other));
        assert_eq!(iter.next(), Some(MathShift ..= Alignment));
        assert_eq!(iter.next(), None);
        assert_eq!(iter.next_back(), None);

        assert_eq!(Step::backward_checked(CatCode::MIN, 1), None);
        assert_eq!(Step::forward_checked(Escape, 15), Some(Invalid));
        assert_eq!(Step::forward_checked(CatCode::MAX, 1), None);
        assert_eq!(Step::backward_checked(Invalid, 15), Some(Escape));
        assert_eq!(Step::backward_checked(BeginGroup, 2), None);
        assert_eq!(Step::backward_checked(BeginGroup, 1), Some(Escape));
        assert_eq!(Step::backward_checked(BeginGroup, 0), Some(BeginGroup));
        assert_eq!(Step::forward_checked(BeginGroup, 2), Some(MathShift));
        assert_eq!(Step::forward_checked(BeginGroup, 14), Some(Invalid));
        assert_eq!(Step::forward_checked(BeginGroup, 15), None);
    }

    #[test]
    fn token_bytes() {
        let token = Token::CS(ControlSequence::new_cwo("relax"));
        assert_eq!(Token::try_from_bytes(&token.to_bytes()), Ok(token));
        let token = Token::CS(ControlSequence::new_cwo("ok好的"));
        assert_eq!(Token::try_from_bytes(&token.to_bytes()), Ok(token));
        let token = Token::CS(ControlSequence::new_csy('?'));
        assert_eq!(Token::try_from_bytes(&token.to_bytes()), Ok(token));
        let token = Token::CS(ControlSequence::new_csy(' '));
        assert_eq!(Token::try_from_bytes(&token.to_bytes()), Ok(token));
        let token = Token::Char(Character::new('X', CatCode::Letter));
        assert_eq!(Token::try_from_bytes(&token.to_bytes()), Ok(token));
        let token = Token::Char(Character::new('号', CatCode::Other));
        assert_eq!(Token::try_from_bytes(&token.to_bytes()), Ok(token));
        let token = Token::Any(0x12345678);
        assert_eq!(Token::try_from_bytes(&token.to_bytes()), Ok(token));
        let token = Token::Any(0x812fa6ff);
        assert_eq!(Token::try_from_bytes(&token.to_bytes()), Ok(token));

        unsafe {
            let token = Token::CS(ControlSequence::new_cwo("relax"));
            assert_eq!(Token::from_bytes_unchecked(&token.to_bytes()), token);
            let token = Token::CS(ControlSequence::new_cwo("ok好的"));
            assert_eq!(Token::from_bytes_unchecked(&token.to_bytes()), token);
            let token = Token::CS(ControlSequence::new_csy('?'));
            assert_eq!(Token::from_bytes_unchecked(&token.to_bytes()), token);
            let token = Token::CS(ControlSequence::new_csy(' '));
            assert_eq!(Token::from_bytes_unchecked(&token.to_bytes()), token);
            let token = Token::Char(Character::new('X', CatCode::Letter));
            assert_eq!(Token::from_bytes_unchecked(&token.to_bytes()), token);
            let token = Token::Char(Character::new('号', CatCode::Other));
            assert_eq!(Token::from_bytes_unchecked(&token.to_bytes()), token);
            let token = Token::Any(0x12345678);
            assert_eq!(Token::from_bytes_unchecked(&token.to_bytes()), token);
            let token = Token::Any(0x812fa6ff);
            assert_eq!(Token::from_bytes_unchecked(&token.to_bytes()), token);
        }
    }
}
