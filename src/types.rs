use hashers::fx_hash::FxHasher64;
use indexmap::{map::IntoIter as IndexMapIntoIter, IndexMap};
use log::{info, warn};
use std::cell::{Ref, RefCell, RefMut};
use std::convert::{Into, TryInto};
use std::default::Default;
use std::fmt::Debug;
use std::hash::{BuildHasherDefault, Hash, Hasher};
use std::iter::{Iterator, Step};
use std::rc::{Rc, Weak};
use std::str::{Chars, FromStr};
use std::{io, vec};

use crate::range::{parse_num, parse_num_range, try_get_char, NumberSpan};

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
}

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
            endline: '\x0D' as u32,
        }
    }
    pub fn is_empty(&self) -> bool {
        self.catcodes.is_empty()
    }
    pub fn len(&self) -> usize {
        self.catcodes.len()
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
    pub fn emplace_item<T: Into<NumberSpan<char>>>(&mut self, item: T, catcode: CatCode) {
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
                                    self.catcodes[start + added as usize] = catcode;
                                } else {
                                    let index = start + self.split_before(item_start).1 as usize;
                                    self.split_after(item_end); // split_after.1 === 0
                                    self.chars[index] = span_item;
                                    self.catcodes[index] = catcode;
                                }
                            }
                        } else {
                            let index = start + self.split_before(item_start).1 as usize;
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
                            self.chars[index] = (Step::forward_unchecked(at, 1)..=iv_end).into();
                        }
                        self.chars.insert(index, at.into());
                        return (1, 0);
                    } else if at == iv_end {
                        self.catcodes.insert(index, self.catcodes[index]);
                        self.chars[index] = at.into();
                        unsafe {
                            // iv_end > char::MIN, can be backward safely
                            self.chars
                                .insert(index, (iv_start..=Step::backward_unchecked(at, 1)).into());
                        }
                        return (1, 1);
                    } else {
                        self.catcodes.insert(index, self.catcodes[index]);
                        self.catcodes.insert(index, self.catcodes[index]);
                        unsafe {
                            // iv_start < at < iv_end, at can be backward and forward safely
                            self.chars[index] = (Step::forward_unchecked(at, 1)..=iv_end).into();
                            self.chars.insert(index, at.into());
                            self.chars
                                .insert(index, (iv_start..=Step::backward_unchecked(at, 1)).into());
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
                    self.chars[index] = (at..=self.chars[index].end()).into();
                    unsafe {
                        self.chars
                            .insert(index, (iv_start..=Step::backward_unchecked(at, 1)).into());
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
                        self.chars[index] = (Step::forward_unchecked(at, 1)..=iv_end).into();
                    }
                    self.chars.insert(index, (iv_start..=at).into());
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
                        self.chars[index] = (Step::forward_unchecked(at, 1)..=iv_end).into();
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
                    self.chars[index] =
                        (self.chars[index].start()..=self.chars[next_index].end()).into();
                } else {
                    let curr_end_forward = unsafe { Step::forward_unchecked(curr_end, 1) };
                    if curr_end_forward == self.chars[next_index].start() {
                        self.chars[index] =
                            (self.chars[index].start()..=self.chars[next_index].end()).into();
                    } else {
                        index += 1;
                        self.chars[index] = self.chars[next_index].clone();
                        // self.catcodes[index] = self.catcodes[next_index]; // no need
                    }
                }
            } else {
                let curr_start = unsafe { Step::forward_unchecked(self.chars[index].end(), 1) };
                index += 1;
                if curr_end >= self.chars[next_index].start() {
                    self.chars[next_index] = (curr_start..=self.chars[next_index].end()).into();
                }
                if index < next_index {
                    // index < next_index < len
                    self.chars[index] = (curr_start..=self.chars[next_index].end()).into();
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
        CTab {
            chars,
            catcodes,
            escape: u32::MAX,
            endline: u32::MAX,
        }
    }
    /// There is `iter`, no `iter_mut`.
    pub fn iter(&self) -> CTabIter {
        CTabIter {
            next: 0,
            ctab: self,
        }
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
    fn extend<T: IntoIterator<Item = (NumberSpan<char>, CatCode)>>(&mut self, iter: T) {
        for (item, catcode) in iter {
            self.emplace_item(item, catcode);
        }
    }
}

pub struct CTabSet {
    ctabs: IndexMap<String, Rc<RefCell<CTab>>, BuildHasherDefault<FxHasher64>>,
}

impl CTabSet {
    pub fn new() -> CTabSet {
        let mut ctabs = IndexMap::with_hasher(BuildHasherDefault::<FxHasher64>::default());
        let ctab = unsafe {
            CTab::from_iter_unchecked([
                (char::MIN..='\x1F', CatCode::Other),
                (' '..=' ', CatCode::Space),
                ('\x21'..=char::MAX, CatCode::Other),
            ])
        };
        ctabs.insert(String::from("other"), Rc::new(RefCell::new(ctab)));
        CTabSet { ctabs }
    }

    /// if name is in ctabset, return false, or put ctab in ctabset, return true
    pub fn add(&mut self, name: &str, ctab: CTab) -> bool {
        if matches!(name, "str" | "other" | "current") || self.ctabs.contains_key(name) {
            return false;
        }
        self.ctabs
            .insert(name.to_string(), Rc::new(RefCell::new(ctab)));
        true
    }

    pub fn put(&mut self, name: &str, ctab: CTab) -> bool {
        if matches!(name, "str" | "other" | "current") {
            return false;
        }
        self.ctabs
            .insert(name.to_string(), Rc::new(RefCell::new(ctab)));
        true
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
    pub fn iter_mut(&mut self) -> indexmap::map::IterMut<'_, String, Rc<RefCell<CTab>>> {
        self.ctabs.iter_mut()
    }

    // TODO: to be clean
    fn _parse(s: &str) -> Result<Self, ErrorKind> {
        fn set_e_e_char(key_str: &str, val_str: &str, ctab: &mut CTab, ignored_lines: &mut usize) {
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
        let mut name = String::new();
        let mut key_str = String::new();
        let mut val_str = String::new();

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
                            let prev_char = key_str.as_bytes()[key_str.len() - 1];
                            if prev_char == '`' as u8 || prev_char == '\\' as u8 {
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
                            let prev_char = key_str.as_bytes()[key_str.len() - 1];
                            if prev_char == '`' as u8 || (prev_char == '\\' as u8 && escape) {
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
                                    Err(_) => None,
                                };
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
                                    set_e_e_char(&key_str, &val_str, &mut ctab, &mut ignored_lines);
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
                Err(_) => None,
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
                set_e_e_char(&key_str, &val_str, &mut ctab, &mut ignored_lines);
            }
        }

        if !name.is_empty() {
            ctabset.put(&name.trim_ascii_end(), ctab);
        }

        if in_name {
            ignored_lines += 1;
        }
        if ignored_lines > 0 {
            warn!(target: "Parsing CTabSet", ">>--- Ignoring {} line(s) ---<<", ignored_lines);
        } else {
            info!(target: "Parsing CTabSet", ">>--- Ignoring {} line(s) ---<<", ignored_lines);
        }
        Ok(ctabset)
    }
}
pub struct CTabSetIntoIter {
    ctabset_iter: IndexMapIntoIter<String, Rc<RefCell<CTab>>>,
}
impl CTabSetIntoIter {
    pub fn new(ctabset: CTabSet) -> Self {
        CTabSetIntoIter {
            ctabset_iter: ctabset.ctabs.into_iter(),
        }
    }
}
impl Iterator for CTabSetIntoIter {
    type Item = (String, CTab);
    fn next(&mut self) -> Option<Self::Item> {
        match self.ctabset_iter.next() {
            Some((name, ctab)) => Some((name, Rc::into_inner(ctab).unwrap().into_inner())),
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
    /// <char num range> = <char num>..<char num> | <char num>
    /// <char num> := `<char> | `\<char> | '0o'[0-7]+ | '0x'[0-F]+ | [0-9]+
    /// 0b100 = 12
    /// 0x20 = 10
    /// 032 = 10
    /// `a = 11
    /// `\\ = 0
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

impl Default for CatCode {
    fn default() -> Self {
        CatCode::Other
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

impl TryFrom<&str> for CatCode {
    type Error = ErrorKind;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if let Ok(num) = value.parse::<i64>() {
            TryInto::<CatCode>::try_into(num)
        } else {
            match value.to_ascii_lowercase().as_str() {
                "escape" => Ok(CatCode::Escape),
                "begingroup" => Ok(CatCode::BeginGroup),
                "endgroup" => Ok(CatCode::EndGroup),
                "mathshift" => Ok(CatCode::MathShift),
                "alignment" => Ok(CatCode::Alignment),
                "endline" => Ok(CatCode::EndLine),
                "parameter" => Ok(CatCode::Parameter),
                "superscript" => Ok(CatCode::Superscript),
                "subscript" => Ok(CatCode::Subscript),
                "ignored" => Ok(CatCode::Ignored),
                "space" => Ok(CatCode::Space),
                "letter" => Ok(CatCode::Letter),
                "other" => Ok(CatCode::Other),
                "active" => Ok(CatCode::Active),
                "comment" => Ok(CatCode::Comment),
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

pub trait CatCodeGetter {
    fn catcode_value(&self, at: char) -> Option<CatCode>;
    fn escape_char(&self) -> Option<char>;
    fn endline_char(&self) -> Option<char>;
}

pub struct CTabFallbackable<'a, T: CatCodeGetter> {
    ctab: T,
    fallback: Option<Box<&'a dyn CatCodeGetter>>,
}

impl<'a1, T: CatCodeGetter> CTabFallbackable<'a1, T> {
    pub fn new<'a: 'a1>(ctab: T, fallback: Option<Box<&'a dyn CatCodeGetter>>) -> Self {
        CTabFallbackable::<'a1> { ctab, fallback }
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
impl CatCodeGetter for Ref<'_, CTab> {
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
impl CatCodeGetter for RefMut<'_, CTab> {
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
impl<'a, T: CatCodeGetter> CatCodeGetter for CTabFallbackable<'a, T> {
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

pub struct ControlSequence {
    csname: String,
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
            }
        }
    }
    /// Control Space.
    pub fn new_csp() -> Self {
        ControlSequence {
            csname: "\x02 ".to_string(),
        }
    }
    pub fn get_csname(&self) -> &str {
        unsafe { self.csname.get_unchecked(1..self.csname.len()) }
    }
    pub fn cs_with_escape_char(&self, escape_char: Option<char>) -> String {
        match escape_char {
            Some(chr) => format!("{}{}", chr, self.get_csname()),
            None => self.get_csname().to_string(),
        }
    }
    /// Write cs to stream, but do not process invisible and unprintable char.
    pub unsafe fn write<T: io::Write>(
        &self,
        escape_char: Option<char>,
        stream: &mut T,
    ) -> io::Result<()> {
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Character {
    charcode: char,
    catcode: CatCode,
}

impl Character {
    pub fn new(charcode: char, catcode: CatCode) -> Self {
        Character { charcode, catcode }
    }
    pub fn space() -> Self {
        Character {
            charcode: ' ',
            catcode: CatCode::Space,
        }
    }
    pub fn get_pairs(&self) -> (char, CatCode) {
        (self.charcode, self.catcode)
    }
    pub unsafe fn write<T: io::Write>(&self, stream: &mut T) -> io::Result<()> {
        if self.charcode == '\r' {
            write!(stream, "\n")?;
        } else {
            write!(stream, "{}", self.charcode)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum Token {
    CS(ControlSequence),
    Char(Character),
    Any(u32),
}
impl Token {
    pub fn is_cs(&self) -> bool {
        if let Token::CS(_) = self {
            true
        } else {
            false
        }
    }
    pub fn is_char(&self) -> bool {
        if let Token::Char(_) = self {
            true
        } else {
            false
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

#[derive(Debug)]
pub struct TokenList {
    values: Vec<Token>,
}

impl TokenList {
    pub fn new() -> Self {
        TokenList { values: Vec::new() }
    }
    pub fn parse<S: AsRef<str>, C: CatCodeGetter>(source: S, catcode: &C) -> Self {
        let endline = match catcode.endline_char() {
            Some(chr) => format!("{}", chr),
            None => "".to_string(),
        };
        let s = source.as_ref().lines().collect::<Vec<_>>().join(&endline);
        TokenList::_parse(s.chars(), catcode)
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
                Token::Any(chr) if *chr == '\n' as u32 || *chr == '\r' as u32 => {
                    write!(stream, "\n")?
                }
                Token::Any(_) => unreachable!(),
            };
        }
        Ok(())
    }

    fn _parse<C: CatCodeGetter>(mut source: Chars, catcode: &C) -> TokenList {
        let mut res = Vec::new();

        let mut cs_name = String::new();
        let mut collect_cs = false;

        while let Some(chr) = source.next() {
            let cat = catcode.catcode_value(chr).unwrap_or_default();
            if collect_cs {
                if cat == CatCode::Letter {
                    cs_name.push(chr);
                    continue;
                } else if cs_name.is_empty() {
                    res.push(Token::CS(ControlSequence::new_csy(chr)));
                    collect_cs = false;
                    continue;
                } else {
                    res.push(Token::CS(ControlSequence::new_cwo(&cs_name)));
                    cs_name.clear();
                    collect_cs = false;
                }
            }
            if cat == CatCode::Escape {
                collect_cs = true;
            } else {
                res.push(Token::Char(Character::new(chr, cat)));
            }
        }
        if !cs_name.is_empty() {
            let i_from = cs_name.floor_char_boundary(cs_name.len() - 1);
            let last_char = cs_name[i_from..cs_name.len()].chars().next().unwrap();
            if last_char == ' ' && cs_name.len() == 1 {
                res.push(Token::CS(ControlSequence::new_csp()));
            } else if catcode.catcode_value(last_char) == Some(CatCode::Letter) {
                res.push(Token::CS(ControlSequence::new_cwo(&cs_name)));
            } else {
                res.push(Token::CS(ControlSequence::new_csy(last_char)));
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
                ('#'..='#', Parameter),
                ('0'..='9', Other),
                ('A'..='Z', Letter),
                ('L'..='a', Letter),
                ('Y'..='d', Other),
                ('e'..='z', Letter),
                ('\u{4E00}'..='\u{9FA5}', Letter),
                ('\u{5E00}'..='\u{5F00}', Other),
                ('\u{9F00}'..='\u{9FFF}', Active),
            ])
        };
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('#'..='#'),
                NumberSpan::from('0'..='9'),
                NumberSpan::from('A'..='Z'),
                NumberSpan::from('L'..='a'),
                NumberSpan::from('Y'..='d'),
                NumberSpan::from('e'..='z'),
                NumberSpan::from('\u{4E00}'..='\u{9FA5}'),
                NumberSpan::from('\u{5E00}'..='\u{5F00}'),
                NumberSpan::from('\u{9F00}'..='\u{9FFF}')
            ]
        );
        assert_eq!(
            &ctab.catcodes,
            &vec![Parameter, Other, Letter, Letter, Other, Letter, Letter, Other, Active]
        );
        ctab._normalize();
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('#'..='#'),
                NumberSpan::from('0'..='9'),
                NumberSpan::from('A'..='a'),
                NumberSpan::from('b'..='d'),
                NumberSpan::from('e'..='z'),
                NumberSpan::from('\u{4E00}'..='\u{9FA5}'),
                NumberSpan::from('\u{9FA6}'..='\u{9FFF}')
            ]
        );
        assert_eq!(
            &ctab.catcodes,
            &vec![Parameter, Other, Letter, Other, Letter, Letter, Active]
        );

        ctab = unsafe {
            CTab::from_iter_unchecked([
                ('#'..='#', Parameter),
                ('0'..='9', Other),
                ('A'..='Z', Letter),
                ('L'..='a', Letter),
                ('Y'..='d', Letter),
                ('e'..='z', Letter),
                ('\u{4E00}'..='\u{9FA5}', Letter),
                ('\u{5E00}'..='\u{5F00}', Other),
                ('\u{9F00}'..='\u{9FFF}', Letter),
            ])
        };
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('#'..='#'),
                NumberSpan::from('0'..='9'),
                NumberSpan::from('A'..='Z'),
                NumberSpan::from('L'..='a'),
                NumberSpan::from('Y'..='d'),
                NumberSpan::from('e'..='z'),
                NumberSpan::from('\u{4E00}'..='\u{9FA5}'),
                NumberSpan::from('\u{5E00}'..='\u{5F00}'),
                NumberSpan::from('\u{9F00}'..='\u{9FFF}')
            ]
        );
        assert_eq!(
            &ctab.catcodes,
            &vec![Parameter, Other, Letter, Letter, Letter, Letter, Letter, Other, Letter]
        );
        ctab._normalize();
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('#'..='#'),
                NumberSpan::from('0'..='9'),
                NumberSpan::from('A'..='z'),
                NumberSpan::from('\u{4E00}'..='\u{9FFF}')
            ]
        );
        assert_eq!(&ctab.catcodes, &vec![Parameter, Other, Letter, Letter]);

        ctab = unsafe {
            CTab::from_iter_unchecked([
                ('#'..='#', Parameter),
                ('0'..='9', Other),
                ('A'..='z', Letter),
                ('\u{4E00}'..='\u{9FA5}', Letter),
            ])
        };
        ctab._normalize();
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('#'..='#'),
                NumberSpan::from('0'..='9'),
                NumberSpan::from('A'..='z'),
                NumberSpan::from('\u{4E00}'..='\u{9FA5}')
            ]
        );
        assert_eq!(&ctab.catcodes, &vec![Parameter, Other, Letter, Letter]);

        ctab = unsafe {
            CTab::from_iter_unchecked([
                ('#'..='#', Parameter),
                ('0'..='9', Other),
                ('A'..='z', Letter),
                ('\u{4E00}'..='\u{10FFFF}', Letter),
                ('\u{10FFF0}'..='\u{10FFF0}', Letter),
            ])
        };
        ctab._normalize();
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('#'..='#'),
                NumberSpan::from('0'..='9'),
                NumberSpan::from('A'..='z'),
                NumberSpan::from('\u{4E00}'..='\u{10FFFF}')
            ]
        );
        assert_eq!(&ctab.catcodes, &vec![Parameter, Other, Letter, Letter]);

        ctab = unsafe {
            CTab::from_iter_unchecked([('#'..='#', Letter), ('\u{10FFFF}'..='\u{10FFFF}', Letter)])
        };
        ctab._normalize();
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('#'..='#'),
                NumberSpan::from('\u{10FFFF}'..='\u{10FFFF}')
            ]
        );
        assert_eq!(&ctab.catcodes, &vec![Letter, Letter]);

        ctab = unsafe { CTab::from_iter_unchecked([(char::MIN..=char::MAX, Other)]) };
        assert_eq!(&ctab.chars, &vec![NumberSpan::from(char::MIN..=char::MAX)]);
        assert_eq!(&ctab.catcodes, &vec![Other]);

        ctab = CTab::new();
        ctab._normalize();
        assert_eq!(&ctab.chars, &vec![]);
        assert_eq!(&ctab.catcodes, &vec![]);

        ctab = unsafe {
            CTab::from_iter_unchecked([
                ('#'..='#', Parameter),
                ('0'..='9', Other),
                ('A'..='z', Letter),
                ('\u{2000}'..='\u{5000}', Other),
                ('\u{5001}'..='\u{8FFF}', Letter),
                ('\u{9000}'..='\u{10000}', Active),
                ('\u{9FA6}'..='\u{10000}', Other),
                ('\u{10001}'..='\u{10FFFF}', Other),
            ])
        };
        ctab._normalize();
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('#'..='#'),
                NumberSpan::from('0'..='9'),
                NumberSpan::from('A'..='z'),
                NumberSpan::from('\u{2000}'..='\u{5000}'),
                NumberSpan::from('\u{5001}'..='\u{8FFF}'),
                NumberSpan::from('\u{9000}'..='\u{10000}'),
                NumberSpan::from('\u{10001}'..='\u{10FFFF}')
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
                ('#'..='#', Parameter),
                ('0'..='9', Other),
                ('A'..='z', Letter),
                ('\u{4E00}'..='\u{10FFFF}', Other),
            ])
        };
        ctab.emplace_item('\u{4E00}'..='\u{9FA5}', Letter);
        assert_eq!(ctab.iter().count(), 5);
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('#'..='#'),
                NumberSpan::from('0'..='9'),
                NumberSpan::from('A'..='z'),
                NumberSpan::from('\u{4E00}'..='\u{9FA5}'),
                NumberSpan::from('\u{9FA6}'..='\u{10FFFF}')
            ]
        );
        assert_eq!(
            &ctab.catcodes,
            &vec![Parameter, Other, Letter, Letter, Other]
        );

        ctab.emplace_item('\x5B'..='\x60', Other);
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('#'..='#'),
                NumberSpan::from('0'..='9'),
                NumberSpan::from('A'..='Z'),
                NumberSpan::from('\x5B'..='\x60'),
                NumberSpan::from('a'..='z'),
                NumberSpan::from('\u{4E00}'..='\u{9FA5}'),
                NumberSpan::from('\u{9FA6}'..='\u{10FFFF}')
            ]
        );
        assert_eq!(
            &ctab.catcodes,
            &vec![Parameter, Other, Letter, Other, Letter, Letter, Other]
        );

        ctab.emplace_item('\x5B'..='\x60', Letter);
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('#'..='#'),
                NumberSpan::from('0'..='9'),
                NumberSpan::from('A'..='z'),
                NumberSpan::from('\u{4E00}'..='\u{9FA5}'),
                NumberSpan::from('\u{9FA6}'..='\u{10FFFF}')
            ]
        );
        assert_eq!(
            &ctab.catcodes,
            &vec![Parameter, Other, Letter, Letter, Other]
        );

        ctab.emplace_item('\u{2000}'..='\u{5000}', Other);
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('#'..='#'),
                NumberSpan::from('0'..='9'),
                NumberSpan::from('A'..='z'),
                NumberSpan::from('\u{2000}'..='\u{5000}'),
                NumberSpan::from('\u{5001}'..='\u{9FA5}'),
                NumberSpan::from('\u{9FA6}'..='\u{10FFFF}')
            ]
        );
        assert_eq!(
            &ctab.catcodes,
            &vec![Parameter, Other, Letter, Other, Letter, Other]
        );
        ctab.emplace_item('\u{9000}'..='\u{10000}', Active);
        assert_eq!(
            &ctab.chars,
            &vec![
                NumberSpan::from('#'..='#'),
                NumberSpan::from('0'..='9'),
                NumberSpan::from('A'..='z'),
                NumberSpan::from('\u{2000}'..='\u{5000}'),
                NumberSpan::from('\u{5001}'..='\u{8FFF}'),
                NumberSpan::from('\u{9000}'..='\u{10000}'),
                NumberSpan::from('\u{10001}'..='\u{10FFFF}')
            ]
        );
        assert_eq!(
            &ctab.catcodes,
            &vec![Parameter, Other, Letter, Other, Letter, Active, Other]
        );
    }

    #[test]
    fn ctabset_parse() {
        use CatCode::*;
        let ctabset_str = r#"
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
    }

    #[test]
    fn ctab_fallbackable() {
        use CatCode::*;
        let ctabset = CTabSet::from_str(
            r#"
        [initex]
        10 = 5  /* <return> */
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
        "#,
        )
        .unwrap();

        assert_eq!(ctabset.get_escape_char("initex"), Some('\\'));
        assert_eq!(ctabset.get_endline_char("initex"), Some('\x0D'));

        {
            let initex = ctabset
                .get_by_name_mut("initex")
                .expect("parse error! no [initex]");
            let latex = ctabset
                .get_by_name("latex")
                .expect("parse error! no [latex]");
            let latex3 = ctabset
                .get_by_name("latex3")
                .expect("parse error! no [latex3]");
            let initex_fb = CTabFallbackable::new(initex, None);
            let latex_fb = CTabFallbackable::new(latex, Some(Box::new(&initex_fb)));
            let latex3_fb = CTabFallbackable::new(latex3, Some(Box::new(&latex_fb)));

            assert_eq!(initex_fb.catcode_value('\n'), Some(EndLine));
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
            assert_eq!(latex_fb.catcode_value('\n'), Some(EndLine));
            assert_eq!(latex_fb.catcode_value(' '), Some(Space));

            assert_eq!(latex3_fb.catcode_value(':'), Some(Letter));
            assert_eq!(latex3_fb.catcode_value('_'), Some(Letter));
            assert_eq!(latex3_fb.catcode_value(' '), Some(Ignored));

            assert_eq!(ctabset.get_escape_char("latex"), Some('\\'));
            assert_eq!(ctabset.get_endline_char("latex"), Some('\x0D'));
        }

        assert_eq!(ctabset.get_escape_char("latex3"), Some('\\'));
        assert_eq!(ctabset.get_endline_char("latex3"), Some('\x0D'));

        assert_eq!(ctabset.get_escape_char("None"), None);
        assert_eq!(ctabset.get_endline_char("None"), None);

        for (name, ctab) in ctabset {
            println!("Get: name={}, len={}", name, ctab.len());
        }
    }
}