use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::iter::Step;
use std::ops::RangeInclusive;
use std::str::Chars;
use std::{io, vec};

use bitflags::bitflags;
use compact_str::{
    format_compact, CompactString, CompactStringExt, ToCompactString,
};
use serde::{Deserialize, Deserializer, Serialize};
use smallvec::SmallVec;
use unicode_properties::{GeneralCategoryGroup, UnicodeGeneralCategory};

use crate::{
    range::MinMaxValue,
    tex::circumflex_mechanism,
    types::{CatCodeGetter, TokenBytes, TokenListBytes},
    ErrorKind,
};

bitflags! {
    #[allow(non_upper_case_globals)]
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
    /// A `CatCodeSet` with catcode 1-4, 6-8, 10-13 are set.
    pub fn tokenized() -> Self {
        CatCodeSet::from_bits_retain(0b0011_1101_1101_1110)
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
    /// Tag of a control sequence.
    /// 0: control word, 1: control symbol, 2: control space.
    pub fn tag(&self) -> u8 {
        unsafe { *self.csname.as_bytes().get_unchecked(0) }
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

/// Character with one or more catcode values.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct McCharacter {
    pub charcode: char,
    pub catcodes: CatCodeSet,
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

#[derive(Clone, Debug, PartialEq)]
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
    pub fn push<T: Into<Token>>(&mut self, token: T) {
        self.values.push(token.into());
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
                    cat = catcode.catcode_value(chr).unwrap_or_default();
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
impl AsRef<[Token]> for TokenList {
    fn as_ref(&self) -> &[Token] {
        &self.values
    }
}
impl std::ops::Deref for TokenList {
    type Target = [Token];
    fn deref(&self) -> &Self::Target {
        &self.values
    }
}
impl std::ops::DerefMut for TokenList {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.values
    }
}
