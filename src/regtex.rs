use compact_str::{format_compact, CompactString, ToCompactString};
use memchr;
use regex::{self, bytes};
use regex_syntax::{
    ast::{AssertionKind, ClassPerlKind},
    is_escapeable_character, is_meta_character,
};
use smallvec::SmallVec;
use std::{
    cell::Cell,
    fmt::{Debug, Display},
    sync::Arc,
};

use crate::types::{CatCode, CatCodeSet as CCSet, TokenListBytes, TokenType};

#[derive(Debug, PartialEq)]
#[non_exhaustive]
pub enum Error {
    CaptureNamedP,
    Internal(regex::Error),
    Invalid,
    InvalidAlternation,
    InvalidEscaped(char),
    InvalidFlag(char),
    InvalidGroup,
    InvalidMeta(char),
    InvalidRepetition,
    InvalidSet,
    InvalidSetEscaped(char),
    InvalidWordBoundary(Option<String>),
    NeedEscape(char),
    TooDeepSet,
    UnclosedGroup,
    UnclosedRange,
    UnclosedSet,
    UnsupprotedMeta(char),
    EOF,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::CaptureNamedP => format!(
                    "Unsupported p-type named capture for RegTEx, use <capture name> instead"
                ),
                Self::Internal(e) => e.to_string(),
                Self::Invalid => format!("Invalid RegTEx pattern"),
                Self::InvalidAlternation => format!("Invalid RegTEx alternation"),
                Self::InvalidEscaped(c) => format!("Invalid RegTEx escaped sequence \\{}", c),
                Self::InvalidFlag(c) => format!("Invalid RegTEx flag '{}'", c),
                Self::InvalidGroup => format!("Invalid RegTEx group pattern"),
                Self::InvalidMeta(c) => format!("Invalid RegTEx escaped char \\{}", c),
                Self::InvalidRepetition => format!("Invalid RegTEx repetition pattern"),
                Self::InvalidSet => format!("Invalid RegTEx set pattern"),
                Self::InvalidSetEscaped(c) => format!("Invalid RegTEx escaped char \\{} in set", c),
                Self::InvalidWordBoundary(c) => match c {
                    Some(s) => format!("Invalid RegTEx word boundary {s}"),
                    None => format!("Invalid RegTEx word boundary"),
                },
                Self::NeedEscape(c) => format!(
                    "The character {} is supposed to be escaped in RegTEx pattern",
                    c
                ),
                Self::TooDeepSet => format!("The set is nested too deep in RegTEx pattern"),
                Self::UnclosedGroup => format!("Unclosed group of the RegTEx pattern"),
                Self::UnclosedRange => format!("Unclosed character range of the RegTEx pattern"),
                Self::UnclosedSet => format!("Unclosed set of the RegTEx pattern"),
                Self::UnsupprotedMeta(c) =>
                    format!("The character {} cannot be escaped in RegTEx pattern", c),
                Self::EOF => format!("Touched an EOF in RegTEx pattern, this should be a bug"),
            }
        )
    }
}

/// Regular expression for TeX tokens.
pub struct RegTEx {
    regex: bytes::Regex,
    pattern: Arc<str>,
}
impl Debug for RegTEx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            let mut dl = f.debug_tuple("RegTEx");
            dl.field_with(|fi| write!(fi, "Raw: '{:#}'", self.as_str()));
            dl.field_with(|fi| {
                write!(fi, "Internal: '{:#}'", self.regex.as_str())
            });
            dl.finish()
        } else {
            write!(
                f,
                "RegTEx(Raw: '{}', Internal: '{}')",
                self.as_str(),
                self.regex.as_str()
            )
        }
    }
}
impl RegTEx {
    pub fn new(s: &str) -> Result<Self, Error> {
        let regex_pattern = RegTExBuilder::new(s).build()?;
        let regex = bytes::Regex::new(&regex_pattern)
            .map_err(|e| Error::Internal(e))?;
        Ok(RegTEx { regex, pattern: Arc::from(s) })
    }
    pub fn as_str(&self) -> &str {
        &self.pattern
    }

    pub fn is_match(&self, tokenlist: &TokenListBytes) -> bool {
        self.regex.is_match(tokenlist.as_bytes())
    }
    pub fn captures<'a, 'b>(
        &'a self,
        tokenlist: &'b TokenListBytes,
    ) -> Option<bytes::Captures<'b>> {
        self.regex.captures(tokenlist.as_bytes())
    }
    pub fn find<'a, 'b>(
        &'a self,
        tokenlist: &'b TokenListBytes,
    ) -> Option<bytes::Match<'b>> {
        self.regex.find(tokenlist.as_bytes())
    }
    pub fn is_match_at(
        &self,
        tokenlist: &TokenListBytes,
        start: usize,
    ) -> bool {
        self.regex
            .is_match_at(tokenlist.as_bytes(), tokenlist.bytes_index(start))
    }
    pub unsafe fn is_match_bytes(&self, tokenlist: &[u8]) -> bool {
        self.regex.is_match(tokenlist)
    }
    pub unsafe fn captures_bytes<'a, 'b>(
        &'a self,
        tokenlist: &'b [u8],
    ) -> Option<bytes::Captures<'b>> {
        self.regex.captures(tokenlist)
    }
    pub unsafe fn find_bytes<'a, 'b>(
        &'a self,
        tokenlist: &'b [u8],
    ) -> Option<bytes::Match<'b>> {
        self.regex.find(tokenlist)
    }
}

/// Regular expression set for TeX tokens.
pub struct RegTExSet {
    regex_set: bytes::RegexSet,
    patterns: Arc<[String]>,
}
impl Debug for RegTExSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            let mut dl = f.debug_tuple("RegTExSet");
            dl.field_with(|fi| write!(fi, "Raw: '{:#?}'", self.patterns()));
            dl.field_with(|fi| {
                write!(fi, "Internal: '{:#?}'", self.regex_set.patterns())
            });
            dl.finish()
        } else {
            write!(
                f,
                "RegTExSet(Raw: '{:?}', Internal: '{:?}')",
                self.patterns(),
                self.regex_set.patterns()
            )
        }
    }
}
impl RegTExSet {
    pub fn new<I, S>(s: I) -> Result<Self, Error>
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
    {
        let mut patterns = Vec::new();
        for pattern in s.into_iter() {
            let regex_pattern =
                RegTExBuilder::new(pattern.as_ref()).build()?;
            patterns.push(regex_pattern);
        }
        let regex_set =
            bytes::RegexSet::new(&patterns).map_err(|e| Error::Internal(e))?;
        Ok(RegTExSet { regex_set, patterns: Arc::from(patterns) })
    }
    pub fn len(&self) -> usize {
        self.patterns.len()
    }
    pub fn patterns(&self) -> &[String] {
        &self.patterns
    }

    pub fn is_empty(&self) -> bool {
        self.regex_set.is_empty()
    }

    pub fn is_match(&self, tokenlist: &TokenListBytes) -> bool {
        self.regex_set.is_match(tokenlist.as_bytes())
    }
    /// Return true if and only if one of the regexes in this set
    /// matches the haystack given, with the search starting at the offset given.
    /// This panics when start > length of tokens.
    pub fn is_match_at(
        &self,
        tokenlist: &TokenListBytes,
        start: usize,
    ) -> bool {
        self.regex_set
            .is_match_at(tokenlist.as_bytes(), tokenlist.bytes_index(start))
    }
    pub unsafe fn is_match_bytes(&self, tokenlist: &[u8]) -> bool {
        self.regex_set.is_match(tokenlist)
    }
}

struct RegTExBuilder<'s> {
    pattern: &'s str,
    offset: Cell<usize>,
    in_set: Cell<usize>,
    set_negated: Cell<u32>,
    ending: Cell<Option<char>>,
}
impl PartialEq for RegTExBuilder<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.pattern.as_ptr() == other.pattern.as_ptr()
            && self.in_set == other.in_set
            && self.set_negated == other.set_negated
    }
}

trait ToPattern {
    fn to_raw(&self, pattern: &str) -> CompactString;
    fn to_pattern(&self, pattern: &str) -> String;
}

impl ToPattern for Flags {
    fn to_raw(&self, _: &str) -> CompactString {
        self.flags
            .iter()
            .map(|flag| match flag {
                FlagKind::CaseInsensitive => 'i',
                FlagKind::Negative => '-',
            })
            .collect()
    }
    fn to_pattern(&self, pattern: &str) -> String {
        self.to_raw(pattern).to_string()
    }
}
impl ToPattern for LiteralKind {
    fn to_raw(&self, _: &str) -> CompactString {
        match *self {
            Self::Meta(chr) => format_compact!("\\{}", chr as char),
            Self::Superfluous(chr) => {
                format_compact!(
                    "{}{}",
                    if is_escapeable_character(chr) { "\\" } else { "" },
                    chr
                )
            }
            Self::Verbatim(chr) => {
                format_compact!(
                    "{}{}",
                    if is_meta_character(chr) { "\\" } else { "" },
                    chr
                )
            }
            Self::X(chr) => format_compact!("\\x{{{:x}}}", chr as u32),
        }
    }
    fn to_pattern(&self, pattern: &str) -> String {
        tagged_char(.., &self.to_raw(pattern))
    }
}
impl ToPattern for Assertion {
    fn to_raw(&self, _: &str) -> CompactString {
        match self {
            Self::EndLine => r"$",
            Self::EndText => r"\z",
            Self::NotWordBoundary => r"\B",
            Self::StartLine => r"^",
            Self::StartText => r"\A",
            Self::WordBoundary => r"\b",
            Self::WordBoundaryEnd => r"\b{end}",
            Self::WordBoundaryEndAngle => r"\>",
            Self::WordBoundaryEndHalf => r"\\b{end-half}",
            Self::WordBoundaryStart => r"\b{start}",
            Self::WordBoundaryStartAngle => r"\<",
            Self::WordBoundaryStartHalf => r"\b{start-half}",
        }
        .to_compact_string()
    }
    fn to_pattern(&self, pattern: &str) -> String {
        if matches!(
            *self,
            Self::WordBoundaryEnd
                | Self::WordBoundaryEndAngle
                | Self::WordBoundaryEndHalf
                | Self::WordBoundaryStart
                | Self::WordBoundaryStartAngle
                | Self::WordBoundaryStartHalf
        ) {
            unimplemented!("Assertion of {:?} is not supported", self)
        } else {
            format!("(?u:{})", self.to_raw(pattern))
        }
    }
}
impl ToPattern for ClassUnicode {
    fn to_raw(&self, pattern: &str) -> CompactString {
        format_compact!(
            "\\{}{}",
            if self.negated { 'P' } else { 'p' },
            &pattern[self.span.start .. self.span.end]
        )
    }
    fn to_pattern(&self, pattern: &str) -> String {
        tagged_char(.., &self.to_raw(pattern))
    }
}
impl ToPattern for ClassPerl {
    fn to_raw(&self, _: &str) -> CompactString {
        let negated = self.negated;
        format_compact!(
            "\\{}",
            match self.kind {
                ClassPerlKind::Digit =>
                    if negated {
                        'D'
                    } else {
                        'd'
                    },
                ClassPerlKind::Word =>
                    if negated {
                        'W'
                    } else {
                        'w'
                    },
                ClassPerlKind::Space =>
                    if negated {
                        'S'
                    } else {
                        's'
                    },
            }
        )
    }
    fn to_pattern(&self, pattern: &str) -> String {
        tagged_char(.., &self.to_raw(pattern))
    }
}
impl ToPattern for ClassBracketed {
    fn to_raw(&self, pattern: &str) -> CompactString {
        format_compact!(
            "[{}{}]",
            if self.negated { "^" } else { "" },
            self.kind.to_raw(pattern)
        )
    }
    fn to_pattern(&self, pattern: &str) -> String {
        if self.kind.has_exact() {
            let (set_item, exacts) = self.kind.move_exacts();
            let mut is_simple_exact = true; // iff all exacts are not in negated set
            for exact in exacts.iter() {
                if let ClassSetItem::Exact(e) = exact {
                    if e.acc_negated {
                        is_simple_exact = false;
                        break;
                    }
                }
            }

            if is_simple_exact {
                let mut res = String::from("(?:");
                let has_item = if let Some(item) = set_item {
                    let b_moved = ClassBracketed {
                        negated: self.negated,
                        acc_negated: self.acc_negated,
                        kind: item,
                    };
                    res.push_str(&tagged_char(.., &b_moved.to_raw(pattern)));
                    true
                } else {
                    false
                };
                if !exacts.is_empty() {
                    if has_item {
                        res.push('|');
                    }
                    let exacts_pattern = exacts
                        .iter()
                        .map(|v| {
                            if let ClassSetItem::Exact(e) = v {
                                match &e.kind {
                                    ExactKind::CS(cs) => tagged_cs(
                                        ..,
                                        &pattern[cs.start .. cs.end],
                                    ),
                                    ExactKind::CatCode(cc) => {
                                        cc.to_tagged_pattern(pattern)
                                    }
                                }
                            } else {
                                unreachable!("ExactKind can never be there")
                            }
                        })
                        .collect::<Vec<_>>()
                        .join("|");
                    res.push_str(&exacts_pattern);
                }
                res.push_str(")");
                res
            } else {
                unimplemented!(
                    "Only simple Exact matching is supported for now"
                )
            }
        } else {
            tagged_char(.., &self.to_raw(pattern))
        }
    }
}
impl ToPattern for ClassSetItem {
    fn to_raw(&self, pattern: &str) -> CompactString {
        match self {
            Self::Empty => CompactString::new(""),
            Self::Literal(l) => l.to_raw(pattern),
            Self::Range(r) => {
                format_compact!(
                    "{}-{}",
                    r.start.to_raw(pattern),
                    r.end.to_raw(pattern)
                )
            }
            Self::Ascii(s) => {
                format_compact!("[{}]", &pattern[s.start .. s.end])
            }
            Self::Unicode(u) => u.to_raw(pattern),
            Self::Perl(p) => p.to_raw(pattern),
            Self::Bracketed(b) => b.to_raw(pattern),
            Self::Exact(e) => e.to_raw(pattern),
            Self::Union(u) => u.to_raw(pattern),
        }
    }
    fn to_pattern(&self, pattern: &str) -> String {
        match self {
            Self::Empty => String::new(),
            Self::Literal(l) => l.to_pattern(pattern),
            Self::Range(r) => tagged_char(
                ..,
                &format!(
                    "{}-{}",
                    r.start.to_raw(pattern),
                    r.end.to_raw(pattern)
                ),
            ),
            Self::Ascii(s) => {
                tagged_char(.., &format!("[{}]", &pattern[s.start .. s.end]))
            }
            Self::Unicode(u) => u.to_pattern(pattern),
            Self::Perl(p) => p.to_pattern(pattern),
            Self::Bracketed(b) => b.to_pattern(pattern),
            Self::Exact(e) => e.to_pattern(pattern),
            Self::Union(u) => u.to_pattern(pattern),
        }
    }
}
impl ToPattern for ClassSetUnion {
    fn to_raw(&self, pattern: &str) -> CompactString {
        self.items.iter().map(|i| i.to_raw(pattern)).collect::<CompactString>()
    }
    fn to_pattern(&self, pattern: &str) -> String {
        self.items
            .iter()
            .map(|v| v.to_pattern(pattern))
            .collect::<Vec<_>>()
            .join("|")
    }
}
impl ToPattern for Repetition {
    fn to_raw(&self, pattern: &str) -> CompactString {
        format_compact!(
            "{}{}",
            self.ast.to_raw(pattern),
            &pattern[self.op.start .. self.op.end]
        )
    }
    fn to_pattern(&self, pattern: &str) -> String {
        format!(
            "(?:{}){}",
            self.ast.to_pattern(pattern),
            &pattern[self.op.start .. self.op.end]
        )
    }
}
impl ToPattern for Group {
    fn to_raw(&self, pattern: &str) -> CompactString {
        format_compact!(
            "({}{})",
            match &self.kind {
                GroupKind::CaptureIndex => CompactString::new(""),
                GroupKind::CaptureNamed(s) =>
                    format_compact!("?{}", &pattern[s.start .. s.end]),
                GroupKind::NoCapturing(f) =>
                    format_compact!("?{}:", f.to_raw(pattern)),
            },
            self.ast.to_raw(pattern),
        )
    }
    fn to_pattern(&self, pattern: &str) -> String {
        format!(
            "({}{})",
            match &self.kind {
                GroupKind::CaptureIndex => CompactString::new(""),
                GroupKind::CaptureNamed(s) =>
                    format_compact!("?{}", &pattern[s.start .. s.end]),
                GroupKind::NoCapturing(f) =>
                    format_compact!("?{}:", f.to_pattern(pattern)),
            },
            self.ast.to_pattern(pattern),
        )
    }
}
impl ToPattern for Alternation {
    fn to_raw(&self, pattern: &str) -> CompactString {
        let mut res = CompactString::new("");
        let mut iter = self.asts.iter();
        match iter.next() {
            Some(ast) => res.push_str(ast.to_raw(pattern).as_ref()),
            None => {}
        }
        while let Some(ast) = iter.next() {
            res.push('|');
            res.push_str(ast.to_raw(pattern).as_ref());
        }
        res
    }
    fn to_pattern(&self, pattern: &str) -> String {
        let mut res = String::new();
        let mut iter = self.asts.iter();
        match iter.next() {
            Some(ast) => res.push_str(&ast.to_pattern(pattern)),
            None => {}
        }
        while let Some(ast) = iter.next() {
            res.push('|');
            res.push_str(&ast.to_pattern(pattern));
        }
        res
    }
}
impl ToPattern for Concat {
    fn to_raw(&self, pattern: &str) -> CompactString {
        self.asts.iter().map(|ast| ast.to_raw(pattern)).collect()
    }
    fn to_pattern(&self, pattern: &str) -> String {
        self.asts.iter().map(|ast| ast.to_pattern(pattern)).collect()
    }
}
impl ToPattern for Exact {
    fn to_raw(&self, pattern: &str) -> CompactString {
        match &self.kind {
            ExactKind::CS(s) => {
                format_compact!("\\c{{{}}}", &pattern[s.start .. s.end])
            }
            ExactKind::CatCode(c) => format_compact!(
                "\\c[{}{}]{}",
                if c.negated { "^" } else { "" },
                c.catcodes
                    .into_catcode_iter()
                    .map(|c| c.char() as char)
                    .collect::<CompactString>(),
                c.ast.to_raw(pattern)
            ),
        }
    }
    fn to_pattern(&self, pattern: &str) -> String {
        if self.acc_negated {
            unimplemented!("Only simple Exact matching is supported for now")
        } else {
            match &self.kind {
                ExactKind::CS(s) => tagged_cs(.., &pattern[s.start .. s.end]),
                ExactKind::CatCode(c) => {
                    format!("(?:{})", c.to_tagged_pattern(pattern))
                }
            }
        }
    }
}
impl ToPattern for Ast {
    fn to_raw(&self, pattern: &str) -> CompactString {
        match self {
            Self::Empty => CompactString::const_new(""),
            Self::Flags(f) => format_compact!("(?{})", f.to_raw(pattern)),
            Self::Literal(l) => l.to_raw(pattern),
            Self::Dot => CompactString::const_new("."),
            Self::Assertion(a) => a.to_raw(pattern),
            Self::ClassUnicode(u) => u.to_raw(pattern),
            Self::ClassPerl(p) => p.to_raw(pattern),
            Self::ClassBracketed(b) => b.to_raw(pattern),
            Self::Repetition(r) => r.to_raw(pattern),
            Self::Group(g) => g.to_raw(pattern),
            Self::Alternation(a) => a.to_raw(pattern),
            Self::Concat(c) => c.to_raw(pattern),
            Self::Exact(e) => e.to_raw(pattern),
            Self::Raw(s) => format_compact!("{}", &pattern[s.start .. s.end]),
        }
    }
    fn to_pattern(&self, pattern: &str) -> String {
        match self {
            Self::Empty => String::new(),
            Self::Flags(f) => format!("(?{})", f.to_raw(pattern)),
            Self::Literal(l) => l.to_pattern(pattern),
            Self::Dot => String::from(r"[^\xFF]+\xFF"),
            Self::Assertion(a) => a.to_pattern(pattern),
            Self::ClassUnicode(u) => u.to_pattern(pattern),
            Self::ClassPerl(p) => p.to_pattern(pattern),
            Self::ClassBracketed(b) => b.to_pattern(pattern),
            Self::Repetition(r) => r.to_pattern(pattern),
            Self::Group(g) => g.to_pattern(pattern),
            Self::Alternation(a) => a.to_pattern(pattern),
            Self::Concat(c) => c.to_pattern(pattern),
            Self::Exact(e) => e.to_pattern(pattern),
            Self::Raw(s) => format!("{}", &pattern[s.start .. s.end]),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Ast {
    Empty,
    Flags(Box<Flags>),                   // (? )
    Literal(Box<LiteralKind>),           // a \^ \% \n \x \u ...
    Dot,                                 // .
    Assertion(Box<Assertion>), // ^ $ \A \z(\Z) \b \B \b{start} \< \b{end} \> \b{start-half} \b{end-half}
    ClassUnicode(Box<ClassUnicode>), // \p \P
    ClassPerl(Box<ClassPerl>), // \d \D \w \W \s \S
    ClassBracketed(Box<ClassBracketed>), // []
    Repetition(Box<Repetition>), // * ? + {m} {m,} {m,n}
    Group(Box<Group>),         // () (?<> ) (?:)
    Alternation(Box<Alternation>), // x|y
    Concat(Box<Concat>),       // xy
    Exact(Box<Exact>),         // \c
    Raw(Span),                 // a Regex pattern
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Span {
    start: usize, // inclusive
    end: usize,   // exclusive
}
impl From<(usize, usize)> for Span {
    fn from((start, end): (usize, usize)) -> Self {
        Span { start, end }
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
struct Flags {
    flags: Vec<FlagKind>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
enum FlagKind {
    Negative,
    CaseInsensitive,
}
#[derive(Debug, Clone, PartialEq, Eq)]
enum LiteralKind {
    X(char),
    Meta(u8),
    Verbatim(char),
    Superfluous(char),
}
type Assertion = AssertionKind;
#[derive(Debug, Clone, PartialEq, Eq)]
struct ClassUnicode {
    negated: bool,
    span: Span,
}
#[derive(Debug, Clone, PartialEq, Eq)]
struct ClassPerl {
    negated: bool,
    kind: ClassPerlKind,
}
#[derive(Debug, Clone, PartialEq, Eq)]
struct ClassBracketed {
    negated: bool,
    /// The accumulated negated value by now.
    /// If current item is set, this negated value is not counted.
    acc_negated: bool,
    kind: ClassSetItem,
}
#[derive(Debug, Clone, PartialEq, Eq)]
enum ClassSetItem {
    Empty,
    Literal(LiteralKind),
    Range(ClassSetRange),
    Ascii(Span),
    Unicode(ClassUnicode),
    Perl(ClassPerl),
    Bracketed(Box<ClassBracketed>),
    Exact(Exact),
    Union(ClassSetUnion),
}
impl ClassSetItem {
    fn has_exact(&self) -> bool {
        match self {
            Self::Bracketed(b) => b.kind.has_exact(),
            Self::Exact(_) => true,
            Self::Union(u) => u.items.iter().any(|v| v.has_exact()),
            _ => false,
        }
    }
    fn move_exacts(
        &self,
    ) -> (Option<ClassSetItem>, SmallVec<[ClassSetItem; 4]>) {
        let mut exacts = SmallVec::new();
        let mut item = None;
        match self {
            Self::Exact(_) => exacts.push(self.clone()),
            Self::Bracketed(b) => {
                let (item_inner, exacts_inner) = b.kind.move_exacts();
                item = item_inner.and_then(|v| {
                    let bracketed = ClassBracketed {
                        negated: b.negated,
                        acc_negated: b.acc_negated,
                        kind: v,
                    };
                    Some(ClassSetItem::Bracketed(Box::new(bracketed)))
                });
                exacts.extend(exacts_inner);
            }
            Self::Union(u) => {
                let mut items = vec![];
                for un in u.items.iter() {
                    let (item_inner, exacts_inner) = un.move_exacts();
                    let _ = item_inner.map_or((), |v| items.push(v));
                    exacts.extend(exacts_inner);
                }
                item = Some(ClassSetItem::Union(ClassSetUnion { items }))
            }
            _ => item = Some(self.clone()),
        }
        (item, exacts)
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
struct ClassSetRange {
    start: LiteralKind,
    end: LiteralKind,
}
#[derive(Debug, Clone, PartialEq, Eq)]
struct ClassSetUnion {
    items: Vec<ClassSetItem>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
struct Repetition {
    op: Span,
    ast: Ast,
}
#[derive(Debug, Clone, PartialEq, Eq)]
struct Group {
    kind: GroupKind,
    ast: Ast,
}
#[derive(Debug, Clone, PartialEq, Eq)]
enum GroupKind {
    CaptureIndex,
    CaptureNamed(Span),
    NoCapturing(Flags),
}
#[derive(Debug, Clone, PartialEq, Eq)]
struct Alternation {
    asts: Vec<Ast>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
struct Concat {
    asts: Vec<Ast>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
struct Exact {
    /// The accumulated negated value by now.
    acc_negated: bool,
    kind: ExactKind,
}
#[derive(Debug, Clone, PartialEq, Eq)]
enum ExactKind {
    CS(Span),
    CatCode(CatCodeSet),
}
#[derive(Debug, Clone, PartialEq, Eq)]
struct CatCodeSet {
    catcodes: CCSet, // ordered
    negated: bool,
    /// must be `Ast::Raw`, otherwise will be a fatal error
    ast: Ast,
}
impl CatCodeSet {
    /// Generate a tagged `String`, which has
    /// `[\x90...]<raw ast>\xFF`, or `[\x80-\x8F]<raw ast>\xFF`,
    /// or `[\x80-\x8F]<raw ast>\xFF|[\x90...]<raw ast>\xFF`.
    fn to_tagged_pattern(&self, pattern: &str) -> String {
        let mut res = String::new();

        let mut cats =
            if self.negated { !self.catcodes } else { self.catcodes };

        let Ast::Raw(raw_ast_s) = &self.ast else {
            unreachable!("Ast Of CatCodeSet must be Ast::Raw")
        };
        let raw_ast = &pattern[raw_ast_s.start .. raw_ast_s.end];

        if cats.contains_catcode(CatCode::Escape) {
            res.push_str(&tagged_cs(
                ..,
                if raw_ast == "." { ".*" } else { raw_ast },
            ));
            cats.remove_catcode(CatCode::Escape);
        }

        let mut char_tags = String::new();
        for cats in cats.into_catcode_range_iter() {
            char_tags.push_str(&cats.to_range_str(TokenType::Char));
        }

        if !char_tags.is_empty() {
            if !res.is_empty() {
                res.push('|');
            }
            res.push_str(&tagged_char(char_tags, raw_ast));
            res.push('|');
            res.push_str(&tagged_any_all(..));
        }
        res
    }
}

#[derive(Debug, Default)]
struct BuilderStacks {
    asts: Vec<Ast>,
    alt: Vec<Ast>,
    concat: Vec<Ast>,
}
impl BuilderStacks {
    fn move_concat(&mut self) -> Vec<Ast> {
        std::mem::take(&mut self.concat)
    }
    fn move_alt(&mut self) -> Vec<Ast> {
        std::mem::take(&mut self.alt)
    }
    fn move_asts(&mut self) -> Vec<Ast> {
        std::mem::take(&mut self.asts)
    }
    fn normalize(mut self) -> Ast {
        if self.alt.is_empty() {
            let mut asts = self.move_asts();
            if !self.concat.is_empty() {
                asts.extend(self.move_concat());
                return Ast::Concat(Box::new(Concat { asts }));
            }
        }
        if !self.concat.is_empty() {
            let concat = Ast::Concat(Box::new(Concat { asts: self.concat }));
            self.alt.push(concat);
        }
        if !self.alt.is_empty() {
            self.asts.push(Ast::Alternation(Box::new(Alternation {
                asts: self.alt,
            })));
        }
        let ast = if self.asts.len() == 0 {
            Ast::Empty
        } else if self.asts.len() == 1 {
            self.asts.pop().unwrap()
        } else {
            Ast::Concat(Box::new(Concat { asts: self.asts }))
        };
        ast
    }
}

impl<'s> RegTExBuilder<'s> {
    pub fn new(pattern: &str) -> RegTExBuilder {
        RegTExBuilder {
            pattern,
            offset: Cell::new(0),
            in_set: Cell::new(0),
            set_negated: Cell::new(0),
            ending: Cell::new(None),
        }
    }

    fn copy_status(&self) -> Self {
        RegTExBuilder {
            pattern: self.pattern,
            offset: Cell::new(self.offset.get()),
            in_set: Cell::new(self.in_set.get()),
            set_negated: Cell::new(self.set_negated.get()),
            ending: Cell::new(self.ending.get()),
        }
    }
    fn from_status(&self, status: Self) {
        assert_eq!(self.pattern.as_ptr(), status.pattern.as_ptr());
        self.offset.set(status.offset.get());
        self.in_set.set(status.in_set.get());
        self.set_negated.set(status.set_negated.get());
        self.ending.set(status.ending.get());
    }
    fn from_status_hold_offset(&self, status: Self) {
        assert_eq!(self.pattern.as_ptr(), status.pattern.as_ptr());
        self.in_set.set(status.in_set.get());
        self.set_negated.set(status.set_negated.get());
        self.ending.set(status.ending.get());
    }
    fn offset(&self) -> usize {
        self.offset.get()
    }
    fn set_negated_add(&self, n: usize) {
        let set_negated = self.set_negated.get() | (1 << n);
        self.set_negated.set(set_negated);
    }
    fn set_negated_remove(&self, n: usize) {
        let set_negated = self.set_negated.get() & !(1 << n);
        self.set_negated.set(set_negated);
    }
    fn set_negated_get(&self, n: usize) -> bool {
        let set_negated = self.set_negated.get();
        (set_negated & (1 << n)) != 0
    }
    fn set_negated_acc(&self) -> bool {
        self.set_negated.get().count_ones() % 2 != 0
    }
    fn is_eof(&self) -> bool {
        self.offset() >= self.pattern.len()
    }
    /// Return the rest of pattern.
    ///
    /// Panic if at eof.
    fn rest(&self) -> &str {
        &self.pattern[self.offset() ..]
    }
    fn rest_n_bytes(&self, n: usize) -> &[u8] {
        let s = self.rest().as_bytes();
        &s[0 .. n.min(s.len())]
    }
    /// Return the character at the current position of the parser.
    ///
    /// This panics if the current position does not point to a valid char.
    fn char(&self) -> char {
        self.char_at(self.offset())
    }
    /// Return the character at the given position.
    ///
    /// This panics if the given position does not point to a valid char.
    fn char_at(&self, i: usize) -> char {
        self.pattern[i ..]
            .chars()
            .next()
            .unwrap_or_else(|| panic!("expected char at offset {}", i))
    }
    /// Bump the parser to the next Unicode scalar value.
    ///
    /// If the end of the input has been reached, then `false` is returned.
    fn _bump(&self) -> bool {
        if self.is_eof() {
            return false;
        }
        let offset = self.offset();
        self.offset.set(offset + self.char().len_utf8());
        self.pattern[self.offset() ..].chars().next().is_some()
    }
    /// Bump the parser to the next Unicode scalar value, and return the value.
    ///
    /// If the input has been exhausted, then this returns `None`.
    fn next(&self) -> Option<char> {
        if self.is_eof() {
            return None;
        }
        let offset = self.offset();
        self.offset.set(offset + self.char().len_utf8());
        self.pattern[self.offset() ..].chars().next()
    }
    /// Bump the parser stepped by `n`.
    ///
    /// Panic if the byte is not a valid char boundary after stepped by `n`.
    fn step(&self, n: usize) -> Option<char> {
        if self.is_eof() {
            return None;
        }
        let offset = self.offset();
        self.offset.set(offset + n);
        if self.is_eof() {
            return None;
        }
        self.pattern[self.offset() ..].chars().next()
    }
    /// Peek at the next character in the input without advancing the parser.
    ///
    /// If the input has been exhausted, then this returns `None`.
    fn peek(&self) -> Option<char> {
        if self.is_eof() {
            return None;
        }
        self.pattern[self.offset() + self.char().len_utf8() ..].chars().next()
    }
    /// Return the character before the current character.
    fn _prev(&self) -> Option<char> {
        if self.offset() == 0 {
            None
        } else {
            self.pattern[.. self.offset()].chars().last()
        }
    }
    /// Return the previous byte of the current character.
    fn _prev_byte(&self) -> Option<u8> {
        if self.offset() == 0 {
            None
        } else {
            self.pattern[.. self.offset()].as_bytes().last().copied()
        }
    }

    pub fn build(&self) -> Result<String, Error> {
        assert_eq!(self.offset(), 0, "Parser can only be used once.");
        let mut regex_pattern = String::from("(?s-u)"); // allow . match \n mode and ASCII compatible mode
        self.parse_re_normalized(None)?
            .build_pattern(&self.pattern, &mut regex_pattern)?;
        Ok(regex_pattern)
    }

    fn parse_re_normalized(
        &self,
        eof_err: Option<Error>,
    ) -> Result<Ast, Error> {
        let mut stacks = BuilderStacks::default();
        loop {
            if self.is_eof() {
                let ending = self.ending.get();
                if !(ending.is_some() && ending == self._prev())
                    && eof_err.is_some()
                {
                    return Err(eof_err.unwrap());
                }
                break;
            }
            let item = self.parse_item(&mut stacks)?;
            match item {
                Some(ast) => stacks.concat.push(ast),
                None => {
                    self.next();
                    break;
                }
            }
        }
        Ok(stacks.normalize())
    }

    // If return Ok(Some()), this method will `next()`, otherwise never `next()`.
    // If return Ok(None), the parser has met `ending`.
    fn parse_item(
        &self,
        asts: &mut BuilderStacks,
    ) -> Result<Option<Ast>, Error> {
        let ast = match self.char() {
            '\\' => Some(self.parse_escape(asts)?),
            '[' => Some(self.parse_set(asts)?),
            chr if self.in_set.get() == 0
                && Some(chr) == self.ending.get() =>
            {
                None
            }
            _ => Some(self.parse_char(asts)?),
        };
        if self.in_set.get() > 0 || ast.is_none() {
            Ok(ast)
        } else {
            Ok(Some(self.find_repetition(asts, ast.unwrap())?))
        }
    }

    fn parse_char(&self, asts: &mut BuilderStacks) -> Result<Ast, Error> {
        if self.in_set.get() > 0 {
            match self.char() {
                '[' => self.parse_set(asts),
                chr => self.parse_uni_literal(asts, chr),
            }
        } else {
            // in normal
            match self.char() {
                '(' => self.parse_group(asts),
                '|' => self.parse_alternation(asts),
                '.' => self.next_with(Ast::Dot),
                '^' => self
                    .next_with(Ast::Assertion(Box::new(Assertion::StartLine))),
                '$' => self
                    .next_with(Ast::Assertion(Box::new(Assertion::EndLine))),
                chr if !chr.is_ascii_alphanumeric()
                    && matches!(chr, '\x21' ..= '\x7e') =>
                {
                    Err(Error::NeedEscape(chr))
                }
                chr => self.parse_uni_literal(asts, chr),
            }
        }
    }

    fn next_with(&self, ast: Ast) -> Result<Ast, Error> {
        self.next();
        Ok(ast)
    }

    fn parse_escape(&self, asts: &mut BuilderStacks) -> Result<Ast, Error> {
        let escape_type = self.next().ok_or(Error::Invalid)?;
        match escape_type {
            'x' => self.parse_meta_x(asts),
            'c' => self.parse_meta_c(asts),
            'p' => self.parse_meta_p(asts, false), // unsupported in l3regex, but very useful
            'P' => self.parse_meta_p(asts, true), // unsupported in l3regex, but very useful
            'd' => self.parse_meta_d(asts, false),
            'D' => self.parse_meta_d(asts, true),
            's' => self.parse_meta_s(asts, false),
            'S' => self.parse_meta_s(asts, true),
            'w' => self.parse_meta_w(asts, false),
            'W' => self.parse_meta_w(asts, true),
            'b' => self.parse_meta_b(asts),
            'a' | 'f' | 'n' | 'r' | 't' => {
                self.parse_meta_char(asts, escape_type as u8)
            }
            'A' => {
                self.next_with(Ast::Assertion(Box::new(Assertion::StartText)))
            }
            'Z' => {
                self.next_with(Ast::Assertion(Box::new(Assertion::EndText)))
            } // \Z = \z
            'z' => {
                self.next_with(Ast::Assertion(Box::new(Assertion::EndText)))
            }
            'B' => self.next_with(Ast::Assertion(Box::new(
                Assertion::NotWordBoundary,
            ))),
            'e' => self.parse_uni_chars(asts, &['\x1b'], false),
            'h' => self.parse_uni_chars(asts, &['\x09', '\x20'], false),
            'H' => self.parse_uni_chars(asts, &['\x09', '\x20'], true),
            'v' => self.parse_uni_chars(
                asts,
                &['\x0A', '\x0B', '\x0C', '\x0D'],
                false,
            ),
            'V' => self.parse_uni_chars(
                asts,
                &['\x0A', '\x0B', '\x0C', '\x0D'],
                true,
            ),
            'G' | 'K' | 'u' | 'U' => {
                return Err(Error::UnsupprotedMeta(escape_type))
            }
            '\\' | ' ' | _ => {
                if escape_type.is_ascii_alphanumeric() {
                    return Err(Error::InvalidEscaped(escape_type));
                } else {
                    self.parse_uni_escaped(asts, escape_type)
                }
            }
        }
    }

    // do `next()`
    fn parse_x_hex(&self) -> Result<char, Error> {
        let p1 = self.next().ok_or(Error::InvalidMeta('x'))?;
        let hex = if p1 == '{' {
            let step = memchr::memchr(b'}', self.rest().as_bytes())
                .ok_or(Error::InvalidMeta('x'))?;
            let num =
                self.rest().get(1 .. step).ok_or(Error::InvalidMeta('x'))?;
            self.step(step + 1);
            u32::from_str_radix(num, 16)
                .map_err(|_| Error::InvalidMeta('x'))?
        } else if matches!(p1, '0'..='9' | 'a'..='f' | 'A'..='F' ) {
            let p2 = self.next().ok_or(Error::InvalidMeta('x'))?;
            self.next();
            if matches!(p2, '0'..='9' | 'a'..='f' | 'A'..='F') {
                u32::from_str_radix(&format_compact!("{}{}", p1, p2), 16)
                    .map_err(|_| Error::InvalidMeta('x'))?
            } else {
                return Err(Error::InvalidMeta('x'));
            }
        } else {
            return Err(Error::InvalidMeta('x'));
        };
        let chr = char::from_u32(hex).ok_or(Error::InvalidMeta('x'))?;
        Ok(chr)
    }

    fn parse_meta_x(&self, _: &mut BuilderStacks) -> Result<Ast, Error> {
        let chr = self.parse_x_hex()?;
        Ok(Ast::Literal(Box::new(LiteralKind::X(chr))))
    }

    // do `next()`
    fn parse_exact_do<R>(
        &self,
        f: impl FnOnce(Exact) -> Result<R, Error>,
    ) -> Result<R, Error> {
        let next_chr = self.next().ok_or(Error::InvalidMeta('c'))?;
        if next_chr == '{' {
            self.next().ok_or(Error::InvalidMeta('c'))?;
            let start = self.offset();
            let step =
                self.find_matching_end().or(Err(Error::InvalidMeta('c')))?;
            self.step(step);
            let kind = ExactKind::CS((start, self.offset()).into());
            self.next();
            f(Exact { acc_negated: self.set_negated_acc(), kind })
        } else if next_chr == '[' {
            let negated = match self.next().ok_or(Error::InvalidMeta('c'))? {
                '^' => {
                    self.next().ok_or(Error::InvalidMeta('c'))?;
                    true
                }
                _ => false,
            };
            let start = self.offset();
            let step = memchr::memchr(b']', self.rest().as_bytes())
                .ok_or(Error::InvalidMeta('c'))?;
            self.step(step);
            let mut catcodes = CCSet::new();
            for c in self.pattern[start .. self.offset()].chars() {
                if let Some(cat) = CatCode::from_char(c) {
                    catcodes.insert_catcode(cat);
                } else {
                    return Err(Error::InvalidMeta('c'));
                }
            }
            if catcodes.is_empty() || (negated && catcodes.is_full()) {
                return Err(Error::InvalidMeta('c'));
            }
            let cat_ast = self.next_raw_ast()?;
            let kind = ExactKind::CatCode(CatCodeSet {
                catcodes,
                negated,
                ast: cat_ast,
            });
            f(Exact { acc_negated: self.set_negated_acc(), kind })
        } else {
            let cat =
                CatCode::from_char(next_chr).ok_or(Error::InvalidMeta('c'))?;
            let cat_ast = self.next_raw_ast()?;
            let kind = ExactKind::CatCode(CatCodeSet {
                catcodes: CCSet::new_with_catcode(cat),
                negated: false,
                ast: cat_ast,
            });
            f(Exact { acc_negated: self.set_negated_acc(), kind })
        }
    }

    // return the length of raw item, do `next()`
    fn find_raw_item(&self, repetion: bool) -> Result<usize, Error> {
        let chr = self.char();
        let len = if chr == '\\' {
            let type_len = match self.next().ok_or(Error::Invalid)? {
                typ @ ('p' | 'P') => self.raw_group_or_one(typ),
                typ @ 'b' => self.raw_group_or_none(typ),
                'x' => self.raw_x(),
                meta @ ('G' | 'K' | 'u' | 'U') => {
                    return Err(Error::UnsupprotedMeta(meta))
                }
                chr => Ok(chr.len_utf8()),
            }?;
            self.next();
            1 + type_len
        } else {
            let type_len = match self.ending.get() {
                Some(ending) if chr == ending => Ok(0),
                _ => match chr {
                    '[' => self.raw_set(),
                    '(' => self.raw_group(),
                    _ => Ok(chr.len_utf8()),
                },
            }?;
            self.next();
            type_len
        };

        let group_len =
            if len > 0 && repetion && !self.is_eof() && self.char() == '{' {
                (self.find_right_brace('c')?, self.next()).0
            } else {
                0
            };

        Ok(len + group_len)
    }

    // do not `next()`
    fn find_right_brace(&self, typ: char) -> Result<usize, Error> {
        let step = memchr::memchr(b'}', self.rest().as_bytes())
            .ok_or(Error::InvalidMeta(typ))?;
        self.step(step);
        Ok(step + 1)
    }
    fn raw_group_or_one(&self, typ: char) -> Result<usize, Error> {
        let chr = self.peek().ok_or(Error::Invalid)?;
        if chr == '{' {
            self.find_right_brace(typ)
        } else {
            Ok(chr.len_utf8())
        }
    }
    fn raw_group_or_none(&self, typ: char) -> Result<usize, Error> {
        match self.peek() {
            Some('{') => self.find_right_brace(typ),
            _ => Ok(typ.len_utf8()),
        }
    }
    fn raw_x(&self) -> Result<usize, Error> {
        let start = self.offset();
        let chr = self.next().ok_or(Error::InvalidMeta('x'))?;
        if chr == '{' {
            Ok(self.offset() + 1 - start)
        } else {
            self.next().ok_or(Error::InvalidMeta('x'))?;
            Ok(self.offset() - start)
        }
    }
    fn raw_set(&self) -> Result<usize, Error> {
        let mut step = 1;
        self.next().ok_or(Error::UnclosedSet)?;
        loop {
            if self.is_eof() {
                return Err(Error::UnclosedSet);
            } else if self.char() == ']' {
                break;
            } else {
                step += self.find_raw_item(false)?;
            }
        }
        Ok(step + 1)
    }
    fn raw_group(&self) -> Result<usize, Error> {
        let mut step = 1;
        self.next().ok_or(Error::UnclosedGroup)?;
        loop {
            if self.is_eof() {
                return Err(Error::UnclosedGroup);
            } else if self.char() == ')' {
                break;
            } else {
                step += self.find_raw_item(true)?;
            }
        }
        Ok(step + 1)
    }
    fn find_matching_end(&self) -> Result<usize, Error> {
        let status = self.copy_status();
        let mut step = 0;
        self.ending.set(Some('}'));
        loop {
            let item_step = self.find_raw_item(true)?;
            if item_step == 0 {
                break;
            } else {
                step += item_step;
            }
        }
        self.from_status(status);
        Ok(step)
    }
    fn next_raw_ast(&self) -> Result<Ast, Error> {
        self.next().ok_or(Error::InvalidMeta('c'))?;
        let status = self.copy_status();

        let start = self.offset();
        self.ending.set(None);
        let step = self.find_raw_item(false)?;
        if step == 0 {
            return Err(Error::InvalidMeta('c'));
        }
        self.from_status_hold_offset(status);
        Ok(Ast::Raw(Span { start, end: self.offset() }))
    }

    fn parse_meta_c(&self, _: &mut BuilderStacks) -> Result<Ast, Error> {
        self.parse_exact_do(|exact| Ok(Ast::Exact(Box::new(exact))))
    }

    fn parse_p_do<R>(
        &self,
        negated: bool,
        f: impl FnOnce(ClassUnicode) -> Result<R, Error>,
    ) -> Result<R, Error> {
        let typ = if negated { 'P' } else { 'p' };
        let next_chr = self.next().ok_or(Error::InvalidMeta(typ))?;
        if next_chr == '{' {
            let start = self.offset();
            let step = memchr::memchr(b'}', self.rest().as_bytes())
                .ok_or(Error::InvalidMeta(typ))?;
            self.step(step + 1);
            let class =
                ClassUnicode { negated, span: (start, self.offset()).into() };
            f(class)
        } else {
            let start = self.offset();
            self.next();
            let class =
                ClassUnicode { negated, span: (start, self.offset()).into() };
            f(class)
        }
    }

    fn parse_meta_p(
        &self,
        _: &mut BuilderStacks,
        negated: bool,
    ) -> Result<Ast, Error> {
        self.parse_p_do(negated, |c| Ok(Ast::ClassUnicode(Box::new(c))))
    }

    fn parse_meta_d(
        &self,
        _: &mut BuilderStacks,
        negated: bool,
    ) -> Result<Ast, Error> {
        self.next_with(Ast::ClassPerl(Box::new(ClassPerl {
            negated,
            kind: ClassPerlKind::Digit,
        })))
    }

    fn parse_meta_s(
        &self,
        _: &mut BuilderStacks,
        negated: bool,
    ) -> Result<Ast, Error> {
        self.next_with(Ast::ClassPerl(Box::new(ClassPerl {
            negated,
            kind: ClassPerlKind::Space,
        })))
    }

    fn parse_meta_w(
        &self,
        _: &mut BuilderStacks,
        negated: bool,
    ) -> Result<Ast, Error> {
        self.next_with(Ast::ClassPerl(Box::new(ClassPerl {
            negated,
            kind: ClassPerlKind::Word,
        })))
    }

    fn parse_meta_b(&self, _: &mut BuilderStacks) -> Result<Ast, Error> {
        let assert = match self.next() {
            None => Ast::Assertion(Box::new(Assertion::WordBoundary)),
            Some('{') => {
                let start = self.offset();
                let step = memchr::memchr(b'}', self.rest_n_bytes(16)).ok_or(
                    Error::InvalidWordBoundary(Some(String::from("{"))),
                )? + 1;
                self.step(step);
                let tag = &self.pattern[start .. self.offset()];
                if tag == "{start}" {
                    Ast::Assertion(Box::new(Assertion::WordBoundaryStart))
                } else if tag == "{end}" {
                    Ast::Assertion(Box::new(Assertion::WordBoundaryEnd))
                } else if tag == "{start-half}" {
                    Ast::Assertion(Box::new(Assertion::WordBoundaryStartHalf))
                } else if tag == "{end-half}" {
                    Ast::Assertion(Box::new(Assertion::WordBoundaryEndHalf))
                } else {
                    return Err(Error::InvalidWordBoundary(Some(
                        String::from(tag),
                    )));
                }
            }
            Some('<') => {
                return Err(Error::InvalidWordBoundary(Some(String::from(
                    "<",
                ))))
            }
            Some('>') => {
                return Err(Error::InvalidWordBoundary(Some(String::from(
                    ">",
                ))))
            }
            _ => Ast::Assertion(Box::new(Assertion::WordBoundary)),
        };
        Ok(assert)
    }

    fn parse_meta_char(
        &self,
        _: &mut BuilderStacks,
        c: u8,
    ) -> Result<Ast, Error> {
        self.next_with(Ast::Literal(Box::new(LiteralKind::Meta(c))))
    }

    fn parse_uni_chars(
        &self,
        _: &mut BuilderStacks,
        chars: &[char],
        negated: bool,
    ) -> Result<Ast, Error> {
        let ci = chars
            .into_iter()
            .map(|&c| ClassSetItem::Literal(LiteralKind::Verbatim(c)))
            .collect::<Vec<_>>();
        let cu = ClassSetUnion { items: ci };
        let cb = ClassBracketed {
            negated,
            acc_negated: self.set_negated_acc(),
            kind: ClassSetItem::Union(cu),
        };
        self.next_with(Ast::ClassBracketed(Box::new(cb)))
    }

    fn parse_uni_literal(
        &self,
        _: &mut BuilderStacks,
        chr: char,
    ) -> Result<Ast, Error> {
        self.next_with(Ast::Literal(Box::new(LiteralKind::Verbatim(chr))))
    }

    fn parse_uni_escaped(
        &self,
        _: &mut BuilderStacks,
        chr: char,
    ) -> Result<Ast, Error> {
        self.next_with(Ast::Literal(Box::new(LiteralKind::Superfluous(chr))))
    }

    fn parse_group(&self, _: &mut BuilderStacks) -> Result<Ast, Error> {
        let next_chr = self.next().ok_or(Error::InvalidGroup)?; // `a(` is not allowed
        if next_chr == '?' {
            let nn_chr = self.peek().ok_or(Error::InvalidGroup)?;
            if nn_chr == 'P' {
                // (?P ..)
                return Err(Error::CaptureNamedP);
            } else if nn_chr == '<' {
                // (?<.>..)
                self.next();
                let start = self.offset();
                let step = memchr::memchr(b'>', self.rest().as_bytes())
                    .ok_or(Error::InvalidGroup)?;
                self.step(step + 1);
                let name_span = (start, self.offset()).into();
                let group_ast = self.parse_group_re()?;
                let group = Group {
                    kind: GroupKind::CaptureNamed(name_span),
                    ast: group_ast,
                };
                Ok(Ast::Group(Box::new(group)))
            } else if nn_chr == ':' {
                // (?: ..)
                self.step(2);
                let group_ast = self.parse_group_re()?;
                let group = Group {
                    kind: GroupKind::NoCapturing(Flags { flags: Vec::new() }),
                    ast: group_ast,
                };
                Ok(Ast::Group(Box::new(group)))
            } else if nn_chr == ')' {
                // we got (?)
                return Err(Error::InvalidGroup);
            } else {
                let mut flags = vec![];
                while let Some(flag) = self.next() {
                    match flag {
                        '-' => flags.push(FlagKind::Negative),
                        'i' => flags.push(FlagKind::CaseInsensitive),
                        ':' => {
                            self.next().ok_or(Error::InvalidGroup)?;
                            let group_ast = self.parse_group_re()?;
                            let group = Group {
                                kind: GroupKind::NoCapturing(Flags { flags }),
                                ast: group_ast,
                            };
                            return Ok(Ast::Group(Box::new(group)));
                        }
                        ')' => {
                            let flags = Flags { flags };
                            return self
                                .next_with(Ast::Flags(Box::new(flags)));
                        }
                        _ => return Err(Error::InvalidFlag(flag)),
                    }
                }
                return Err(Error::Invalid);
            }
        } else {
            let group_ast = self.parse_group_re()?;
            let group =
                Group { kind: GroupKind::CaptureIndex, ast: group_ast };
            Ok(Ast::Group(Box::new(group)))
        }
    }

    // do `next()`
    fn parse_group_re(&self) -> Result<Ast, Error> {
        let status = self.copy_status();
        self.ending.set(Some(')'));
        let group_asts =
            self.parse_re_normalized(Some(Error::InvalidGroup))?;
        self.from_status_hold_offset(status);
        Ok(group_asts)
    }

    fn parse_set(&self, _: &mut BuilderStacks) -> Result<Ast, Error> {
        let bracketed = self.parse_set_items()?;
        Ok(Ast::ClassBracketed(Box::new(bracketed)))
    }

    // do `next()`
    fn parse_set_items(&self) -> Result<ClassBracketed, Error> {
        self.in_set.set(self.in_set.get() + 1);
        let in_set_level = if self.in_set.get() > 31 {
            return Err(Error::TooDeepSet);
        } else {
            self.in_set.get()
        };

        let next_chr = self.next().ok_or(Error::UnclosedSet)?;
        if next_chr == '^' {
            self.set_negated_add(in_set_level);
            self.next();
            self.parse_set_continue()
        } else if next_chr == ':' {
            let start = self.offset();
            self.next().ok_or(Error::UnclosedSet)?;
            let rest_n = self.rest_n_bytes(10);
            match memchr::memchr2(b':', b']', rest_n) {
                Some(step)
                    if rest_n[step] == b':'
                        && rest_n.get(step + 1) == Some(&b']') =>
                {
                    self.step(step + 1);
                    let class_ascii =
                        ClassSetItem::Ascii((start, self.offset()).into());
                    self.next();
                    Ok(self.pop_set_with(class_ascii))
                }
                Some(_) | None => {
                    self.offset.set(self.offset() - 1);
                    self.parse_set_continue()
                }
            }
        } else {
            self.parse_set_continue()
        }
    }

    fn parse_set_continue(&self) -> Result<ClassBracketed, Error> {
        let mut items = vec![];
        if !self.is_eof() && self.char() == ']' {
            items.push(ClassSetItem::Literal(LiteralKind::Verbatim(']')));
            self.next();
        }
        loop {
            if self.is_eof() {
                return Err(Error::UnclosedSet);
            }
            match self.find_set_item() {
                Ok(item_opt) => {
                    if let Some(item) = item_opt {
                        items.push(item);
                    } else {
                        self.next();
                        break;
                    }
                }
                Err(e) => return Err(e),
            }
        }
        let kind = if items.is_empty() {
            ClassSetItem::Empty
        } else if items.len() == 1 {
            items.pop().unwrap()
        } else {
            ClassSetItem::Union(ClassSetUnion { items })
        };
        Ok(self.pop_set_with(kind))
    }

    // do `next()`
    fn find_set_item(&self) -> Result<Option<ClassSetItem>, Error> {
        match self.char() {
            '[' => self.find_set_nested().map(|v| Some(v)),
            '\\' => match self.next().ok_or(Error::Invalid)? {
                'x' => self.set_meta_x(),
                'c' => self.set_meta_c(),
                'p' => self.set_meta_p(false), // unsupported in l3regex, but very useful
                'P' => self.set_meta_p(true), // unsupported in l3regex, but very useful
                'd' => self.set_meta_d(false),
                'D' => self.set_meta_d(true),
                's' => self.set_meta_s(false),
                'S' => self.set_meta_s(true),
                'w' => self.set_meta_w(false),
                'W' => self.set_meta_w(true),
                escape_type @ ('a' | 'f' | 'n' | 'r' | 't') => self
                    .next_find_set_range_with(LiteralKind::Meta(
                        escape_type as u8,
                    )),
                'e' => self.next_find_set_range_with(LiteralKind::X('\x1b')),
                'h' => self.set_uni_chars(&['\x09', '\x20'], false),
                'H' => self.set_uni_chars(&['\x09', '\x20'], true),
                'v' => self
                    .set_uni_chars(&['\x0A', '\x0B', '\x0C', '\x0D'], false),
                'V' => {
                    self.set_uni_chars(&['\x0A', '\x0B', '\x0C', '\x0D'], true)
                }
                escape_type @ ('K' | 'u' | 'U') => {
                    return Err(Error::UnsupprotedMeta(escape_type))
                }
                escape_type @ ('G' | '\\' | ' ' | _) => {
                    if escape_type.is_ascii_alphanumeric() {
                        return Err(Error::InvalidSetEscaped(escape_type));
                    } else {
                        self.next_find_set_range_with(
                            LiteralKind::Superfluous(escape_type),
                        )
                    }
                }
            },
            ']' => Ok(None),
            chr => self.next_find_set_range_with(LiteralKind::Verbatim(chr)),
        }
    }

    fn find_set_nested(&self) -> Result<ClassSetItem, Error> {
        let item = self.parse_set_items()?;
        Ok(ClassSetItem::Bracketed(Box::new(item)))
    }
    fn set_meta_x(&self) -> Result<Option<ClassSetItem>, Error> {
        let chr = self.parse_x_hex()?;
        self.offset.set(self.offset() - 1);
        self.next_find_set_range_with(LiteralKind::X(chr))
    }
    fn set_meta_c(&self) -> Result<Option<ClassSetItem>, Error> {
        self.parse_exact_do(|exact| Ok(Some(ClassSetItem::Exact(exact))))
    }
    fn set_meta_p(
        &self,
        negated: bool,
    ) -> Result<Option<ClassSetItem>, Error> {
        self.parse_p_do(negated, |c| Ok(Some(ClassSetItem::Unicode(c))))
    }
    fn set_meta_d(
        &self,
        negated: bool,
    ) -> Result<Option<ClassSetItem>, Error> {
        self.next();
        Ok(Some(ClassSetItem::Perl(ClassPerl {
            negated,
            kind: ClassPerlKind::Digit,
        })))
    }
    fn set_meta_w(
        &self,
        negated: bool,
    ) -> Result<Option<ClassSetItem>, Error> {
        self.next();
        Ok(Some(ClassSetItem::Perl(ClassPerl {
            negated,
            kind: ClassPerlKind::Word,
        })))
    }
    fn set_meta_s(
        &self,
        negated: bool,
    ) -> Result<Option<ClassSetItem>, Error> {
        self.next();
        Ok(Some(ClassSetItem::Perl(ClassPerl {
            negated,
            kind: ClassPerlKind::Space,
        })))
    }
    fn set_uni_chars(
        &self,
        chars: &[char],
        negated: bool,
    ) -> Result<Option<ClassSetItem>, Error> {
        self.next();
        let items = chars
            .into_iter()
            .map(|&c| ClassSetItem::Literal(LiteralKind::X(c)))
            .collect::<Vec<_>>();
        let un = ClassSetUnion { items };
        let bracketed = ClassBracketed {
            negated,
            acc_negated: self.set_negated_acc(),
            kind: ClassSetItem::Union(un),
        };
        Ok(Some(ClassSetItem::Bracketed(Box::new(bracketed))))
    }
    fn next_find_set_range_with(
        &self,
        literal: LiteralKind,
    ) -> Result<Option<ClassSetItem>, Error> {
        match self.next_find_set_range()? {
            Some(lit_r) => Ok(Some(ClassSetItem::Range(ClassSetRange {
                start: literal,
                end: lit_r,
            }))),
            None => Ok(Some(ClassSetItem::Literal(literal))),
        }
    }
    // do `next()`
    fn next_find_set_range(&self) -> Result<Option<LiteralKind>, Error> {
        match self.next() {
            Some('-') => {
                let lit = match self.next().ok_or(Error::UnclosedSet)? {
                    '\\' => match self.next().ok_or(Error::Invalid)? {
                        'x' => {
                            let chr = self.parse_x_hex()?;
                            Ok(LiteralKind::X(chr))
                        }
                        escape_type @ ('a' | 'f' | 'n' | 'r' | 't') => {
                            self.next();
                            Ok(LiteralKind::Meta(escape_type as u8))
                        }
                        escape_type @ ('u' | 'U') => {
                            return Err(Error::UnsupprotedMeta(escape_type))
                        }
                        escape_type => {
                            if escape_type.is_ascii_alphanumeric() {
                                return Err(Error::InvalidSetEscaped(
                                    escape_type,
                                ));
                            } else {
                                self.next();
                                Ok(LiteralKind::Superfluous(escape_type))
                            }
                        }
                    },
                    ']' => Err(Error::UnclosedRange),
                    chr => {
                        self.next();
                        Ok(LiteralKind::Verbatim(chr))
                    }
                }?;
                Ok(Some(lit))
            }
            _ => Ok(None),
        }
    }

    // do not `next()`
    fn pop_set_with(&self, kind: ClassSetItem) -> ClassBracketed {
        let in_set = self.in_set.get();
        assert!(in_set > 0);
        self.in_set.set(in_set - 1);
        let negated = self.set_negated_get(in_set);
        self.set_negated_remove(in_set);
        ClassBracketed { negated, acc_negated: self.set_negated_acc(), kind }
    }

    fn parse_alternation(
        &self,
        asts: &mut BuilderStacks,
    ) -> Result<Ast, Error> {
        if asts.alt.is_empty() {
            let concat_ast = if asts.concat.is_empty() {
                Ast::Empty // `a|b||c`, `|b|c` and `a|b|` are all allowed!
            } else {
                let concat_ast = asts.move_concat();
                Ast::Concat(Box::new(Concat { asts: concat_ast }))
            };
            asts.alt.push(concat_ast);
        }
        self.next();
        while !self.is_eof() {
            match self.parse_item(asts)? {
                Some(ast) => asts.alt.push(ast),
                None => {
                    // we got `ending`
                    break;
                }
            }
        }
        let alt = asts.move_alt();
        Ok(Ast::Alternation(Box::new(Alternation { asts: alt })))
    }

    // do `next()`
    fn find_repetition(
        &self,
        _: &mut BuilderStacks,
        ast: Ast,
    ) -> Result<Ast, Error> {
        if self.in_set.get() > 0 {
            unreachable!("Cannot use repetition in set")
        } else {
            let start = self.offset();
            while !self.is_eof() {
                let c = self.char();
                if matches!(c, '*' | '?' | '+') {
                    self.next();
                } else if c == '{' {
                    let step = memchr::memchr(b'}', self.rest().as_ref())
                        .ok_or(Error::InvalidRepetition)?;
                    self.step(step + 1);
                } else {
                    break;
                }
            }
            if self.offset() == start {
                Ok(ast)
            } else {
                let rep =
                    Repetition { op: (start, self.offset()).into(), ast };
                Ok(Ast::Repetition(Box::new(rep)))
            }
        }
    }
}

impl Ast {
    fn build_pattern(
        self,
        raw: &str,
        pattern: &mut String,
    ) -> Result<(), Error> {
        pattern.push_str(&self.to_pattern(raw));
        Ok(())
    }
}

trait ToRange: Clone {
    fn to_range_str(self, token_type: TokenType) -> CompactString;
}
impl ToRange for &str {
    fn to_range_str(self, _: TokenType) -> CompactString {
        CompactString::new(self)
    }
}
impl ToRange for String {
    fn to_range_str(self, _: TokenType) -> CompactString {
        CompactString::from(self)
    }
}
impl ToRange for CatCode {
    fn to_range_str(self, token_type: TokenType) -> CompactString {
        (self as u8).to_range_str(token_type)
    }
}
impl ToRange for u8 {
    fn to_range_str(self, token_type: TokenType) -> CompactString {
        let b = token_tag(token_type, self);
        let s = unsafe { std::str::from_utf8_unchecked(&b) };
        CompactString::new(s)
    }
}
impl ToRange for std::ops::RangeInclusive<CatCode> {
    fn to_range_str(self, token_type: TokenType) -> CompactString {
        let b_start = token_tag(token_type, *self.start() as u8);
        let s_start = unsafe { std::str::from_utf8_unchecked(&b_start) };
        if self.start() < self.end() {
            let b_end = token_tag(token_type, *self.end() as u8);
            let s_end = unsafe { std::str::from_utf8_unchecked(&b_end) };
            format_compact!("{}-{}", s_start, s_end)
        } else if self.start() == self.end() {
            CompactString::new(s_start)
        } else {
            CompactString::new("")
        }
    }
}
impl ToRange for std::ops::RangeInclusive<u8> {
    fn to_range_str(self, token_type: TokenType) -> CompactString {
        let b_start = token_tag(token_type, *self.start());
        let s_start = unsafe { std::str::from_utf8_unchecked(&b_start) };
        if self.start() < self.end() {
            let b_end = token_tag(token_type, *self.end());
            let s_end = unsafe { std::str::from_utf8_unchecked(&b_end) };
            format_compact!("{}-{}", s_start, s_end)
        } else if self.start() == self.end() {
            CompactString::new(s_start)
        } else {
            CompactString::new("")
        }
    }
}
impl ToRange for std::ops::RangeFull {
    fn to_range_str(self, token_type: TokenType) -> CompactString {
        (0u8 ..= 15u8).to_range_str(token_type)
    }
}
impl<T: ToRange> ToRange for &[T] {
    fn to_range_str(self, token_type: TokenType) -> CompactString {
        self.into_iter().map(|v| v.clone().to_range_str(token_type)).collect()
    }
}
const fn token_tag(token_type: TokenType, token_class: u8) -> [u8; 4] {
    let t1 = match token_type {
        TokenType::CS => 56,
        TokenType::Char => 57,
        TokenType::Any => 65,
    };
    let t2 = match token_class {
        0 ..= 9 => 48 + token_class,
        10 ..= 15 => 65 - 10 + token_class,
        _ => unreachable!(),
    };
    [92, 120, t1, t2] // \ x . .
}

fn _tagged_many_all<'a, R: ToRange + 'a, T>(rngs: T) -> String
where
    T: IntoIterator<IntoIter = std::slice::Iter<'a, (R, TokenType)>>,
{
    format!(
        "[{}][^\\xFF]*\\xFF",
        rngs.into_iter()
            .map(|(rng, token_type)| rng.clone().to_range_str(*token_type))
            .collect::<String>()
    )
}
fn _tagged_cs_all<T: ToRange>(rng: T) -> CompactString {
    format_compact!("[{}][^\\xFF]*\\xFF", rng.to_range_str(TokenType::CS))
}
fn _tagged_char_all<T: ToRange>(rng: T) -> CompactString {
    format_compact!("[{}][^\\xFF]*\\xFF", rng.to_range_str(TokenType::Char))
}
fn tagged_any_all<T: ToRange>(rng: T) -> CompactString {
    format_compact!("[{}][^\\xFF]*\\xFF", rng.to_range_str(TokenType::Any))
}
fn tagged_cs<T: ToRange>(rng: T, s: &str) -> String {
    format!("[{}](?u:{})\\xFF", rng.to_range_str(TokenType::CS), s)
}
fn tagged_char<T: ToRange>(rng: T, s: &str) -> String {
    format!("[{}](?u:{})\\xFF", rng.to_range_str(TokenType::Char), s)
}
fn _tagged_any<T: ToRange>(rng: T, s: u32) -> String {
    format!("[{}]{}\\xFF", rng.to_range_str(TokenType::Any), s)
}

#[cfg(test)]
mod tests {
    use crate::{
        config::LexerType,
        tokenlist::SourcedTokenList,
        types::{CTab, CTabSet, CatCodeStack, TokenList},
    };

    use super::*;

    #[test]
    fn equality() {
        let re = regex::Regex::new(
            r"^(\\([\p{Han}\pL]+|\pP|\p{Cc})\s*)+ad.relax?a+好?$",
        )
        .unwrap();
        assert!(re.is_match(r"\controlsequence控制序列\.\。\\addrelaxaaaaa"));

        let re = RegTEx::new(
            r"^(\c{([\p{Han}\pL]+|\pP|\p{Cc})}\s*)+ad.relax?a+好?$",
        )
        .unwrap();
        let mut ctab = CTab::document();
        ctab.extend(CTab::cjk_ideographs(CatCode::Letter));
        let tl = TokenList::parse(
            r"\controlsequence控制序列\.\。\\addrelaxaaaaa",
            &ctab,
        );
        let tb = tl.to_bytes();
        assert!(re.is_match(&tb));
    }

    #[test]
    fn builder() {
        let test_pattern = |s: &str, err: Option<Error>| {
            let b = RegTExBuilder::new(s);
            if err.is_some() {
                assert_eq!(b.parse_re_normalized(None), Err(err.unwrap()));
            } else {
                let _ = b
                    .parse_re_normalized(None)
                    .expect(&format!("Cannot Build Pattern: {:?}", s));
                // println!("Ast: {:?}", ast);
                // println!("Raw Pattern: {:?}", ast.to_raw(s));
            }
        };

        test_pattern("^ad.relax?a+好?$", None);
        test_pattern(r"\\\a\e\f\t\h\v\V\h\H\d\D\s\S\w\W\b\B\A\Z\z", None);
        test_pattern(
            r"a+ b* c? d+? e*? f?? g{4,} h{4} i{4,10} j.{4}{4} k{4}",
            None,
        );
        test_pattern(r"e{4,", Some(Error::InvalidRepetition));
        test_pattern(r"\b{start}\b{start-half}\b{end}\b{end-half}", None);
        test_pattern(
            r"\pL\p{Lo}\p{Han}\p{Greek}\PL\P{Lo}\P{Han}\P{Greek}",
            None,
        );
        test_pattern(r"\x58\x{2ef4}", None);
        test_pattern(r"\x{pf4}", Some(Error::InvalidMeta('x')));
        test_pattern(r"\u{32}", Some(Error::UnsupprotedMeta('u')));
        test_pattern(r"\U{32}", Some(Error::UnsupprotedMeta('U')));
        test_pattern(r"\K", Some(Error::UnsupprotedMeta('K')));
        test_pattern(r"\G", Some(Error::UnsupprotedMeta('G')));
        test_pattern(r"[abcd|f.][^abc]", None);
        test_pattern(
            r"[\\\a\e\f\t\h\v\V\h\H\d\D\s\S\w\W\pL\PL\p{Lo}\P{Lo}\x{20}]",
            None,
        );
        test_pattern(r"[\a-\x7ea-b\x{4e00}-\x{9fa5}]", None);
        test_pattern(
            r"[:alnum:][:alpha:][:ascii:][:blank:][:cntrl:][:digit:][:graph:][:lower:][:print:][:punct:][:space:][:upper:][:word:][:xdigit:]?",
            None,
        );
        test_pattern(
            r"[[:alnum:][:alpha:][:ascii:][:blank:][:cntrl:][:digit:][:graph:][:lower:][:print:][:punct:][:space:][:upper:][:word:][:xdigit:]]?",
            None,
        );
        test_pattern(r"(relax)(some long+ text[s]?)*", None);
        test_pattern(r"(?<name>Namegroup) r(?<name2>(?-i)Namegroup)", None);
        test_pattern(r"(?:)", None);
        test_pattern(r"(?)", Some(Error::InvalidGroup));
        test_pattern(r"(?-i:None[\p{Lu}\p{Han}])", None);
        test_pattern(r"(?-i)* (?i).+", None);
        test_pattern(r"(?P<a>err)", Some(Error::CaptureNamedP));
        test_pattern(r"a|b|c|", None);
        test_pattern(r"(a|\c{.}|c|)", None);
        test_pattern(r"[abc]*|(jit)?|\\{4}|\?", None);
        test_pattern(r"(?:^|\cJ.|\n)\c{makeatletter}$", None);
        test_pattern(
            r"\cX.?\c[XO].\c[^XC].\c{[^[:alpha:]]}*?\c{[[:alpha:]@#$%^&*.?\\]}+",
            None,
        );
        test_pattern(r"\c{a{2}} \c{(?i:[ab]|c|d|\\{2})}{5}", None);
        test_pattern(r"\c[].", Some(Error::InvalidMeta('c')));
        test_pattern(r"\c", Some(Error::InvalidMeta('c')));
        test_pattern(r"[a-b\cX.\c[^X].\c{[^[:alpha:]]}]", None);
        test_pattern(r"[a-c[b-j\c[XO].][^lm]]{2,}", None);
        test_pattern(r"[^a-c[^b-j\c[XO].\c{(no)?relax}][lm]]?", None);
    }

    #[test]
    fn pattern() {
        let ref lexer = LexerType::default();
        let ref ctabset = CTabSet::new_empty();
        let test_pattern = |s: &str, test: &[(&str, bool)]| {
            let b = RegTExBuilder::new(s).parse_re_normalized(None).unwrap();
            let re = RegTEx::new(s).unwrap();
            if test.is_empty() {
                println!("Ast Pattern: {}", b.to_pattern(s));
            }
            for &(ts, res) in test {
                let ref mut catcode = CatCodeStack::new_with(CTab::document());
                let tl = SourcedTokenList::parse(
                    ts.to_string().into(),
                    catcode,
                    (lexer, ctabset),
                );
                let m = unsafe { re.is_match_bytes(tl.bytes()) };
                assert_eq!(
                    m,
                    res,
                    "Mismatched pattern,\n\tHaystack: {:?}\n\tTokenList: {:?}\n\tAst: {:?}\n\tPattern: {:?}",
                    ts,
                    tl.to_str_repr(),
                    &b,
                    b.to_pattern(s)
                );
            }
        };

        test_pattern(
            "^ad.relax?a+好?$",
            &[("adxrelaaaa好", true), ("abrel", false)],
        );
        // test_pattern(r"\\\a\e\f\t\h\v\V\h\H\d\D\s\S\w\W\b\B\A\Z\z", &[]);
        test_pattern(
            r"a+ b* c? d+? e*? f?? g{4,} h{4} i{4,10} j.{4} k{2}{2}",
            &[("aaa  c dd ee  ggggg hhhh iiiii jkkkk kkkk", true)],
        );
        test_pattern(r"\b.*?\b", &[("a", true)]);
        test_pattern(
            r"\pL\p{L}\p{Han}\p{Greek}\PL\P{Lo}\P{Han}\P{Greek}",
            &[("TΠ好Π。LLL", true)],
        );
        test_pattern(
            r"\x58\x{2ef4}",
            &[("\x58\u{2ef4}", true), ("ab", false)],
        );
        test_pattern(
            r"[abcd|f.][^abc]",
            &[
                ("ak", true),
                (".l", true),
                ("|m", true),
                ("aa", false),
                ("pl", false),
            ],
        );
        // test_pattern(
        //     r"[\\\a\e\f\t\h\v\V\h\H\d\D\s\S\w\W\pL\PL\p{Lo}\P{Lo}\x{20}]",
        //     &[],
        // );
        test_pattern(
            r"[\a-\x7ea-b\x{4e00}-\x{9fa5}]",
            &[
                ("p", true),
                ("好", true),
                ("\x01", false),
                ("\u{1F000}", false),
            ],
        );
        test_pattern(
            r"[:alnum:][:alpha:][:ascii:][:blank:][:cntrl:][:digit:][:graph:][:lower:][:print:][:punct:][:space:][:upper:][:word:][:xdigit:]?",
            &[("1aa \t0~l ! L_f", true)],
        );
        test_pattern(
            r"[[:alnum:][:alpha:][:ascii:][:blank:][:cntrl:][:digit:][:graph:][:lower:][:print:][:punct:][:space:][:upper:][:word:][:xdigit:]]?",
            &[("1aa \t0~l ! L_f", true)],
        );
        test_pattern(
            r"(relax)(some long+ text[s]?)*",
            &[("relaxsome longgg texts", true), ("relax", true)],
        );
        test_pattern(
            r"(?<name>Namegroup) r(?<name2>(?i)Namegroup)",
            &[("Namegroup rnameGROUP", true)],
        );
        test_pattern(r"(?:)", &[("a", true), ("", true)]);
        test_pattern(
            r"(?-i:None[\p{Lu}\p{Han}])",
            &[("None好", true), ("NoneΠ", true), ("none好", false)],
        );
        test_pattern(r"(?-i)* (?i).+", &[("? ?l", true)]);
        test_pattern(r"a|b|c|", &[("a", true), ("b", true), ("c", true)]);
        test_pattern(
            r"(a|\c{.}|c|)",
            &[("a", true), (r"\?", true), ("c", true)],
        );
        test_pattern(
            r"[abc]*|(jit)?|\\{4}|\?",
            &[("abc", true), ("jit", true), (r"\\\\", true), ("?", true)],
        );
        test_pattern(
            r"(?:^|\cJ.|\n)\c{makeatletter}$",
            &[
                ("\n\\makeatletter", true),
                (r"\makeatletter", true),
                (r"a\makeatletter", false),
            ],
        );
        test_pattern(
            r"\cX.?\c[XO].\c[^XC].\c{[^[:alpha:]]}*?\c{[[:alpha:]@#$%^&*.?\\]}+$",
            &[(r"\a\bc#\.\好\\", true), (r"\a\bc#\.\好.", false)],
        );
        test_pattern(
            r"\c{a{2}} *\c{(?i:[ab]|c|d|\\{2})}{5}",
            &[(r"\aa\a\b\c\d\A", true), ((r"\aa\a\b\c\d", false))],
        );
        test_pattern(
            r"[a-b\cX.\c[^X].\c{[^[:alpha:]]}]",
            &[(r"a\relax\.", true)],
        );
        test_pattern(
            r"[a-c[b-j\c[XO].][^jm]]{2,}",
            &[(r"**", true), (r"\a#", true), (r"mj", false)],
        );
        test_pattern(
            r"[^a-c[^b-j\c[XO].\c{(no)?relax}][lm]]?",
            &[(r"d\a\norelax", true), (r"l\a\norelax", true)],
        );
    }
}
