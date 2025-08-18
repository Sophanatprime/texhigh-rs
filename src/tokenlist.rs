/* tokenlist.rs
Copyright (C) 2024-2025, Wenjian Chern.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>. */

use std::cell::{Cell, RefCell};
use std::slice::Iter;
use std::sync::Arc;

use bitflags::bitflags;
use compact_str::{format_compact, CompactString, ToCompactString};
use log::warn;

use crate::config::{
    Category, CategorySpan, HighConfig, LexerType, RangeComments,
};
use crate::high::{HWrite, HighFormat};
use crate::tex::args_parser::Argument;
use crate::tex::circumflex_mechanism;
use crate::tex::{
    escape_string, escape_string_filter, escape_string_small, get_cs_type_re,
    primitive_engine, CatCode, LaTeXType,
};
use crate::types::{
    CTabSet, CatCodeGetter, CatCodeStack, Character, ControlSequence,
    ErrorKind, Position, Token, TokenBytes, TokenListBytes, TokenListBytesRef,
};

#[derive(Debug)]
pub struct SourcedTokenList {
    pub(crate) tl: Vec<Token>,
    pub(crate) bytes: Vec<u8>,
    pub(crate) source: Arc<String>,
    pub(crate) bytes_indices: Vec<usize>,
    pub(crate) source_indices: Vec<usize>,
}

impl SourcedTokenList {
    pub fn parse<'c, 'i: 'c, 'l: 'c + 'i>(
        source: Arc<String>,
        catcode: &'c mut CatCodeStack<'i>,
        (lexer, ctabset): (&'l LexerType, &'l CTabSet),
    ) -> Self {
        let mut tl = Vec::new();
        let mut bytes = Vec::new();
        let mut bytes_indices = vec![0];
        let mut source_indices = vec![0];
        parse_tokenlist(
            &mut tl,
            &mut bytes,
            &mut bytes_indices,
            &mut source_indices,
            &source,
            catcode,
            (lexer, ctabset),
        );
        SourcedTokenList { tl, bytes, source, bytes_indices, source_indices }
    }
    pub fn push_tokens<'c, 'l: 'c, C: CatCodeGetter>(
        &mut self,
        source: &str,
        catcode: &'c mut CatCodeStack<'c>,
        (lexer, ctabset): (&'l LexerType, &'l CTabSet),
    ) {
        parse_tokenlist(
            &mut self.tl,
            &mut self.bytes,
            &mut self.bytes_indices,
            &mut self.source_indices,
            source,
            catcode,
            (lexer, ctabset),
        );
        let mut s = self.source.as_str().to_string();
        s.push_str(source);
        self.source = Arc::new(s);
    }
    pub fn len(&self) -> usize {
        self.tl.len()
    }
    pub fn is_empty(&self) -> bool {
        self.tl.is_empty()
    }
    pub fn tokenlist(&self) -> &[Token] {
        &self.tl
    }
    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }
    pub fn sources(&self) -> &str {
        &self.source
    }
    pub fn bytes_indices(&self) -> &[usize] {
        &self.bytes_indices
    }
    pub fn source_indices(&self) -> &[usize] {
        &self.source_indices
    }
    pub unsafe fn get_value_unchecked(
        &self,
        index: usize,
    ) -> (&Token, &[u8], &str) {
        let rng_bytes = (
            self.bytes_indices.get_unchecked(index),
            self.bytes_indices.get_unchecked(index + 1),
        );
        let rng_source = (
            self.source_indices.get_unchecked(index),
            self.source_indices.get_unchecked(index + 1),
        );
        (
            self.tl.get_unchecked(index),
            self.bytes.get_unchecked(*rng_bytes.0 .. *rng_bytes.1),
            self.source.get_unchecked(*rng_source.0 .. *rng_source.1),
        )
    }
    pub fn at(&self, index: usize) -> Option<(&Token, &[u8], &str)> {
        if index < self.len() {
            Some(unsafe { self.get_value_unchecked(index) })
        } else {
            None
        }
    }
    pub fn tl_get(&self, span: impl CategorySpan) -> Option<&[Token]> {
        self.tokenlist().get(span)
    }
    pub fn source_get(&self, span: impl CategorySpan) -> Option<&str> {
        let s = self.source_indices().get(span)?;
        if s.is_empty() {
            return Some("");
        }
        if s.last() == self.source_indices.last() {
            if s.len() == 1 {
                return Some("");
            }
            self.source.get(s[0] .. s[s.len() - 1])
        } else {
            // We can asure that s.last() is not the last value of self.source_indices,
            // hence, the next position of the s.last() can be got safely.
            let last =
                unsafe { *(s.last().unwrap() as *const usize).offset(1) };
            self.source.get(s[0] .. last)
        }
    }
    pub fn bytes_get(&self, span: impl CategorySpan) -> Option<&[u8]> {
        let s = self.bytes_indices().get(span)?;
        if s.is_empty() {
            return Some(&[]);
        }
        if s.last() == self.bytes_indices.last() {
            if s.len() == 1 {
                return Some(&[]);
            }
            self.bytes.get(s[0] .. s[s.len() - 1])
        } else {
            // We can asure that s.last() is not the last value of self.bytes_indices,
            // hence, the next position of the s.last() can be got safely.
            let last =
                unsafe { *(s.last().unwrap() as *const usize).offset(1) };
            self.bytes.get(s[0] .. last)
        }
    }
    pub fn to_str_repr(&self) -> String {
        self.tl.iter().map(|v| v.to_str_repr()).collect()
    }
    pub fn to_string(&self) -> String {
        self.tl.iter().map(|v| v.to_string()).collect()
    }
}
impl AsRef<[Token]> for SourcedTokenList {
    fn as_ref(&self) -> &[Token] {
        self.tokenlist()
    }
}

fn parse_tokenlist<'c, 'i: 'c, 'l: 'c + 'i>(
    tokenlist: &mut Vec<Token>,
    bytes: &mut Vec<u8>,
    bytes_indices: &mut Vec<usize>,
    source_indices: &mut Vec<usize>,
    source: &str,
    catcode: &'c mut CatCodeStack<'i>,
    (lexer, ctabset): (&'l LexerType, &'l CTabSet),
) {
    if source.is_empty() {
        return;
    }

    let range_stack: RefCell<Vec<&Category>> = RefCell::new(Vec::new());
    let mut cs_name = String::new();
    let mut escape_char = u32::MAX;
    let mut collect_cs = false;
    let mut next_char: Option<char> = None;
    let mut chr;
    let mut row = 0;
    let mut col = 0;
    let mut source_index = 0;
    let mut bytes_index = 0;
    let mut s_iter = source.chars();
    let mut curr_char_len;
    let mut cat;

    let mut last_pos = (0, 1, 0); // needed for the first update (0, 0, 0)
    macro_rules! update_ctab {
        (None) => {
            if last_pos != (row, col, tokenlist.len()) {
                last_pos = (row, col, tokenlist.len());
                if let Err(e) = catcode.update(
                    (lexer, ctabset),
                    &Position {
                        row,
                        col,
                        tl: None,
                        bytes: None,
                        source: None,
                    },
                    range_stack.borrow_mut(),
                ) {
                    warn!("{}", e);
                }
            }
        };
        () => {
            if last_pos != (row, col, tokenlist.len()) {
                if last_pos.2 == tokenlist.len() {
                    if let Err(e) = catcode.update(
                        (lexer, ctabset),
                        &Position {
                            row,
                            col,
                            tl: None,
                            bytes: None,
                            source: None,
                        },
                        range_stack.borrow_mut(),
                    ) {
                        warn!("{}", e);
                    }
                } else {
                    if let Err(e) = catcode.update(
                        (lexer, ctabset),
                        &Position {
                            row,
                            col,
                            tl: Some(&tokenlist),
                            bytes: Some(&bytes),
                            source: Some(&source[.. source_index]),
                        },
                        range_stack.borrow_mut(),
                    ) {
                        warn!("{}", e);
                    }
                }
            }
            last_pos = (row, col, tokenlist.len());
        };
    }

    update_ctab!(None);
    row += 1;
    update_ctab!(None);

    loop {
        (chr, curr_char_len) = if let Some(nc) = next_char {
            next_char = None;
            (nc, 0)
        } else if let Some(nc) = s_iter.next() {
            let len = nc.len_utf8();
            source_index += len;
            col += 1;
            update_ctab!();
            (nc, len)
        } else {
            break;
        };

        if chr == '\r' {
            if collect_cs {
                update_ctab!();
                let mut t = ControlSequence::new_cwo(&cs_name);
                t.set_escape_char(escape_char);
                let t = Token::CS(t);
                let b = t.to_bytes();
                bytes_index += b.len();
                tokenlist.push(t);
                bytes.extend(b.iter());
                bytes_indices.push(bytes_index);
                source_indices.push(source_index - curr_char_len);
                cs_name.clear();
                collect_cs = false;
            }
            continue;
        }
        if chr == '\n' {
            update_ctab!();
            if let Some(new_line_char) = catcode.endline_char() {
                row += 1;
                col = 0;
                chr = new_line_char;
            } else {
                row += 1;
                col = 0;

                let t = Token::Any(chr as u32);
                let b = t.to_bytes();
                bytes_index += b.len();
                tokenlist.push(t);
                bytes.extend(b.iter());
                bytes_indices.push(bytes_index);
                source_indices.push(source_index);
                update_ctab!();
                continue;
            }
        }

        update_ctab!();
        cat = catcode.catcode_value(chr).unwrap_or_default();

        let chr = if cat == CatCode::Superscript {
            let (chr, n) = circumflex_mechanism(catcode, s_iter.clone(), chr);
            let s_start = s_iter.as_str().len();
            s_iter.advance_by(n).unwrap();
            let cir_len = s_start - s_iter.as_str().len();
            source_index += cir_len;
            col += cir_len as u32;
            curr_char_len += cir_len;
            update_ctab!();
            cat = catcode.catcode_value(chr).unwrap_or_default();
            chr
        } else {
            chr
        };

        if collect_cs {
            if cat == CatCode::Letter {
                cs_name.push(chr);
                continue;
            } else if cs_name.is_empty() {
                let mut t = ControlSequence::new_csy(chr);
                t.set_escape_char(escape_char);
                let t = Token::CS(t);
                let b = t.to_bytes();
                bytes_index += b.len();
                tokenlist.push(t);
                bytes.extend(b.iter());
                bytes_indices.push(bytes_index);
                source_indices.push(source_index);
                collect_cs = false;
                continue;
            } else {
                let mut t = ControlSequence::new_cwo(&cs_name);
                t.set_escape_char(escape_char);
                let t = Token::CS(t);
                let b = t.to_bytes();
                bytes_index += b.len();
                tokenlist.push(t);
                bytes.extend(b.iter());
                bytes_indices.push(bytes_index);
                source_indices.push(source_index - curr_char_len);
                cs_name.clear();
                collect_cs = false;
            }
        }
        update_ctab!();
        if cat == CatCode::Escape {
            collect_cs = true;
            escape_char = chr as u32;
        } else {
            let t = Token::Char(Character { charcode: chr, catcode: cat });
            let b = t.to_bytes();
            bytes_index += b.len();
            tokenlist.push(t);
            bytes.extend(b.iter());
            bytes_indices.push(bytes_index);
            source_indices.push(source_index);
        }
    }

    if !cs_name.is_empty() {
        let last_char = cs_name.chars().next_back().unwrap();
        update_ctab!();
        let _ = last_pos;
        let mut t =
            if catcode.catcode_value(last_char) == Some(CatCode::Letter) {
                ControlSequence::new_cwo(&cs_name)
            } else {
                ControlSequence::new_csy(last_char)
            };
        t.set_escape_char(escape_char);
        let t = Token::CS(t);
        let b = t.to_bytes();
        bytes_index += b.len();
        tokenlist.push(t);
        bytes.extend(b.iter());
        bytes_indices.push(bytes_index);
        source_indices.push(source_index);
        cs_name.clear();
    }
}

impl Into<TokenListBytes> for SourcedTokenList {
    fn into(self) -> TokenListBytes {
        TokenListBytes { values: self.bytes, lens: self.bytes_indices }
    }
}
impl<'b> Into<TokenListBytesRef<'b>> for &'b SourcedTokenList {
    fn into(self) -> TokenListBytesRef<'b> {
        unsafe {
            TokenListBytesRef::new(&self.tl, &self.bytes, &self.bytes_indices)
        }
    }
}

pub struct SourcedFormatter<'a> {
    high_config: &'a HighConfig,
    tokenlist: Arc<SourcedTokenList>,
    group_level: Cell<isize>,
    index: Cell<usize>, // index <= tokenlist.tokenlist().len()
    range: Cell<Option<(&'a str, RangeKind)>>,
    in_comment: Cell<bool>,
}

#[derive(Debug, Clone, Copy)]
struct RangeKind {
    start: usize,
    end: usize,
    inner: RangeInnerKind,
}
#[derive(Debug, Clone, Copy)]
enum RangeInnerKind {
    NoneArg {
        range_start: usize,
        insert_brace: bool,
    },
    OneArg {
        range_start: usize,
        arg_start: usize,
        arg_end: usize,
        insert_brace: bool,
    },
    Escape {
        range_start: usize,
        arg_start: usize,
        insert_brace: bool,
    },
    // len <= 9, index <= 9
    // step: end pos of arg_i - arg_start
    // spec: arg spec name, ascii letter
    Normal {
        arg_start: usize,
        len: u8,
        index: u8,
        step: [u32; 9],
        presents: ArgsPresent,
        spec: [u8; 9],
    },
}
bitflags! {
    #[derive(Debug, Clone, Copy)]
    struct ArgsPresent: u16 {
        const One = 1 << 0;
        const Two = 1 << 1;
        const Three = 1 << 2;
        const Four = 1 << 3;
        const Five = 1 << 4;
        const Six = 1 << 5;
        const Seven = 1 << 6;
        const Eight = 1 << 7;
        const Nine = 1 << 8;
    }
}
impl ArgsPresent {
    fn has(&self, n: u8) -> bool {
        n > 0
            && n < 9
            && self.contains(ArgsPresent::from_bits_retain(1 << n - 1))
    }
}

impl<'a> SourcedFormatter<'a> {
    pub fn format_now<T: HWrite>(
        &self,
        stream: &mut T,
    ) -> Result<(), ErrorKind> {
        let close_el = !self.tokenlist.is_empty();
        if close_el {
            self.fmt_raw(stream, format_args!("\\THls"))?;
        }

        let mut next_token: Option<&Token> = None;
        let mut tokenlist_iter = self.tokenlist.tl.iter();
        let mut fmt_s = String::new();
        loop {
            let token = match next_token {
                Some(token) => {
                    next_token = None;
                    token
                }
                None => {
                    self.detect_range();
                    if let Some(n) = self.write_range(stream)? {
                        self.index.set(self.index.get() + n);
                        tokenlist_iter.advance_by(n).unwrap();
                        if self.range.get().is_none() || n > 0 {
                            // if range is cleared, we need detect a new one
                            continue;
                        }
                    }
                    match tokenlist_iter.next() {
                        Some(token) => {
                            self.index.set(self.index.get() + 1);
                            token
                        }
                        None => break,
                    }
                }
            };

            match token {
                Token::Char(chr) if self.is_newline(chr) => {
                    self.fmt_newline(stream)?;
                }
                Token::Char(chr)
                    if matches!(
                        chr.catcode,
                        CatCode::Letter | CatCode::Other
                    ) =>
                {
                    fmt_s.clear();
                    next_token = Self::write_string(
                        self,
                        &mut tokenlist_iter,
                        &mut fmt_s,
                        chr,
                    )?;
                    self.fmt_string(stream, &fmt_s)?;
                }
                Token::Char(chr) if chr.catcode == CatCode::Comment => {
                    fmt_s.clear();
                    let last_token = Self::write_comment(
                        self,
                        &mut tokenlist_iter,
                        &mut fmt_s,
                        chr,
                    )?;
                    self.fmt_raw(
                        stream,
                        format_args!(
                            "\\THrs{{{0}}}{1}\\THre{{{0}}}",
                            "comment", &fmt_s
                        ),
                    )?;
                    if let Some(last_token) = last_token {
                        // the range may end at endline,
                        // we subtract index by 1 in write_comment, now add it back
                        if self.range.get().is_some() {
                            // at this position, we may replace a range by the range before the comment
                            // now write the range which is before the comment
                            self.write_range(stream)?;
                            // then try detect range, if a range start with newline,
                            // it was detected in comment, but it was replaced, we now detect it
                            self.detect_range();
                            // if found a range, it must start with newline,
                            // we will skip newline in write_range, so it's ok
                            self.write_range(stream)?;
                        }
                        self.index.set(self.index.get() + 1);
                        self.fmt_token(stream, last_token)?;
                    }
                }
                Token::Char(chr) if chr.catcode == CatCode::MathShift => {
                    fmt_s.clear();
                    let mut is_succ = true;
                    Self::write_inline_math(
                        self,
                        &mut tokenlist_iter,
                        &mut fmt_s,
                        chr,
                        &mut is_succ,
                    )?;
                    if is_succ {
                        self.fmt_raw(
                            stream,
                            format_args!(
                                "\\THrs{{{0}}}{1}\\THre{{{0}}}",
                                "math.inline", &fmt_s
                            ),
                        )?;
                    } else {
                        self.fmt_token(stream, token)?;
                    }
                }
                Token::Char(chr) if chr.catcode == CatCode::Parameter => {
                    self.write_parameter(&mut tokenlist_iter, stream, chr)?;
                }
                _ => {
                    self.fmt_token(stream, token)?;
                }
            }
        }

        if close_el {
            self.fmt_raw(stream, format_args!("\\THle"))?;
        }
        Ok(())
    }
    pub fn new<'h>(
        high_config: &'a HighConfig,
        tokenlist: Arc<SourcedTokenList>,
    ) -> Self {
        Self {
            high_config,
            tokenlist,
            group_level: Cell::new(0),
            index: Cell::new(0),
            range: Cell::new(None),
            in_comment: Cell::new(false),
        }
    }
    fn detect_range(&self) -> Option<usize> {
        use crate::config::{Category, RangeItem};

        // current token position
        let curr_pos = self.index.get(); // end <= tl.len()
                                         // the position after rangeitem.start
                                         // let arg_pos = curr_pos; // defined later

        let ranges = &self.high_config.ranges;
        let tl = self.tokenlist.tokenlist();

        if self.range.get().is_some()
            || ranges.is_empty()
            || curr_pos > tl.len()
        {
            return None;
        }

        // find the argument position (ie, position after the start)
        // given source: % \startmacro{.}[.](.),
        // the tl: ('%', 14)(' ', 10)("startmacro")('{', 1)('.', 10)('}', 2)...
        // if str or re match '% \start', the arg pos is after \startmacro, it cannot break cs
        // if regtex match '%\ \c{startmacro}', the arg pos is after \startmacro
        //
        // use captures to skip unnecessary tokens??
        let is_comment = self.in_comment.get();
        let (arg_pos, key, item) = ranges.iter().find_map(|(k, r)| {
            let (start, in_comments) = match r {
                RangeItem::Escape { start, in_comments, .. } => (start, in_comments),
                RangeItem::Normal { start, in_comments, .. } => (start, in_comments),
            };

            if (is_comment && *in_comments == RangeComments::Forbidden)
                ||(!is_comment && *in_comments == RangeComments::Required) {
                return None;
            }

            let arg_pos = match start {
                Category::Any => curr_pos,
                Category::None => return None,
                Category::Span(_) => return None,
                Category::String(s) => {
                    let source =
                        self.tokenlist.source_get(curr_pos ..).unwrap();
                    if !source.starts_with(s) {
                        return None;
                    }
                    let source_end =
                        self.tokenlist.source_indices()[curr_pos] + s.len();
                    let arg_pos = match self
                        .tokenlist
                        .source_indices()
                        .binary_search(&source_end)
                    {
                        Ok(count) => count,
                        Err(count) => count,
                    };
                    arg_pos
                }
                Category::Regex(re) => {
                    let source =
                        self.tokenlist.source_get(curr_pos ..).unwrap();
                    let source_end =
                        self.tokenlist.source_indices()[curr_pos] + re.find(source)?.end();
                    let arg_pos = match self
                        .tokenlist
                        .source_indices()
                        .binary_search(&source_end)
                    {
                        Ok(count) => count,
                        Err(count) => count,
                    };
                    arg_pos
                }
                Category::RegTEx(re) => {
                    let bytes = self.tokenlist.bytes_get(curr_pos ..).unwrap();
                    let token_counts = unsafe {
                        let end = re.find_bytes(bytes)?.end();
                        TokenBytes::tokens_len_unchecked(&bytes[.. end])
                    };
                    curr_pos + token_counts
                }
            };
            log::trace!(
                "Found range [key: {:?}] {{ start_pos: {}, arg_pos: {}, item: {:?} }}",
                k, curr_pos, arg_pos, r
            );
            Some((arg_pos, k, r))
        })?;

        let arg_pos = if item.start_is_arg() { curr_pos } else { arg_pos };
        // curr_pos <= arg_pos <= tl.len
        assert!(arg_pos <= tl.len());
        let args_tl = unsafe { tl.get_unchecked(arg_pos ..) };

        let mut end = arg_pos;
        let range_inner = match item {
            RangeItem::Escape {
                start: _,
                arguments,
                remove_start,
                insert_brace,
                use_argument,
                insert_ending,
                in_comments: _,
                start_is_arg: _,
            } => {
                let args = match arguments.find_all(args_tl) {
                    Ok(args) => args,
                    Err(e) => {
                        log::warn!(
                            "I cannot parse arguments cause: {:?}, at: {}, source: {:?}",
                            e, arg_pos,
                            self.tokenlist
                                .source_get(curr_pos..).unwrap_or_default()
                                .chars().take(40).collect::<String>()
                        );
                        return None;
                    }
                };
                if args.len() == 0 {
                    let range_start =
                        if *remove_start { arg_pos } else { curr_pos };
                    RangeInnerKind::NoneArg {
                        range_start,
                        insert_brace: *insert_brace,
                    }
                } else if args.len() == 1 {
                    let the_arg = &args[0];
                    let the_spec_name =
                        *unsafe { arguments.spec_names().get_unchecked(0) };

                    end = arg_pos + the_arg.end();

                    let (arg_start, arg_end) = if *remove_start
                        && *use_argument
                    {
                        match the_arg {
                            &Argument::UnPresent(e_step) => {
                                (arg_pos + e_step, arg_pos + e_step)
                            }
                            &Argument::Present(s_step, e_step) => {
                                match the_spec_name {
                                    b'o' | b'O' | b'd' | b'D' | b'g'
                                    | b'G' => (
                                        arg_pos + s_step + 1,
                                        arg_pos + e_step - 1,
                                    ),
                                    b's' | b't' | _ => {
                                        (arg_pos + s_step, arg_pos + e_step)
                                    }
                                }
                            }
                            &Argument::Span(s_step, e_step) => {
                                match the_spec_name {
                                    b'm' => {
                                        if e_step - 1 == s_step {
                                            (
                                                arg_pos + s_step,
                                                arg_pos + e_step,
                                            )
                                        } else {
                                            (
                                                arg_pos + s_step + 1,
                                                arg_pos + e_step - 1,
                                            )
                                        }
                                    }
                                    b'r' | b'R' | b'v' => (
                                        arg_pos + s_step + 1,
                                        arg_pos + e_step - 1,
                                    ),
                                    b'l' | _ => {
                                        (arg_pos + s_step, arg_pos + e_step)
                                    }
                                }
                            }
                            &Argument::Ending(arg_e_step, e_step) => {
                                match the_spec_name {
                                    b'u' | b'U' | b'\n' | _ => {
                                        if *insert_ending {
                                            end -= e_step - arg_e_step;
                                        }
                                        (arg_pos, arg_pos + arg_e_step)
                                    }
                                }
                            }
                        }
                    } else {
                        match the_arg {
                            &Argument::UnPresent(e_step) => {
                                (arg_pos + e_step, arg_pos + e_step)
                            }
                            &Argument::Present(s_step, e_step) => {
                                (arg_pos + s_step, arg_pos + e_step)
                            }
                            &Argument::Span(s_step, e_step) => {
                                (arg_pos + s_step, arg_pos + e_step)
                            }
                            &Argument::Ending(_, e_step) => {
                                (arg_pos, arg_pos + e_step)
                            }
                        }
                    };
                    let range_start = if *remove_start {
                        if *use_argument {
                            arg_start
                        } else {
                            arg_pos
                        }
                    } else {
                        curr_pos
                    };
                    RangeInnerKind::OneArg {
                        range_start,
                        arg_start,
                        arg_end,
                        insert_brace: *insert_brace,
                    }
                } else {
                    let last_arg =
                        unsafe { args.get_unchecked(args.len() - 1) };
                    end = arg_pos + last_arg.end();
                    if *insert_ending {
                        match last_arg {
                            Argument::Ending(arg_e_step, e_step) => {
                                end -= e_step - arg_e_step;
                            }
                            _ => {}
                        }
                    }
                    let range_start =
                        if *remove_start { arg_pos } else { curr_pos };
                    RangeInnerKind::Escape {
                        range_start,
                        arg_start: arg_pos,
                        insert_brace: *insert_brace,
                    }
                }
            }
            RangeItem::Normal {
                start: _,
                arguments,
                insert_ending,
                in_comments: _,
                start_is_arg: _,
            } => {
                let args = match arguments.find_all(args_tl) {
                    Ok(args) => args,
                    Err(e) => {
                        log::warn!(
                            "I cannot parse arguments cause: {:?}, at: {}, source: {:?}",
                            e, arg_pos,
                            self.tokenlist
                                .source_get(curr_pos..).unwrap_or_default()
                                .chars().take(40).collect::<String>()
                        );
                        return None;
                    }
                };
                assert!(args.len() <= 9);
                assert_eq!(arguments.spec_names().len(), args.len());
                let len = args.len() as u8;

                let mut spec = [0; 9];
                spec[.. args.len()].copy_from_slice(&arguments.spec_names());

                let mut step = [0; 9];
                let mut presents = ArgsPresent::empty();
                if args.len() == 0 {
                    end = arg_pos;
                } else {
                    let mut last_end = 0;
                    for (i, r) in args.iter().enumerate() {
                        match r {
                            Argument::UnPresent(e) => {
                                if last_end != *e {
                                    step[i] = *e as u32;
                                }
                            }
                            _ => {
                                presents |=
                                    ArgsPresent::from_bits_retain(1 << i);
                                last_end = r.end();
                                step[i] = last_end as u32;
                            }
                        }
                    }
                    let last_arg = &args[args.len() - 1];
                    end = arg_pos + last_arg.end();
                    if *insert_ending {
                        match last_arg {
                            Argument::Ending(s_end, e_end) => {
                                let comp = *e_end - *s_end;
                                end -= comp;
                                step[args.len() - 1] -= comp as u32;
                            }
                            _ => {}
                        }
                    }
                }

                RangeInnerKind::Normal {
                    arg_start: arg_pos,
                    len,
                    index: 0,
                    presents,
                    step,
                    spec,
                }
            }
        };

        let range = RangeKind { start: curr_pos, end, inner: range_inner };
        log::trace!("{:?}", &range);
        self.range.set(Some((key, range)));
        Some(curr_pos)
    }
    fn is_newline_at(&self, pos: usize) -> bool {
        if pos >= self.tokenlist.len() {
            return true;
        }
        match &self.tokenlist.tokenlist()[pos] {
            Token::Char(_) => {
                let source = unsafe { self.source_at(pos + 1) };
                matches!(source, "\r" | "\n" | "\r\n" | "\n\r")
            }
            _ => false,
        }
    }
    fn write_range<'t, T: HWrite>(
        &self,
        stream: &mut T,
    ) -> Result<Option<usize>, ErrorKind> {
        fn fmt_spec(spec: u8) -> CompactString {
            match spec {
                b'\x00' .. b'\x20' => {
                    format_compact!("^^{}", (spec + 0x40) as char)
                }
                b'\x7f' => format_compact!("^^{}", '\x3f'),
                _ => format_compact!("{}", spec as char),
            }
        }

        let Some((range_name, range)) = self.range.get() else {
            return Ok(None);
        };
        let curr_pos = self.index.get();
        if curr_pos < range.start || curr_pos > self.tokenlist.len() {
            return Ok(None);
        }
        if curr_pos == range.start {
            if self.is_newline_at(curr_pos) {
                let new_range = RangeKind {
                    start: range.start + 1,
                    end: range.end,
                    inner: range.inner,
                };
                log::trace!(
                    "Update range: {:?}, cause touching newline",
                    &new_range
                );
                self.range.set(Some((range_name, new_range)));
                return Ok(None);
            }

            let iter_step;
            let escape;
            let mut escape_brace = false;
            let mut possible = String::new();

            match range.inner {
                RangeInnerKind::NoneArg { range_start, insert_brace } => {
                    let source = self
                        .tokenlist
                        .source_get(range_start .. range.end)
                        .unwrap_or_default();
                    possible.push_str(source);
                    iter_step = range.end - range.start;
                    escape = true;
                    escape_brace = insert_brace;
                }
                RangeInnerKind::OneArg {
                    range_start,
                    arg_start,
                    arg_end,
                    insert_brace,
                } => {
                    let source_start = self
                        .tokenlist
                        .source_get(range_start .. arg_start)
                        .unwrap_or_default();
                    possible.push_str(source_start);
                    let source_arg = self
                        .tokenlist
                        .source_get(arg_start .. arg_end)
                        .unwrap_or_default();
                    possible.push_str(source_arg);
                    iter_step = range.end - range.start;
                    escape = true;
                    escape_brace = insert_brace;
                }
                RangeInnerKind::Escape {
                    range_start,
                    arg_start,
                    insert_brace,
                } => {
                    let source_start = self
                        .tokenlist
                        .source_get(range_start .. arg_start)
                        .unwrap_or_default();
                    possible.push_str(source_start);
                    let source_args = self
                        .tokenlist
                        .source_get(arg_start .. range.end)
                        .unwrap_or_default();
                    possible.push_str(source_args);
                    iter_step = range.end - range.start;
                    escape = true;
                    escape_brace = insert_brace;
                }
                RangeInnerKind::Normal {
                    arg_start,
                    index,
                    presents,
                    spec,
                    ..
                } => {
                    iter_step = 0;
                    escape = false;
                    if curr_pos == arg_start && presents.has(1) && index == 0 {
                        possible.push_str(&format!(
                            "\\THrs{{argument.{}}}",
                            fmt_spec(spec[0])
                        ));
                    }
                }
            }

            let real_name = HighConfig::real_name(range_name);
            let a = if escape {
                if escape_brace {
                    format_args!("\\THes{{{}}}{{{}", real_name, &possible)
                } else {
                    format_args!("\\THes{{{}}}{}", real_name, &possible)
                }
            } else {
                format_args!("\\THrs{{{}}}{}", real_name, &possible)
            };
            self.fmt_raw(stream, a)?;

            Ok(Some(iter_step))
        } else if curr_pos >= range.end {
            // use >=
            if curr_pos > range.end {
                log::warn!(
                    "I found current position ({}) > end of range '{}'",
                    curr_pos,
                    range_name
                );
            }
            self.range.set(None);
            let (escape, escape_brace) = match range.inner {
                RangeInnerKind::NoneArg { insert_brace, .. } => {
                    (true, insert_brace)
                }
                RangeInnerKind::OneArg { insert_brace, .. } => {
                    (true, insert_brace)
                }
                RangeInnerKind::Escape { insert_brace, .. } => {
                    (true, insert_brace)
                }
                RangeInnerKind::Normal {
                    arg_start: _,
                    len,
                    index,
                    step: _,
                    presents,
                    spec,
                } => {
                    if len > 0 && presents.has(index + 1) {
                        self.fmt_raw(
                            stream,
                            format_args!(
                                "\\THre{{argument.{}}}",
                                fmt_spec(spec[index as usize])
                            ),
                        )?;
                    }
                    (false, false)
                }
            };
            let real_name = HighConfig::real_name(range_name);
            let a = if escape {
                if escape_brace {
                    format_args!("}}\\THee{{{}}}", real_name)
                } else {
                    format_args!("\\THee{{{}}}", real_name)
                }
            } else {
                format_args!("\\THre{{{}}}", real_name)
            };
            self.fmt_raw(stream, a)?;
            Ok(Some(0))
        } else {
            let mut done = false;
            match range.inner {
                RangeInnerKind::NoneArg { .. } => {}
                RangeInnerKind::OneArg { .. } => {}
                RangeInnerKind::Escape { .. } => {}
                RangeInnerKind::Normal {
                    arg_start,
                    len,
                    index,
                    step,
                    presents,
                    spec,
                } => {
                    if len > index {
                        if curr_pos == arg_start && presents.has(1) {
                            done = true;
                            self.fmt_raw(
                                stream,
                                format_args!(
                                    "\\THrs{{argument.{}}}",
                                    fmt_spec(spec[0])
                                ),
                            )?;
                        }
                        let curr_end = step[index as usize];
                        let arg_index = arg_start + curr_end as usize;
                        if presents.has(index + 1) {
                            if curr_pos < arg_index {
                                return Ok(None);
                            }
                            if curr_pos == arg_index {
                                done = true;
                                self.fmt_raw(
                                    stream,
                                    format_args!(
                                        "\\THre{{argument.{}}}",
                                        fmt_spec(spec[index as usize])
                                    ),
                                )?;
                            }
                        }
                        if presents.has(index + 2) {
                            if curr_pos < arg_index {
                                return Ok(None);
                            }
                            if curr_pos == arg_index {
                                done = true;
                                self.fmt_raw(
                                    stream,
                                    format_args!(
                                        "\\THrs{{argument.{}}}",
                                        fmt_spec(spec[index as usize + 1])
                                    ),
                                )?;
                            }
                        }
                        let inner = RangeInnerKind::Normal {
                            arg_start,
                            len,
                            index: index + 1,
                            presents,
                            step,
                            spec,
                        };
                        let new_range = RangeKind { inner, ..range };
                        log::trace!(
                            "Update range: {:?}, cause stepping index",
                            &new_range
                        );
                        self.range.set(Some((range_name, new_range)));
                    }
                }
            }
            return Ok(done.then_some(0));
        }
    }
    fn is_range_bound(&self, index: usize, range: &RangeKind) -> bool {
        if range.start == index || range.end == index {
            return true;
        }
        match range.inner {
            RangeInnerKind::NoneArg { range_start, insert_brace: _ } => {
                range_start == index
            }
            RangeInnerKind::OneArg {
                range_start,
                arg_start,
                arg_end,
                insert_brace: _,
            } => {
                range_start == index || arg_start == index || arg_end == index
            }
            RangeInnerKind::Escape {
                range_start,
                arg_start,
                insert_brace: _,
            } => range_start == index || arg_start == index,
            RangeInnerKind::Normal {
                arg_start,
                len,
                index: r_idx,
                step,
                presents,
                spec: _,
            } => {
                if len == 0 || len == r_idx {
                    false
                } else {
                    let curr_arg_end = step[r_idx as usize];
                    arg_start == index
                        || (presents.has(r_idx + 1)
                            && (arg_start + curr_arg_end as usize == index))
                }
            }
        }
    }
    unsafe fn source_at(&self, index: usize) -> &str {
        let s_index_start =
            self.tokenlist.source_indices.get_unchecked(index - 1);
        let s_index_end = self.tokenlist.source_indices.get_unchecked(index);
        self.tokenlist.source.get_unchecked(*s_index_start .. *s_index_end)
    }
    pub fn source_of_current(&self) -> &str {
        unsafe { self.source_at(self.index.get()) }
    }
    fn write_string<'t, T: HWrite>(
        &self,
        tokens: &mut Iter<'t, Token>,
        stream: &mut T,
        chr: &Character,
    ) -> Result<Option<&'t Token>, ErrorKind> {
        self.fmt_chr_try_not(stream, chr)?;

        self.detect_range();
        if let Some((_, r)) = &self.range.get() {
            let curr_index = self.index.get();
            if self.is_range_bound(curr_index, r) {
                return Ok(None);
            } else {
                if let Some(n) = self.write_range(stream)? {
                    self.index.set(curr_index + n);
                    tokens.advance_by(n).unwrap();
                    self.detect_range();
                }
            }
        }
        while let Some(token) = tokens.next() {
            self.index.set(self.index.get() + 1);
            self.detect_range();
            match token {
                Token::Char(c)
                    if matches!(
                        c.catcode,
                        CatCode::Letter | CatCode::Other | CatCode::Space
                    ) && !self.is_newline(c) =>
                {
                    self.fmt_chr_try_not(stream, c)?;
                    if let Some((_, r)) = &self.range.get() {
                        if self.is_range_bound(self.index.get(), r) {
                            return Ok(None);
                        } else {
                            if let Some(n) = self.write_range(stream)? {
                                self.index.set(self.index.get() + n);
                                tokens.advance_by(n).unwrap();
                                self.detect_range();
                            }
                        }
                    }
                }
                _ => return Ok(Some(token)),
            }
        }
        Ok(None)
    }
    fn write_comment<'b, T: HWrite>(
        &self,
        tokens: &'b mut Iter<Token>,
        stream: &mut T,
        chr: &Character,
    ) -> Result<Option<&'b Token>, ErrorKind> {
        self.in_comment.set(true);

        // we got: %<index><tokens>..^^M
        let start_pos = self.index.get();
        let mut old_range = None;
        let has_range = match &self.range.get() {
            Some((k, r)) => match r.inner {
                RangeInnerKind::NoneArg { .. } => true,
                RangeInnerKind::OneArg { .. } => true,
                RangeInnerKind::Escape { .. } => true,
                RangeInnerKind::Normal {
                    arg_start,
                    len,
                    index: i_idx,
                    step,
                    presents,
                    spec,
                } => {
                    if len > 0 && i_idx == 0 && presents.has(1) {
                        let n = tokens
                            .clone()
                            .enumerate()
                            .position(|(i, _)| {
                                self.is_newline_at(start_pos + i)
                            })
                            .unwrap_or(tokens.as_slice().len());
                        if step[i_idx as usize] as usize + arg_start
                            == start_pos + n
                        {
                            let fake_range = RangeKind {
                                start: r.start,
                                end: r.end + 1,
                                inner: r.inner,
                            };
                            log::trace!(
                                "Update range: {:?}, cause in comment",
                                &fake_range
                            );
                            self.range.set(Some((*k, fake_range)));

                            let fake_present =
                                presents.difference(ArgsPresent::One);
                            let fake_inner = RangeInnerKind::Normal {
                                arg_start,
                                len,
                                index: i_idx,
                                step,
                                presents: fake_present,
                                spec,
                            };
                            let mut new_r = *r;
                            new_r.inner = fake_inner;
                            old_range = Some((*k, new_r));

                            false
                        } else {
                            true
                        }
                    } else {
                        true
                    }
                }
            },
            None => false,
        };

        let mut next_char = None;
        let mut fmt_s = String::new();
        self.fmt_chr(stream, chr)?;
        loop {
            let token = if let Some(nt) = next_char {
                next_char = None;
                nt
            } else {
                self.detect_range();
                if let Some(n) = self.write_range(stream)? {
                    self.index.set(self.index.get() + n);
                    tokens.advance_by(n).unwrap();
                    continue;
                }
                match tokens.next() {
                    Some(t) => {
                        self.index.set(self.index.get() + 1);
                        t
                    }
                    None => break,
                }
            };
            match token {
                Token::Char(chr) if self.is_newline(chr) => {
                    // we can asure index > 0, becasue at least one token exists
                    self.index.set(self.index.get() - 1);
                    next_char = Some(token);
                    break;
                }
                Token::Char(chr)
                    if matches!(
                        chr.catcode,
                        CatCode::Letter | CatCode::Other
                    ) =>
                {
                    next_char =
                        Self::write_string(self, tokens, &mut fmt_s, chr)?;
                    self.fmt_string(stream, &fmt_s)?;
                    fmt_s.clear();
                }
                Token::Char(c) => {
                    self.fmt_chr(stream, c)?;
                }
                Token::CS(cs) => self.fmt_cs(stream, cs)?,
                Token::Any(any) => {
                    if *any == ('\r' as u32) || *any == ('\n' as u32) {
                        next_char = Some(token);
                        break;
                    } else {
                        self.fmt_any(stream, *any)?;
                    }
                }
            }
        }

        self.in_comment.set(false);
        match &self.range.get() {
            Some((_, r)) => {
                if r.start >= start_pos && r.start < self.index.get() {
                    // range started in comment, but did not end in comment
                    log::error!("Range start: {}, string start: {}, current position: {}", r.start, start_pos, self.index.get());
                    return Err(ErrorKind::HighRangeError);
                }
            }
            None => {
                if has_range {
                    // range started before comment, but ended in comment
                    // but the following code will raise an error,
                    // start=^\c{cl}, argument='^^J{f}', and code:
                    // ```tex
                    // \cl aa % bb.
                    // ```
                    log::error!(
                        "String start: {}, current position: {}",
                        start_pos,
                        self.index.get()
                    );
                    return Err(ErrorKind::HighRangeError);
                }
            }
        }
        if old_range.is_some() {
            self.range.set(old_range);
        }
        Ok(next_char)
    }
    fn write_inline_math<T: HWrite>(
        &self,
        tokens: &mut Iter<Token>,
        stream: &mut T,
        chr: &Character,
        is_succ: &mut bool,
    ) -> Result<(), ErrorKind> {
        self.fmt_chr(stream, chr)?;
        let mut new_chars = tokens.clone();
        let mut group_level = 0;

        let curr_group_level = self.group_level.get();
        let curr_in_comment = self.in_comment.get();
        let curr_index = self.index.get();
        let curr_range = self.range.get();

        loop {
            self.detect_range();
            if let Some(n) = self.write_range(stream)? {
                self.index.set(self.index.get() + n);
                new_chars.advance_by(n).unwrap();
                continue;
            }
            let Some(token) = new_chars.next() else {
                *is_succ = false;
                break;
            };
            self.index.set(self.index.get() + 1);
            match token {
                Token::Char(c) => {
                    self.fmt_chr(stream, c)?;
                    if c.catcode == CatCode::BeginGroup {
                        group_level += 1;
                    } else if c.catcode == CatCode::EndGroup {
                        group_level -= 1;
                    }
                    if group_level < 0 {
                        *is_succ = false;
                        break;
                    }
                    if c.catcode == CatCode::MathShift {
                        break;
                    }
                }
                Token::CS(cs) => self.fmt_cs(stream, cs)?,
                Token::Any(any) => self.fmt_any(stream, *any)?,
            }
        }

        if *is_succ {
            *tokens = new_chars;
        } else {
            self.group_level.set(curr_group_level);
            self.in_comment.set(curr_in_comment);
            self.index.set(curr_index);
            self.range.set(curr_range);
        }
        Ok(())
    }
    fn write_parameter<T: HWrite>(
        &self,
        tokens: &mut Iter<Token>,
        stream: &mut T,
        chr: &Character,
    ) -> Result<(), ErrorKind> {
        let ref mut s = String::new();
        let mut tokens_iter = tokens.clone();
        let mut succ = false;
        match tokens_iter.next() {
            Some(nt) => match nt {
                Token::Char(nc)
                    if nc.catcode == CatCode::Parameter
                        || (nc.catcode == CatCode::Other
                            && nc.charcode.is_ascii_digit()) =>
                {
                    self.detect_range();
                    self.write_range(s)?;
                    if s.len() == 0 {
                        succ = true;

                        self.fmt_raw(
                            s,
                            format_args!("\\THrs{{{}}}", "parameter"),
                        )?;
                        self.fmt_chr(s, chr)?;
                        self.index.set(self.index.get() + 1);
                        self.fmt_chr(s, nc)?;
                        self.fmt_raw(
                            s,
                            format_args!("\\THre{{{}}}", "parameter"),
                        )?;
                    }
                }
                _ => {}
            },
            None => {}
        }
        if succ {
            *tokens = tokens_iter;
            stream.write_str(s)
        } else {
            self.fmt_chr(stream, chr)
        }
    }
    fn fmt_chr_try_not<T: HWrite>(
        &self,
        stream: &mut T,
        chr: &Character,
    ) -> Result<(), ErrorKind> {
        let chr_s = self.source_of_current();
        let cc = self.get_chr_catogery(chr);
        let chr_e = if self.high_config.char_replacements.is_empty() {
            escape_string_small(chr_s, b'^')
        } else {
            escape_string_filter(
                chr_s,
                b'^',
                |v| self.high_config.char_replacements.contains(&v),
                |v| self.get_char_replacement(v),
            )
        };
        if &cc != "?" {
            self.fmt_raw(
                stream,
                format_args!("\\THch{{{}}}{{{}}}", &cc, &chr_e),
            )?;
        } else if chr.is_punct() {
            self.fmt_raw(stream, format_args!("\\THpn{{?}}{{{}}}", &chr_e))?;
        } else {
            if chr_s == &chr_e {
                self.fmt_raw(stream, format_args!("{}", chr_s))?;
            } else {
                self.fmt_raw(
                    stream,
                    format_args!("\\THch{{{}}}{{{}}}", &cc, &chr_e),
                )?;
            }
        }

        if self.high_config.break_at.contains(chr.charcode) {
            let tokenlist = self.tokenlist.as_ref();
            let index = self.index.get();
            let no_break = self
                .high_config
                .do_not_break
                .iter()
                .any(|v| v.contains_at(tokenlist, .. index));
            if no_break {
                log::trace!("Do not break before the token: {:?}", chr);
            } else {
                self.fmt_break(stream, "char")?;
            }
        }

        Ok(())
    }
}
impl<'a> HighFormat for SourcedFormatter<'a> {
    fn get_cs_catogery(&self, cs: &ControlSequence) -> CompactString {
        let csname = cs.get_csname();
        match self.high_config.cs_categories.categories(csname) {
            Some(cs_cat) => {
                CompactString::new(HighConfig::sanitize_name(cs_cat))
            }
            None => {
                let engine = primitive_engine(csname);
                if engine.is_empty() {
                    match get_cs_type_re(csname) {
                        LaTeXType::L3Primitive => "latex3.primitive",
                        LaTeXType::L3FunctionInternal => {
                            "latex3.function.internal"
                        }
                        LaTeXType::L3FunctionPublic => {
                            "latex3.function.public"
                        }
                        LaTeXType::L3FunctionKernel => {
                            "latex3.function.kernel"
                        }
                        LaTeXType::L3VariableInternal => {
                            "latex3.variable.internal"
                        }
                        LaTeXType::L3VariablePublic => {
                            "latex3.variable.public"
                        }
                        LaTeXType::L3VariableKernel => {
                            "latex3.variable.kernel"
                        }
                        LaTeXType::DocumentSmallCarmel if csname.len() > 1 => {
                            "latex.programming"
                        }
                        LaTeXType::DocumentPascal => "latex.programming",
                        LaTeXType::L2eInternal => "latex.internal",
                        LaTeXType::L2eKernel => "latex.internal",
                        LaTeXType::Punctuation => "?",
                        LaTeXType::Other | _ => "?",
                    }
                    .to_compact_string()
                } else {
                    format_compact!("primitive.{}", engine)
                }
            }
        }
    }
    fn get_chr_catogery(&self, chr: &Character) -> CompactString {
        if let Some(category) =
            self.high_config.char_categories.categories(chr.charcode)
        {
            CompactString::new(HighConfig::sanitize_name(category))
        } else if matches!(chr.catcode, CatCode::BeginGroup) {
            self.group_level.set(self.group_level.get() + 1);
            format_compact!("group.{}", self.group_level.get())
        } else if matches!(chr.catcode, CatCode::EndGroup) {
            let level = self.group_level.get();
            self.group_level.set(level - 1);
            format_compact!("group.{}", level)
        } else if !matches!(chr.catcode, CatCode::Letter | CatCode::Other) {
            format_compact!("catcode.{}", chr.catcode as u8)
        } else {
            CompactString::const_new("?")
        }
    }
    fn is_newline(&self, _: &Character) -> bool {
        let source = self.source_of_current();
        // should take catcode into consideration?
        matches!(source, "\r" | "\n" | "\r\n" | "\n\r")
    }
    fn fmt_newline<T: HWrite>(&self, stream: &mut T) -> Result<(), ErrorKind> {
        self.fmt_raw(stream, format_args!("\\THle\n\\THls"))
    }
    fn fmt_chr<T: HWrite>(
        &self,
        stream: &mut T,
        chr: &Character,
    ) -> Result<(), ErrorKind> {
        if self.is_newline(chr) {
            self.fmt_newline(stream)
        } else {
            let source = self.source_of_current();
            let escaped = if self.high_config.char_replacements.is_empty() {
                escape_string_small(source, b'^')
            } else {
                escape_string_filter(
                    source,
                    b'^',
                    |v| self.high_config.char_replacements.contains(&v),
                    |v| self.get_char_replacement(v),
                )
            };
            self.fmt_raw(
                stream,
                format_args!(
                    "\\THch{{{}}}{{{}}}",
                    &self.get_chr_catogery(chr),
                    &escaped
                ),
            )?;

            if self.high_config.break_at.contains(chr.charcode) {
                let tokenlist = self.tokenlist.as_ref();
                let index = self.index.get();
                let no_break = self
                    .high_config
                    .do_not_break
                    .iter()
                    .any(|v| v.contains_at(tokenlist, .. index));
                if no_break {
                    log::trace!("Do not break before the token: {:?}", chr);
                } else {
                    self.fmt_break(stream, "char")?;
                }
            }

            Ok(())
        }
    }
    fn fmt_cs<T: HWrite>(
        &self,
        stream: &mut T,
        cs: &ControlSequence,
    ) -> Result<(), ErrorKind> {
        let tokenlist = self.tokenlist.as_ref();
        let index = self.index.get();
        let no_break = self
            .high_config
            .do_not_break
            .iter()
            .any(|v| v.contains_at(tokenlist, .. index));

        if no_break {
            log::trace!("Do not break before the token: {:?}", cs);
        } else {
            self.fmt_break(stream, "char")?;
        }

        let cs_source = unsafe { self.source_at(index) };
        let escape_char_len = cs_source
            .chars()
            .next()
            .expect(&format!("Invalid token at index {}", index))
            .len_utf8();
        assert!(
            escape_char_len > 0,
            "{:?} is not a cs, at index {}",
            cs_source,
            self.index.get()
        );

        let (escaped_escape_char, escaped_cs_name) = if self
            .high_config
            .char_replacements
            .is_empty()
        {
            (
                escape_string_small(
                    unsafe { cs_source.get_unchecked(0 .. escape_char_len) },
                    b'^',
                ),
                escape_string(
                    unsafe { cs_source.get_unchecked(escape_char_len ..) },
                    b'^',
                ),
            )
        } else {
            (
                escape_string_filter(
                    unsafe { cs_source.get_unchecked(0 .. escape_char_len) },
                    b'^',
                    |v| self.high_config.char_replacements.contains(&v),
                    |v| self.get_char_replacement(v),
                ),
                escape_string_filter(
                    unsafe { cs_source.get_unchecked(escape_char_len ..) },
                    b'^',
                    |v| self.high_config.char_replacements.contains(&v),
                    |v| self.get_char_replacement(v),
                ),
            )
        };
        self.fmt_raw(
            stream,
            format_args!(
                "\\THcs{{{}}}{{{}}}{{{}}}",
                self.get_cs_catogery(cs).as_str(),
                &escaped_escape_char,
                &escaped_cs_name
            ),
        )
    }
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::Read;
    use std::str::FromStr;

    use crate::config::{LexerAction, LexerCatCodeKind};
    use crate::high::Null;
    use crate::regtex::RegTEx;
    use crate::types::CTab;

    use super::*;

    #[test]
    fn basic() {
        let s = String::from(r#"\input{a\s^^74quote\ {"some$file"}.tex}\^^n"#);
        let ref mut lexer = LexerType::default();
        let ref ctabset = CTabSet::new_empty();
        let mut cat = CatCodeStack::new();
        cat.push(CTab::document());
        let stl =
            SourcedTokenList::parse(s.into(), &mut cat, (lexer, ctabset));

        {
            let stl = SourcedTokenList::parse(
                String::from(r"\a^^^^4e00lax").into(),
                &mut cat,
                (lexer, ctabset),
            );
            println!("{}", stl.to_str_repr());
            stl.source_indices().iter().for_each(|v| println!("{:?}", v));
        }

        assert_eq!(
            stl.tokenlist(),
            &[
                Token::CS(ControlSequence::new_cwo("input")),
                Token::Char(Character::new('{', CatCode::BeginGroup)),
                Token::Char(Character::new('a', CatCode::Letter)),
                Token::CS(ControlSequence::new_cwo("stquote")),
                Token::CS(ControlSequence::new_csp()),
                Token::Char(Character::new('{', CatCode::BeginGroup)),
                Token::Char(Character::new('"', CatCode::Other)),
                Token::Char(Character::new('s', CatCode::Letter)),
                Token::Char(Character::new('o', CatCode::Letter)),
                Token::Char(Character::new('m', CatCode::Letter)),
                Token::Char(Character::new('e', CatCode::Letter)),
                Token::Char(Character::new('$', CatCode::MathShift)),
                Token::Char(Character::new('f', CatCode::Letter)),
                Token::Char(Character::new('i', CatCode::Letter)),
                Token::Char(Character::new('l', CatCode::Letter)),
                Token::Char(Character::new('e', CatCode::Letter)),
                Token::Char(Character::new('"', CatCode::Other)),
                Token::Char(Character::new('}', CatCode::EndGroup)),
                Token::Char(Character::new('.', CatCode::Other)),
                Token::Char(Character::new('t', CatCode::Letter)),
                Token::Char(Character::new('e', CatCode::Letter)),
                Token::Char(Character::new('x', CatCode::Letter)),
                Token::Char(Character::new('}', CatCode::EndGroup)),
                Token::CS(ControlSequence::new_csy('.')),
            ]
        );
        assert_eq!(
            stl.source_indices(),
            &[
                0, 6, 7, 8, 19, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
                32, 33, 34, 35, 36, 37, 38, 39, 43
            ]
        );
        assert_eq!(stl.source_indices.len(), stl.bytes_indices.len());
    }

    #[test]
    fn index() {
        let s = String::from(r#"\input{a\s^^74quote\ {"some$file"}.tex}\^^n"#);
        let ref mut lexer = LexerType::default();
        let ref ctabset = CTabSet::new_empty();
        let mut cat = CatCodeStack::new();
        cat.push(CTab::document());
        let stl =
            SourcedTokenList::parse(s.into(), &mut cat, (lexer, ctabset));

        let (t, _b, s) = stl.at(0).unwrap();
        assert_eq!(t, &Token::new_cs("input"));
        assert_eq!(s, r"\input");

        let (t, _b, s) = stl.at(6).unwrap();
        assert_eq!(t, &Token::new_char('"', CatCode::Other));
        assert_eq!(s, "\"");

        let (t, _b, s) = stl.at(23).unwrap();
        assert_eq!(t, &Token::new_cs("."));
        assert_eq!(s, r"\^^n");

        assert_eq!(None, stl.at(24));

        assert_eq!(
            stl.tl_get(.. 5),
            Some(
                &[
                    Token::new_cs("input"),
                    Token::new_char('{', CatCode::BeginGroup),
                    Token::new_char('a', CatCode::Letter),
                    Token::new_cs("stquote"),
                    Token::new_cs(" "),
                ][..]
            )
        );
        assert_eq!(stl.source_get(.. 5), Some(r"\input{a\s^^74quote\ "));

        assert_eq!(stl.tl_get(23 ..), Some(&[Token::new_cs(".")][..]));
        assert_eq!(stl.source_get(23 ..), Some(r"\^^n"));
    }

    #[test]
    fn parse() {
        let s = r###"
% This is a sample LaTeX input file.  (Version of 12 August 2004.)
%
% A '%' character causes TeX to ignore all remaining text on the line,
% and is used for comments like this one.

\documentclass{article}      % Specifies the document class

                             % The preamble begins here.
\title{An Example Document}  % Declares the document's title.
\author{Leslie Lamport}      % Declares the author's name.
\date{January 21, 1994}      % Deleting this command produces today's date.

\newcommand{\ip}[2]{(#1, #2)}
                             % Defines \ip{arg1}{arg2} to mean
                             % (arg1, arg2).

%\newcommand{\ip}[2]{\langle #1 | #2\rangle}
                             % This is an alternative definition of
                             % \ip that is commented out.

\begin{document}             % End of preamble and beginning of text.

\maketitle                   % Produces the title.

This is an example input file.  Comparing it with
the output it generates can show you how to
produce a simple document of your own.

\section{Ordinary Text}      % Produces section heading.  Lower-level
                             % sections are begun with similar
                             % \subsection and \subsubsection commands.

The ends  of words and sentences are marked
  by   spaces. It  doesn't matter how many
spaces    you type; one is as good as 100.  The
end of   a line counts as a space.
        "###
        .to_string();
        let lexer = LexerType::default();
        let ctabset = CTabSet::new_empty();
        let mut cat = CatCodeStack::new();
        cat.push(CTab::document());
        let stl =
            SourcedTokenList::parse(s.into(), &mut cat, (&lexer, &ctabset));
        assert_eq!(stl.bytes_indices.len(), stl.tl.len() + 1);
        assert_eq!(stl.source_indices.len(), stl.tl.len() + 1);
        assert_eq!(stl.bytes_indices.last(), Some(stl.bytes.len()).as_ref());
        assert_eq!(stl.source_indices.last(), Some(stl.source.len()).as_ref());
    }

    #[test]
    fn lexer() {
        let s = r###"\def\foo:n{FOO}\ExplSyntaxOn
\cs_set:Npn \foo:n #1 # { [{#1}] }
\ExplSyntaxOff \foo:n
\foo:n \aaa_b
"###
        .to_string();
        let categories = vec![(
            Category::Span([2, 0]),
            Category::Span([3, 0]),
            LexerAction::CatCode(LexerCatCodeKind::CTab(String::from(
                "latex3",
            ))),
        )];
        let lexer = LexerType(categories);
        let mut ctabset = CTabSet::new_empty();
        ctabset.add("latex3", CTab::latex3());
        let mut cat = CatCodeStack::new();
        cat.push(CTab::document());
        let stl =
            SourcedTokenList::parse(s.into(), &mut cat, (&lexer, &ctabset));
        let result = &[
            Token::new_cs("def"),
            Token::new_cs("foo"),
            Token::new_char(':', CatCode::Other),
            Token::new_char('n', CatCode::Letter),
            Token::new_char('{', CatCode::BeginGroup),
            Token::new_char('F', CatCode::Letter),
            Token::new_char('O', CatCode::Letter),
            Token::new_char('O', CatCode::Letter),
            Token::new_char('}', CatCode::EndGroup),
            Token::new_cs("ExplSyntaxOn"),
            Token::new_char('\r', CatCode::EndLine),
            Token::new_cs("cs_set:Npn"),
            Token::new_char(' ', CatCode::Ignored),
            Token::new_cs("foo:n"),
            Token::new_char(' ', CatCode::Ignored),
            Token::new_char('#', CatCode::Parameter),
            Token::new_char('1', CatCode::Other),
            Token::new_char(' ', CatCode::Ignored),
            Token::new_char('#', CatCode::Parameter),
            Token::new_char(' ', CatCode::Ignored),
            Token::new_char('{', CatCode::BeginGroup),
            Token::new_char(' ', CatCode::Ignored),
            Token::new_char('[', CatCode::Other),
            Token::new_char('{', CatCode::BeginGroup),
            Token::new_char('#', CatCode::Parameter),
            Token::new_char('1', CatCode::Other),
            Token::new_char('}', CatCode::EndGroup),
            Token::new_char(']', CatCode::Other),
            Token::new_char(' ', CatCode::Ignored),
            Token::new_char('}', CatCode::EndGroup),
            Token::new_char(' ', CatCode::Space), //TODO: this should be `CatCode::Ignored`
            Token::new_cs("ExplSyntaxOff"),
            Token::new_char(' ', CatCode::Space),
            Token::new_cs("foo"),
            Token::new_char(':', CatCode::Other),
            Token::new_char('n', CatCode::Letter),
            Token::new_char('\r', CatCode::EndLine),
            Token::new_cs("foo"),
            Token::new_char(':', CatCode::Other),
            Token::new_char('n', CatCode::Letter),
            Token::new_char(' ', CatCode::Space),
            Token::new_cs("aaa"),
            Token::new_char('_', CatCode::Subscript),
            Token::new_char('b', CatCode::Letter),
            Token::new_char('\r', CatCode::EndLine),
        ];
        let unequal = std::iter::zip(&stl.tl, result)
            .enumerate()
            .filter_map(|(i, (l, r))| (l != r).then_some(i))
            .next();
        if let Some(idx) = unequal {
            println!(
                "left = {}, right = {}, at {}",
                stl.tl[idx].to_str_repr(),
                result[idx].to_str_repr(),
                idx
            );
        }
        assert_eq!(&stl.tl, result);
    }

    #[test]
    fn high() {
        let mut s = String::new();
        File::open(r"benches/list.tex")
            .unwrap()
            .read_to_string(&mut s)
            .unwrap();
        let lexer = LexerType::default();
        let ctabset = CTabSet::new_empty();
        let mut cat = CatCodeStack::new_with(CTab::document());
        let stl =
            SourcedTokenList::parse(s.into(), &mut cat, (&lexer, &ctabset));

        let high_config = HighConfig::default();
        let fmt = SourcedFormatter::new(&high_config, stl.into());
        assert_eq!(fmt.format_now(&mut Null), Ok(()));
    }

    #[test]
    fn high_mixed() {
        let s = r###"
\def\foo@#1{[#1]} \foo@{FOO} \@kernel
\makeatletter
\def\foo@:#1{[#1]} \foo@:#1{FOO} \@kernel \scan_stop:
\ExplSyntaxOn
\cs_set:Npn \foo@: #1 { [#1] }
\foo@: {FOO} \@kernel \scan_stop:
\ExplSyntaxOff
\@kernel \scan_stop:
\makeatother
\@kernel \scan_stop:
        "###
        .to_string();

        let ctabset_str = r"[atletter] `@ = 11 [explon] 32  = 9 `\_ = 11 `\: = 11 `\~ = 10 endlinechar=32";
        let ctabset = CTabSet::from_str(ctabset_str).unwrap();

        let mut lexer = LexerType::default();
        lexer.0.push((
            Category::RegTEx(RegTEx::new(r"\c{makeatletter}$").unwrap()),
            Category::RegTEx(RegTEx::new(r"\c{makeatother}$").unwrap()),
            LexerAction::CatCode(LexerCatCodeKind::CTab(String::from(
                "atletter",
            ))),
        ));
        lexer.0.push((
            Category::RegTEx(RegTEx::new(r"\c{ExplSyntaxOn}$").unwrap()),
            Category::RegTEx(RegTEx::new(r"\c{ExplSyntaxOff}$").unwrap()),
            LexerAction::CatCode(LexerCatCodeKind::CTab(String::from(
                "explon",
            ))),
        ));
        let mut cat = CatCodeStack::new_with(CTab::document());
        let _stl =
            SourcedTokenList::parse(s.into(), &mut cat, (&lexer, &ctabset));
        // println!("{}", stl.to_str_repr());

        let s = r"abc def  #1{\space}".to_string();
        let mut lexer = LexerType::default();
        lexer.0.clear();
        lexer.0.push((
            Category::Span([1, 4]),
            Category::Span([1, 8]),
            LexerAction::CatCode(LexerCatCodeKind::Char(vec![(
                ' ',
                CatCode::Escape,
            )])),
        ));
        let mut cat = CatCodeStack::new_with(CTab::document());
        let stl =
            SourcedTokenList::parse(s.into(), &mut cat, (&lexer, &ctabset));
        assert_eq!(
            stl.tokenlist(),
            &[
                Token::new_char('a', CatCode::Letter),
                Token::new_char('b', CatCode::Letter),
                Token::new_char('c', CatCode::Letter),
                Token::new_cs("def"),
                Token::new_char(' ', CatCode::Space),
                Token::new_char(' ', CatCode::Space),
                Token::new_char('#', CatCode::Parameter),
                Token::new_char('1', CatCode::Other),
                Token::new_char('{', CatCode::BeginGroup),
                Token::new_cs("space"),
                Token::new_char('}', CatCode::EndGroup),
            ]
        );
    }

    #[test]
    #[ignore]
    fn high_expl3() {
        let time = std::time::Instant::now();
        let mut s = String::new();
        File::open(r"benches/expl3-code.tex")
            .unwrap()
            .read_to_string(&mut s)
            .unwrap();
        let lexer = LexerType::default();
        let ctabset = CTabSet::new_empty();
        let mut cat = CatCodeStack::new();
        cat.push(CTab::document());
        let stl =
            SourcedTokenList::parse(s.into(), &mut cat, (&lexer, &ctabset));
        let high_config = HighConfig::default();
        let fmt = SourcedFormatter::new(&high_config, stl.into());

        let mut f = std::io::BufWriter::with_capacity(
            64_000,
            std::fs::File::create("./expl3-code.texhigh").unwrap(),
        );
        fmt.format_now(&mut f).unwrap();
        println!("Takes: {}s", time.elapsed().as_secs_f32());
    }
}
