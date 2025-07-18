use std::cell::{Cell, RefCell};
use std::slice::Iter;
use std::sync::Arc;

use compact_str::{format_compact, CompactString, ToCompactString};
use log::warn;
use rayon::prelude::*;

use crate::config::{Category, CategorySpan, HighConfig, LexerType};
use crate::high::{HWrite, HighFormat};
use crate::tex::circumflex_mechanism;
use crate::tex::{
    escape_string, escape_string_filter, escape_string_small, get_cs_type_re,
    primitive_engine, CatCode, LaTeXType,
};
use crate::types::{
    CTabSet, CatCodeGetter, CatCodeStack, Character, ControlSequence,
    ErrorKind, Position, Token, TokenListBytes, TokenListBytesRef,
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
        let s = &self.source_indices()[span];
        if s.is_empty() {
            return None;
        }
        if s.last() == self.source_indices.last() {
            if s.len() == 1 {
                return None;
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
        let s = &self.bytes_indices()[span];
        if s.is_empty() {
            return None;
        }
        if s.last() == self.bytes_indices.last() {
            if s.len() == 1 {
                return None;
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
    #[allow(dead_code)]
    range: Cell<usize>, //TODO: detect ranges, i.e. arguments of macros
}
impl<'a> SourcedFormatter<'a> {
    pub fn format_now<T: HWrite>(
        &self,
        stream: &mut T,
    ) -> Result<(), ErrorKind> {
        let mut next_token: Option<&Token> = None;
        let mut tokenlist_iter = self.tokenlist.tl.iter();
        let mut fmt_s = String::new();
        loop {
            let token = match next_token {
                Some(token) => {
                    next_token = None;
                    token
                }
                None => match tokenlist_iter.next() {
                    Some(token) => {
                        self.index.set(self.index.get() + 1);
                        token
                    }
                    None => break,
                },
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
                    match tokenlist_iter.next() {
                        Some(nt) => match nt {
                            Token::Char(nc)
                                if nc.catcode == CatCode::Parameter
                                    || (nc.catcode == CatCode::Other
                                        && nc.charcode.is_ascii_digit()) =>
                            {
                                self.fmt_raw(
                                    stream,
                                    format_args!("\\THrs{{{}}}", "parameter"),
                                )?;
                                self.fmt_chr(stream, chr)?;
                                self.index.set(self.index.get() + 1);
                                self.fmt_chr(stream, nc)?;
                                self.fmt_raw(
                                    stream,
                                    format_args!("\\THre{{{}}}", "parameter"),
                                )?;
                            }
                            _ => {
                                self.fmt_chr(stream, chr)?;
                                self.index.set(self.index.get() + 1);
                                next_token = Some(nt);
                            }
                        },
                        None => {
                            self.fmt_chr(stream, chr)?;
                            break;
                        }
                    }
                }
                _ => {
                    self.fmt_token(stream, token)?;
                }
            }
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
            range: Cell::new(0),
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
        while let Some(token) = tokens.next() {
            self.index.set(self.index.get() + 1);
            match token {
                Token::Char(c)
                    if matches!(
                        c.catcode,
                        CatCode::Letter | CatCode::Other
                    ) && !self.is_newline(c) =>
                {
                    self.fmt_chr_try_not(stream, c)?
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
        let mut next_char = None;
        let mut fmt_s = String::new();
        self.fmt_chr(stream, chr)?;
        loop {
            let token = if let Some(nt) = next_char {
                next_char = None;
                nt
            } else {
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
                    return Ok(Some(token));
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
                        return Ok(Some(token));
                    } else {
                        self.fmt_any(stream, *any)?;
                    }
                }
            }
        }
        Ok(None)
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
        let mut group_level = 0isize;
        let curr_index = self.index.get();
        loop {
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
            self.index.set(curr_index);
        }
        Ok(())
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
                .par_iter()
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
            Some(cs_cat) => CompactString::new(cs_cat),
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
            CompactString::new(category)
        } else if matches!(chr.catcode, CatCode::BeginGroup) {
            self.group_level.set(self.group_level.get() + 1);
            format_compact!("group.{}", self.group_level.get())
        } else if matches!(chr.catcode, CatCode::EndGroup) {
            let level = self.group_level.get();
            self.group_level.set(level - 1);
            format_compact!("group.{}", level)
        } else {
            CompactString::const_new("?")
        }
    }
    fn is_newline(&self, _: &Character) -> bool {
        let source = self.source_of_current();
        // should take catcode into consideration?
        matches!(source, "\r" | "\n" | "\r\n" | "\n\r")
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
                    .par_iter()
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
            .par_iter()
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
