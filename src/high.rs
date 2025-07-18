use compact_str::{format_compact, CompactString, ToCompactString};
use std::{
    fmt::Arguments,
    fs::File,
    io::{self, BufWriter, Write},
    sync::Arc,
};

use crate::config::HighConfig;
use crate::tex::{escape_string, escape_string_filter, escape_string_small};
use crate::types::*;
use crate::{get_cs_type, primitive_engine, LaTeXType};

pub enum HighItemType<'a> {
    CS(&'a ControlSequence),
    Punct(char),
    Escaped(char),
}

pub trait HWrite {
    fn write_str(&mut self, s: &str) -> Result<(), ErrorKind>;
    fn write_fmt(&mut self, a: Arguments) -> Result<(), ErrorKind>;
}
impl<W: HWrite + ?Sized> HWrite for &mut W {
    fn write_str(&mut self, s: &str) -> Result<(), ErrorKind> {
        (*self).write_str(s)
    }
    fn write_fmt(&mut self, a: Arguments) -> Result<(), ErrorKind> {
        (*self).write_fmt(a)
    }
}
impl HWrite for File {
    fn write_str(&mut self, s: &str) -> Result<(), ErrorKind> {
        Write::write_all(self, s.as_bytes()).map_err(|e| e.into())
    }
    fn write_fmt(&mut self, a: Arguments) -> Result<(), ErrorKind> {
        Write::write_fmt(self, a).map_err(|e| e.into())
    }
}
impl HWrite for String {
    fn write_str(&mut self, s: &str) -> Result<(), ErrorKind> {
        self.push_str(s);
        Ok(())
    }
    fn write_fmt(&mut self, a: Arguments) -> Result<(), ErrorKind> {
        self.push_str(&format!("{}", a));
        Ok(())
    }
}
impl HWrite for CompactString {
    fn write_str(&mut self, s: &str) -> Result<(), ErrorKind> {
        self.push_str(s);
        Ok(())
    }
    fn write_fmt(&mut self, a: Arguments) -> Result<(), ErrorKind> {
        self.push_str(&format!("{}", a));
        Ok(())
    }
}
impl HWrite for io::Stdout {
    fn write_str(&mut self, s: &str) -> Result<(), ErrorKind> {
        Write::write_all(self, s.as_bytes()).map_err(|e| e.into())
    }
    fn write_fmt(&mut self, a: Arguments) -> Result<(), ErrorKind> {
        Write::write_fmt(self, a).map_err(|e| e.into())
    }
}
impl HWrite for io::StdoutLock<'_> {
    fn write_str(&mut self, s: &str) -> Result<(), ErrorKind> {
        Write::write_all(self, s.as_bytes()).map_err(|e| e.into())
    }
    fn write_fmt(&mut self, a: Arguments) -> Result<(), ErrorKind> {
        Write::write_fmt(self, a).map_err(|e| e.into())
    }
}
impl HWrite for io::BufWriter<File> {
    fn write_str(&mut self, s: &str) -> Result<(), ErrorKind> {
        self.write_all(s.as_bytes()).map_err(|e| e.into())
    }
    fn write_fmt(&mut self, a: Arguments) -> Result<(), ErrorKind> {
        <BufWriter<File> as Write>::write_fmt(self, a).map_err(|e| e.into())
    }
}
impl HWrite for io::BufWriter<io::Stdout> {
    fn write_str(&mut self, s: &str) -> Result<(), ErrorKind> {
        self.write_all(s.as_bytes()).map_err(|e| e.into())
    }
    fn write_fmt(&mut self, a: Arguments) -> Result<(), ErrorKind> {
        <BufWriter<io::Stdout> as Write>::write_fmt(self, a)
            .map_err(|e| e.into())
    }
}
impl HWrite for io::BufWriter<io::StdoutLock<'_>> {
    fn write_str(&mut self, s: &str) -> Result<(), ErrorKind> {
        self.write_all(s.as_bytes()).map_err(|e| e.into())
    }
    fn write_fmt(&mut self, a: Arguments) -> Result<(), ErrorKind> {
        <BufWriter<io::StdoutLock> as Write>::write_fmt(self, a)
            .map_err(|e| e.into())
    }
}
pub struct Null;
impl HWrite for Null {
    #[inline]
    fn write_fmt(&mut self, _: Arguments) -> Result<(), ErrorKind> {
        Ok(())
    }
    #[inline]
    fn write_str(&mut self, _: &str) -> Result<(), ErrorKind> {
        Ok(())
    }
}

/// Indentifier ::=
/// `\THnl` : new line;
/// `\THin{<len>}` : indent;
/// `\THbp{<category>}` : break point;
/// `\THcs{<category>}{<escape char>}{<csname>}` : control sequence;
/// `\THch{<category>}{<chr>}` : character;
/// `\THrs{<category>}` : range start;
/// `\THre{<category>}` : range end;
/// `\THst{<category>}{<string>}` : string (Letters and Others)
/// `\THes{<category>}` : escaped start;
/// `\THee{<category>}` : escaped end;
/// `\THpn{<category>}{<punctuation>}` : punctuation;
/// `\THcr{<unicode code point number>}`: character needed to be replaced;
///
/// All of characters are either literal or in `\THcr` or escaped by its hex form:
/// i.e. `alpha"20 beta"2E ` is supposed to be `alpha<U+20>beta<U+2E>`.
/// Unicode letters never be escaped.
pub trait HighFormat {
    /// Indent category. argument 2: nest level
    fn get_indent_len(&self, nest: usize) -> usize {
        2 * nest
    }
    fn get_break_category(&self, _: &Token) -> CompactString {
        CompactString::const_new("?")
    }
    fn get_cs_catogery(&self, cs: &ControlSequence) -> CompactString {
        let engine = primitive_engine(cs.get_csname());
        if engine.is_empty() {
            match get_cs_type(cs.get_csname()) {
                LaTeXType::L3Primitive => "latex3.primitive",
                LaTeXType::L3FunctionInternal => "latex3.function.internal",
                LaTeXType::L3FunctionPublic => "latex3.function.public",
                LaTeXType::L3FunctionKernel => "latex3.function.kernel",
                LaTeXType::L3VariableInternal => "latex3.variable.internal",
                LaTeXType::L3VariablePublic => "latex3.variable.public",
                LaTeXType::L3VariableKernel => "latex3.variable.kernel",
                LaTeXType::DocumentSmallCarmel => "latex.programming",
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
    fn get_chr_catogery(&self, _: &Character) -> CompactString {
        CompactString::const_new("?")
    }
    fn get_string_category(&self, _: &str) -> CompactString {
        CompactString::const_new("?")
    }
    fn get_punct_category(&self, chr: &Character) -> Option<CompactString> {
        if chr.is_punct() {
            Some(CompactString::const_new("?"))
        } else {
            None
        }
    }
    fn get_char_replacement(&self, chr: char) -> CompactString {
        format_compact!("\\THcr{{{}}}", chr as u32)
    }

    fn is_newline(&self, chr: &Character) -> bool {
        (chr.charcode == '\n'
            && matches!(chr.catcode, CatCode::Letter | CatCode::Other))
            || chr.catcode == CatCode::EndLine
    }

    /// Format raw content.
    /// Using: `self.fmt_raw(stream, format_args!(...))`
    #[inline]
    fn fmt_raw<T: HWrite>(
        &self,
        stream: &mut T,
        args: Arguments,
    ) -> Result<(), ErrorKind> {
        write!(stream, "{}", args)
    }
    fn fmt_newline<T: HWrite>(&self, stream: &mut T) -> Result<(), ErrorKind> {
        self.fmt_raw(stream, format_args!("\\THnl\n"))
    }
    fn fmt_indent<T: HWrite>(
        &self,
        stream: &mut T,
        nest: usize,
    ) -> Result<(), ErrorKind> {
        self.fmt_raw(
            stream,
            format_args!("\\THin{{{}}}", self.get_indent_len(nest)),
        )
    }
    fn fmt_break<T: HWrite>(
        &self,
        stream: &mut T,
        category: &str,
    ) -> Result<(), ErrorKind> {
        self.fmt_raw(stream, format_args!("\\THbp{{{}}}", category))
    }
    fn fmt_cs<T: HWrite>(
        &self,
        stream: &mut T,
        cs: &ControlSequence,
    ) -> Result<(), ErrorKind> {
        self.fmt_raw(
            stream,
            format_args!(
                "\\THcs{{{}}}{{{}}}{{{}}}",
                self.get_cs_catogery(cs).as_str(),
                escape_string(
                    &cs.escape_char
                        .map_or("".to_compact_string(), |c| format_compact!(
                            "{}", c
                        )),
                    b'^'
                ),
                &cs.get_csname_escaped(b'^').as_str()
            ),
        )
    }
    fn fmt_chr<T: HWrite>(
        &self,
        stream: &mut T,
        chr: &Character,
    ) -> Result<(), ErrorKind> {
        if self.is_newline(chr) {
            self.fmt_newline(stream)
        } else {
            self.fmt_raw(
                stream,
                format_args!(
                    "\\THch{{{}}}{{{}}}",
                    self.get_chr_catogery(chr).as_str(),
                    escape_string_small(chr.escape_control(b'^'), b'^')
                ),
            )
        }
    }
    fn fmt_punct<T: HWrite>(
        &self,
        stream: &mut T,
        chr: &Character,
    ) -> Result<(), ErrorKind> {
        let mut s = [0; 4];
        self.fmt_raw(
            stream,
            format_args!(
                "\\THpn{{{}}}{{{}}}",
                self.get_punct_category(chr).unwrap().as_str(),
                escape_string(chr.charcode.encode_utf8(&mut s), b'^'),
            ),
        )
    }
    fn fmt_string<T: HWrite>(
        &self,
        stream: &mut T,
        s: &str,
    ) -> Result<(), ErrorKind> {
        self.fmt_raw(
            stream,
            format_args!(
                "\\THst{{{}}}{{{}}}",
                self.get_string_category(s).as_str(),
                s
            ),
        )
    }
    fn fmt_any<T: HWrite>(
        &self,
        stream: &mut T,
        any: u32,
    ) -> Result<(), ErrorKind> {
        if any == '\r' as u32 || any == '\n' as u32 {
            self.fmt_newline(stream)
        } else if any == 0 {
            Ok(())
        } else {
            unreachable!("Illegal Any Token. ID={:08x}", any)
        }
    }
    fn fmt_token<T: HWrite>(
        &self,
        stream: &mut T,
        token: &Token,
    ) -> Result<(), ErrorKind> {
        match token {
            Token::Char(chr) => self.fmt_chr(stream, chr),
            Token::CS(cs) => self.fmt_cs(stream, cs),
            Token::Any(any) => self.fmt_any(stream, *any),
        }
    }
    fn fmt_tokenlist<T: HWrite>(
        &self,
        stream: &mut T,
        tokenlist: &[Token],
    ) -> Result<(), ErrorKind> {
        let mut st = String::new();
        for token in tokenlist.iter() {
            match token {
                Token::Char(chr)
                    if matches!(
                        chr.catcode,
                        CatCode::Letter | CatCode::Other
                    ) && !self.is_newline(chr) =>
                {
                    if self.get_punct_category(chr).is_some() {
                        self.fmt_punct(&mut st, chr)?;
                    } else {
                        st.push(chr.charcode);
                    }
                }
                _ => {
                    if !st.is_empty() {
                        self.fmt_string(stream, &st)?;
                        st.clear();
                    }
                    self.fmt_token(stream, token)?;
                }
            }
        }
        if !st.is_empty() {
            self.fmt_string(stream, &st)?;
        }
        Ok(())
    }
    fn fmt_tokenlist_with_raw<'a, T: HWrite, I>(
        &self,
        _stream: &mut T,
        _t_r: I,
    ) -> Result<(), ErrorKind>
    where
        I: Iterator<Item = (&'a Token, &'a str)>,
    {
        Ok(())
    }
}

pub struct PlainFormatter;
impl HighFormat for PlainFormatter {
    fn fmt_tokenlist<T: HWrite>(
        &self,
        stream: &mut T,
        tokenlist: &[Token],
    ) -> Result<(), ErrorKind> {
        let mut index = 0;
        while index < tokenlist.len() {
            let curr_token = unsafe { tokenlist.get_unchecked(index) };
            let mut token_str = match curr_token {
                Token::CS(cs) => format_compact!(
                    "{}{}",
                    cs.escape_char.unwrap_or('\\'),
                    cs.get_csname_escaped(b'^')
                ),
                Token::Char(c) => c.escape_control(b'^'),
                Token::Any(_) => CompactString::const_new(""),
            };
            index += 1;
            if matches!(curr_token, Token::CS(_)) {
                if let Some(Token::Char(c)) = tokenlist.get(index) {
                    if c.catcode == CatCode::Letter {
                        token_str.push_str(" ");
                    }
                }
            }
            self.fmt_raw(stream, format_args!("{}", token_str))?;
        }
        Ok(())
    }
}

pub struct DefaultFormater<'a> {
    tokenlist: &'a [Token],
}
impl<'a> DefaultFormater<'a> {
    pub fn new(tokenlist: &'a [Token]) -> Self {
        Self { tokenlist }
    }
    pub fn format_now<T: HWrite>(
        &self,
        stream: &mut T,
    ) -> Result<(), ErrorKind> {
        self.fmt_tokenlist(stream, self.tokenlist)
    }
}
impl HighFormat for DefaultFormater<'_> {}

pub struct StandardFormatter<'a> {
    high_config: &'a HighConfig,
    tokenlist: Arc<TokenList>,
}
impl HighFormat for StandardFormatter<'_> {
    fn get_indent_len(&self, nest: usize) -> usize {
        nest * self.high_config.break_indent as usize
    }
    fn get_cs_catogery(&self, cs: &ControlSequence) -> CompactString {
        let csname = cs.get_csname();
        match self.high_config.cs_categories.categories(csname) {
            Some(cs_cat) => cs_cat.to_compact_string(),
            None => {
                let engine = primitive_engine(csname);
                if engine.is_empty() {
                    match get_cs_type(csname) {
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
        if matches!(chr.catcode, CatCode::BeginGroup | CatCode::EndGroup) {
            CompactString::const_new("group")
        } else {
            CompactString::const_new("?")
        }
    }
    fn fmt_punct<T: HWrite>(
        &self,
        stream: &mut T,
        chr: &Character,
    ) -> Result<(), ErrorKind> {
        let mut s = [0; 4];
        self.fmt_raw(
            stream,
            format_args!(
                "\\THpn{{{}}}{{{}}}",
                self.get_punct_category(chr).unwrap().as_str(),
                if self.high_config.char_replacements.contains(&chr.charcode) {
                    self.get_char_replacement(chr.charcode)
                } else {
                    escape_string(chr.charcode.encode_utf8(&mut s), b'^')
                },
            ),
        )?;
        if self.high_config.break_at.contains(chr.charcode) {
            self.fmt_break(stream, "?")?;
        }
        Ok(())
    }
    fn fmt_chr<T: HWrite>(
        &self,
        stream: &mut T,
        chr: &Character,
    ) -> Result<(), ErrorKind> {
        if self.is_newline(chr) {
            self.fmt_newline(stream)
        } else {
            self.fmt_raw(
                stream,
                format_args!(
                    "\\THch{{{}}}{{{}}}",
                    self.get_chr_catogery(chr).as_str(),
                    if self
                        .high_config
                        .char_replacements
                        .contains(&chr.charcode)
                    {
                        self.get_char_replacement(chr.charcode)
                    } else {
                        escape_string_small(&chr.escape_control(b'^'), b'^')
                    }
                ),
            )?;
            if self.high_config.break_at.contains(chr.charcode) {
                self.fmt_break(stream, "?")?;
            }
            Ok(())
        }
    }
    fn fmt_cs<T: HWrite>(
        &self,
        stream: &mut T,
        cs: &ControlSequence,
    ) -> Result<(), ErrorKind> {
        self.fmt_break(stream, "?")?;
        let escaped_escape_char = if let Some(chr) = cs.escape_char {
            if self.high_config.char_replacements.contains(&chr) {
                self.get_char_replacement(chr)
            } else {
                escape_string(chr.to_compact_string(), b'^')
            }
        } else {
            CompactString::new("")
        };
        let escaped_cs_name = escape_string_filter(
            cs.get_csname(),
            b'^',
            |v| self.high_config.char_replacements.contains(&v),
            |v| self.get_char_replacement(v),
        );
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
    fn fmt_tokenlist<T: HWrite>(
        &self,
        stream: &mut T,
        tokenlist: &[Token],
    ) -> Result<(), ErrorKind> {
        let mut next_token: Option<&Token> = None;
        let mut tokenlist_iter = tokenlist.iter();
        let mut fmt_s = String::new();
        loop {
            let token = if next_token.is_some() {
                let ret = next_token.unwrap();
                next_token = None;
                ret
            } else {
                match tokenlist_iter.next() {
                    Some(t) => t,
                    None => break,
                }
            };
            match token {
                Token::Char(chr)
                    if matches!(
                        chr.catcode,
                        CatCode::Letter | CatCode::Other
                    ) && !self.is_newline(chr) =>
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
                                self.fmt_chr(stream, nc)?;
                                self.fmt_raw(
                                    stream,
                                    format_args!("\\THre{{{}}}", "parameter"),
                                )?;
                            }
                            _ => {
                                self.fmt_chr(stream, chr)?;
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
}

use std::slice::Iter;
impl<'b> StandardFormatter<'b> {
    pub fn new<'a: 'b>(
        high_config: &'a HighConfig,
        tokenlist: Arc<TokenList>,
    ) -> Self {
        Self { high_config, tokenlist }
    }
    pub fn format_now<T: HWrite>(
        &self,
        stream: &mut T,
    ) -> Result<(), ErrorKind> {
        self.fmt_tokenlist(stream, &self.tokenlist.values)
    }
    fn write_string<'t, T: HWrite>(
        &self,
        tokens: &mut Iter<'t, Token>,
        stream: &mut T,
        chr: &Character,
    ) -> Result<Option<&'t Token>, ErrorKind> {
        if self.get_punct_category(chr).is_some() {
            self.fmt_punct(stream, chr)?;
        } else {
            self.fmt_raw(
                stream,
                format_args!(
                    "{}",
                    if self
                        .high_config
                        .char_replacements
                        .contains(&chr.charcode)
                    {
                        self.get_char_replacement(chr.charcode)
                    } else {
                        chr.charcode.to_compact_string()
                    }
                ),
            )?;
        }
        while let Some(token) = tokens.next() {
            match token {
                Token::Char(c)
                    if matches!(
                        c.catcode,
                        CatCode::Letter | CatCode::Other
                    ) && !self.is_newline(c) =>
                {
                    if self.get_punct_category(c).is_some() {
                        self.fmt_punct(stream, c)?;
                    } else {
                        self.fmt_raw(
                            stream,
                            format_args!(
                                "{}",
                                if self
                                    .high_config
                                    .char_replacements
                                    .contains(&c.charcode)
                                {
                                    self.get_char_replacement(c.charcode)
                                } else {
                                    c.charcode.to_compact_string()
                                }
                            ),
                        )?;
                    }
                }
                _ => return Ok(Some(token)),
            }
        }
        Ok(None)
    }
    fn write_comment<'a, T: HWrite>(
        &self,
        tokens: &'a mut Iter<Token>,
        stream: &mut T,
        chr: &Character,
    ) -> Result<Option<&'a Token>, ErrorKind> {
        let mut next_char = None;
        let mut fmt_s = String::new();
        self.fmt_chr(stream, chr)?;
        loop {
            let token = if let Some(nt) = next_char {
                next_char = None;
                nt
            } else {
                match tokens.next() {
                    Some(t) => t,
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
        loop {
            let Some(token) = new_chars.next() else {
                *is_succ = false;
                break;
            };
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
        }
        Ok(())
    }
}
