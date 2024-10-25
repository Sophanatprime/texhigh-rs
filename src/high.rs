use std::{
    fmt::Arguments,
    fs::File,
    io::{self, BufWriter, Write},
};
use unicode_properties::{GeneralCategoryGroup as UG, UnicodeGeneralCategory};

use crate::config::HighConfig;
use crate::tex::{get_cs_type, primitive_engine, LaTeXType};
use crate::types::*;

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
        <BufWriter<io::Stdout> as Write>::write_fmt(self, a).map_err(|e| e.into())
    }
}
impl HWrite for io::BufWriter<io::StdoutLock<'_>> {
    fn write_str(&mut self, s: &str) -> Result<(), ErrorKind> {
        self.write_all(s.as_bytes()).map_err(|e| e.into())
    }
    fn write_fmt(&mut self, a: Arguments) -> Result<(), ErrorKind> {
        <BufWriter<io::StdoutLock> as Write>::write_fmt(self, a).map_err(|e| e.into())
    }
}

/// Indentifier ::=
/// `\THnl` : new line;
/// `\THin{<len>}` : indent;
/// `\THbp{<category>}` : break point;
/// `\THcs{<category>}{<escape char>}{<csname>}` : control sequence;
/// `\THch{<category>}{\<chr>}` : character;
/// `\THrs{<category>}` : range start;
/// `\THre{<category>}` : range end;
/// `\THst{<category>}{<string>}` : string (Letters and Others)
/// `\THes{<category>}` : escaped start;
/// `\THee{<category>}` : escaped end;
/// `\THpn{<category>}{<punctuation>}` : punctuation;
pub trait HighFormat {
    /// Indent category. argument 2: nest level
    fn get_indent_len(&self, nest: usize) -> usize {
        2 * nest
    }
    fn get_break_category(&self, _: &Token) -> &str {
        "?"
    }
    fn get_cs_catogery(&self, cs: &ControlSequence) -> String {
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
            .to_string()
        } else {
            format!("primitive.{}", engine)
        }
    }
    fn get_chr_catogery(&self, _: &Character) -> &str {
        "?"
    }
    fn get_string_category(&self, _: &str) -> &str {
        "?"
    }
    fn get_punct_category(&self, token: &Token) -> Option<&str> {
        match token {
            Token::Char(chr) if chr.charcode.general_category_group() == UG::Punctuation => {
                Some("?")
            }
            _ => None,
        }
    }

    /// Format raw content.
    /// Using: `self.fmt_raw(stream, format_args!(...))`
    #[inline]
    fn fmt_raw<T: HWrite>(&self, stream: &mut T, args: Arguments) -> Result<(), ErrorKind> {
        write!(stream, "{}", args)
    }
    fn fmt_newline<T: HWrite>(&self, stream: &mut T) -> Result<(), ErrorKind> {
        self.fmt_raw(stream, format_args!("\\THnl\n"))
    }
    fn fmt_indent<T: HWrite>(&self, stream: &mut T, nest: usize) -> Result<(), ErrorKind> {
        self.fmt_raw(
            stream,
            format_args!("\\THin{{{}}}", self.get_indent_len(nest)),
        )
    }
    fn fmt_break<T: HWrite>(&self, stream: &mut T, category: &str) -> Result<(), ErrorKind> {
        self.fmt_raw(stream, format_args!("\\THbp{{{}}}", category))
    }
    fn fmt_cs<T: HWrite>(&self, stream: &mut T, cs: &ControlSequence) -> Result<(), ErrorKind> {
        // let escape_char = match cs.escape_char {
        //     Some(c) => if c.is_control() {
        //         escape_string(&escape_control(c, b'^'), b'^')
        //     } else {
        //         format!("{}", c)
        //     },
        //     _ => "".to_string(),
        // };
        self.fmt_raw(
            stream,
            format_args!(
                "\\THcs{{{}}}{{{}}}{{{}}}",
                self.get_cs_catogery(cs),
                escape_string(
                    &cs.escape_char.map_or("".to_string(), |c| format!("{}", c)),
                    b'^'
                ),
                &cs.get_csname_escaped(b'^')
            ),
        )
    }
    fn fmt_chr<T: HWrite>(&self, stream: &mut T, chr: &Character) -> Result<(), ErrorKind> {
        if chr.catcode == CatCode::EndLine {
            self.fmt_newline(stream)
        } else {
            self.fmt_raw(
                stream,
                format_args!(
                    "\\THch{{{}}}{{{}}}",
                    self.get_chr_catogery(chr),
                    escape_string(&chr.escape_control(b'^'), b'^')
                ),
            )
        }
    }
    fn fmt_punct<T: HWrite>(&self, stream: &mut T, token: &Token) -> Result<(), ErrorKind> {
        self.fmt_raw(
            stream,
            format_args!(
                "\\THpn{{{}}}{{\\{}}}",
                self.get_punct_category(token).unwrap(),
                match token {
                    Token::Char(chr) => chr.escape_control(b'^'),
                    Token::CS(cs) => cs.get_csname().to_string(),
                    Token::Any(_) => unreachable!(),
                }
            ),
        )
    }
    fn fmt_string<T: HWrite>(&self, stream: &mut T, s: &str) -> Result<(), ErrorKind> {
        self.fmt_raw(
            stream,
            format_args!("\\THst{{{}}}{{{}}}", self.get_string_category(s), s),
        )
    }
    fn fmt_any<T: HWrite>(&self, stream: &mut T, any: u32) -> Result<(), ErrorKind> {
        if any == '\r' as u32 || any == '\r' as u32 {
            self.fmt_newline(stream)
        } else {
            unreachable!("Illegal Any Token.")
        }
    }
    fn fmt_token<T: HWrite>(&self, stream: &mut T, token: &Token) -> Result<(), ErrorKind> {
        match token {
            Token::Char(chr) => self.fmt_chr(stream, chr),
            Token::CS(cs) => self.fmt_cs(stream, cs),
            Token::Any(any) => self.fmt_any(stream, *any),
        }
    }
    fn fmt_tokenlist<T: HWrite>(
        &self,
        stream: &mut T,
        tokenlist: &TokenList,
    ) -> Result<(), ErrorKind> {
        let mut st = String::new();
        for token in tokenlist.iter() {
            match token {
                Token::Char(chr) if matches!(chr.catcode, CatCode::Letter | CatCode::Other) => {
                    if self.get_punct_category(token).is_some() {
                        self.fmt_punct(&mut st, token)?;
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

pub struct DefaultFormater {}
impl DefaultFormater {
    #[rustfmt::skip]
    pub fn new() -> Self { Self {} }
}
impl HighFormat for DefaultFormater {}

pub struct StandardFormatter<'a> {
    high_config: &'a HighConfig,
    tokenlist: TokenList,
}
impl HighFormat for StandardFormatter<'_> {
    fn get_indent_len(&self, nest: usize) -> usize {
        nest * self.high_config.break_indent as usize
    }
    fn get_cs_catogery(&self, cs: &ControlSequence) -> String {
        let csname = cs.get_csname();
        match self.high_config.cs_categories.categories(csname) {
            Some(cs_cat) => cs_cat.to_string(),
            None => {
                let engine = primitive_engine(csname);
                if engine.is_empty() {
                    match get_cs_type(csname) {
                        LaTeXType::L3Primitive => "latex3.primitive",
                        LaTeXType::L3FunctionInternal => "latex3.function.internal",
                        LaTeXType::L3FunctionPublic => "latex3.function.public",
                        LaTeXType::L3FunctionKernel => "latex3.function.kernel",
                        LaTeXType::L3VariableInternal => "latex3.variable.internal",
                        LaTeXType::L3VariablePublic => "latex3.variable.public",
                        LaTeXType::L3VariableKernel => "latex3.variable.kernel",
                        LaTeXType::DocumentSmallCarmel if csname.len() > 1 => "latex.programming",
                        LaTeXType::DocumentPascal => "latex.programming",
                        LaTeXType::L2eInternal => "latex.internal",
                        LaTeXType::L2eKernel => "latex.internal",
                        LaTeXType::Punctuation => "?",
                        LaTeXType::Other | _ => "?",
                    }
                    .to_string()
                } else {
                    format!("primitive.{}", engine)
                }
            }
        }
    }
    fn get_chr_catogery(&self, chr: &Character) -> &str {
        if matches!(chr.catcode, CatCode::BeginGroup | CatCode::EndGroup) {
            "group"
        } else {
            "?"
        }
    }
    fn fmt_chr<T: HWrite>(&self, stream: &mut T, chr: &Character) -> Result<(), ErrorKind> {
        if chr.catcode == CatCode::EndLine {
            self.fmt_newline(stream)
        } else {
            if self.high_config.break_at.contains(&chr.charcode) {
                self.fmt_break(stream, "?")?;
            }
            self.fmt_raw(
                stream,
                format_args!(
                    "\\THch{{{}}}{{{}}}",
                    self.get_chr_catogery(chr),
                    escape_string(&chr.escape_control(b'^'), b'^')
                ),
            )
        }
    }
    fn fmt_cs<T: HWrite>(&self, stream: &mut T, cs: &ControlSequence) -> Result<(), ErrorKind> {
        self.fmt_break(stream, "?")?;
        self.fmt_raw(
            stream,
            format_args!(
                "\\THcs{{{}}}{{{}}}{{{}}}",
                self.get_cs_catogery(cs),
                escape_string(
                    &cs.escape_char.map_or("".to_string(), |c| format!("{}", c)),
                    b'^'
                ),
                &cs.get_csname_escaped(b'^')
            ),
        )
    }
    #[rustfmt::skip]
    fn fmt_tokenlist<T: HWrite>(
        &self,
        stream: &mut T,
        tokenlist: &TokenList,
    ) -> Result<(), ErrorKind> {
        let mut next_char: Option<&Token> = None;
        let mut tokenlist_iter = tokenlist.iter();
        loop {
            let token = if next_char.is_some() {
                (next_char.unwrap(), { next_char = None; }).0
            } else {
                match tokenlist_iter.next() {
                    Some(t) => t,
                    None => break,
                }
            };
            match token {
                Token::Char(chr) if matches!(chr.catcode, CatCode::Letter | CatCode::Other) => {
                    let mut s = String::new();
                    next_char = StandardFormatter::write_string(self, &mut tokenlist_iter, &mut s, chr)?;
                    self.fmt_string(stream, &s)?;
                }
                Token::Char(chr) if chr.catcode == CatCode::Comment => {
                    let mut s = String::new();
                    let last_token = StandardFormatter::write_comment(self, &mut tokenlist_iter, &mut s, chr)?;
                    self.fmt_raw(stream, format_args!("\\THrs{{{0}}}{1}\\THre{{{0}}}", "comment", &s))?;
                    if last_token.is_some() {
                        self.fmt_token(stream, last_token.unwrap())?;
                    }
                }
                Token::Char(chr) if chr.catcode == CatCode::MathShift => {
                    let mut s = String::new();
                    let mut is_succ = true;
                    StandardFormatter::write_inline_math(self, &mut tokenlist_iter, &mut s, chr, &mut is_succ)?;
                    if is_succ {
                        self.fmt_raw( stream, format_args!("\\THrs{{{0}}}{1}\\THre{{{0}}}", "math.inline", &s))?;
                    } else {
                        self.fmt_token(stream, token)?;
                    }
                }
                Token::Char(chr) if chr.catcode == CatCode::Parameter => {
                    match tokenlist_iter.next() {
                        Some(nt) => match nt {
                            Token::Char(nc) if nc.catcode == CatCode::Parameter || (
                                nc.catcode == CatCode::Other && nc.charcode.is_ascii_digit()) => {
                                    self.fmt_raw(stream, format_args!("\\THrs{{{}}}", "parameter"))?;
                                    self.fmt_chr(stream, chr)?;
                                    self.fmt_chr(stream, nc)?;
                                    self.fmt_raw(stream, format_args!("\\THre{{{}}}", "parameter"))?;
                            }
                            _ => {
                                self.fmt_token(stream, token)?;
                                next_char = Some(nt);
                            }
                        }
                        None => break,
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
    pub fn new<'a: 'b>(high_config: &'a HighConfig, tokenlist: TokenList) -> Self {
        Self {
            high_config,
            tokenlist,
        }
    }
    pub fn format_now<T: HWrite>(&self, stream: &mut T) -> Result<(), ErrorKind> {
        self.fmt_tokenlist(stream, &self.tokenlist)
    }
    fn write_string<'t, T: HWrite>(
        &self,
        tokens: &mut Iter<'t, Token>,
        stream: &mut T,
        chr: &Character,
    ) -> Result<Option<&'t Token>, ErrorKind> {
        if self.get_punct_category(&Token::Char(*chr)).is_some() {
            self.fmt_punct(stream, &Token::Char(*chr))?;
        } else {
            self.fmt_raw(stream, format_args!("{}", chr.charcode))?;
        }
        while let Some(token) = tokens.next() {
            match token {
                Token::Char(c) if c.catcode == CatCode::Letter || c.catcode == CatCode::Other => {
                    if self.get_punct_category(token).is_some() {
                        self.fmt_punct(stream, token)?;
                    } else {
                        self.fmt_raw(stream, format_args!("{}", c.charcode))?;
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
        self.fmt_chr(stream, chr)?;
        loop {
            let token = if next_char.is_some() {
                (next_char.unwrap(), {
                    next_char = None;
                })
                    .0
            } else {
                match tokens.next() {
                    Some(t) => t,
                    None => break,
                }
            };
            match token {
                Token::Char(chr) if matches!(chr.catcode, CatCode::Letter | CatCode::Other) => {
                    let mut s = String::new();
                    next_char = StandardFormatter::write_string(self, tokens, &mut s, chr)?;
                    self.fmt_string(stream, &s)?;
                }
                Token::Char(c) => {
                    if c.catcode == CatCode::EndLine {
                        return Ok(Some(token));
                    } else {
                        self.fmt_chr(stream, c)?;
                    }
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
        chars: &mut Iter<Token>,
        stream: &mut T,
        chr: &Character,
        is_succ: &mut bool,
    ) -> Result<(), ErrorKind> {
        self.fmt_chr(stream, chr)?;
        let mut new_chars = chars.clone();
        let mut group_level = 0isize;
        while let Some(token) = new_chars.next() {
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
            *chars = new_chars;
        }
        Ok(())
    }
}
