use std::{fmt::Arguments, fs::File, io};
use unicode_properties::{GeneralCategoryGroup as UG, UnicodeGeneralCategory};

use crate::tex::{get_cs_type, primitive_engine, LaTeXType};
use crate::types::*;
use crate::config::HighConfig;

pub enum HighItemType<'a> {
    CS(&'a ControlSequence),
    Punct(char),
    Escaped(char),
}

pub trait Write {
    fn write_str(&mut self, s: &str) -> Result<(), ErrorKind>;
    fn write_fmt(&mut self, a: Arguments) -> Result<(), ErrorKind>;
}
impl<W: Write + ?Sized> Write for &mut W {
    fn write_str(&mut self, s: &str) -> Result<(), ErrorKind> {
        (*self).write_str(s)
    }
    fn write_fmt(&mut self, a: Arguments) -> Result<(), ErrorKind> {
        (*self).write_fmt(a)
    }
}
impl Write for File {
    fn write_str(&mut self, s: &str) -> Result<(), ErrorKind> {
        io::Write::write_all(self, s.as_bytes()).map_err(|e| e.into())
    }
    fn write_fmt(&mut self, a: Arguments) -> Result<(), ErrorKind> {
        io::Write::write_fmt(self, a).map_err(|e| e.into())
    }
}
impl Write for String {
    fn write_str(&mut self, s: &str) -> Result<(), ErrorKind> {
        self.push_str(s);
        Ok(())
    }
    fn write_fmt(&mut self, a: Arguments) -> Result<(), ErrorKind> {
        self.push_str(&format!("{}", a));
        Ok(())
    }
}
impl Write for io::Stdout {
    fn write_str(&mut self, s: &str) -> Result<(), ErrorKind> {
        io::Write::write_all(self, s.as_bytes()).map_err(|e| e.into())
    }
    fn write_fmt(&mut self, a: Arguments) -> Result<(), ErrorKind> {
        io::Write::write_fmt(self, a).map_err(|e| e.into())
    }
}
impl Write for io::StdoutLock<'_> {
    fn write_str(&mut self, s: &str) -> Result<(), ErrorKind> {
        io::Write::write_all(self, s.as_bytes()).map_err(|e| e.into())
    }
    fn write_fmt(&mut self, a: Arguments) -> Result<(), ErrorKind> {
        io::Write::write_fmt(self, a).map_err(|e| e.into())
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
    fn fmt_raw<T: Write>(&self, stream: &mut T, args: Arguments) -> Result<(), ErrorKind> {
        write!(stream, "{}", args)
    }
    fn fmt_newline<T: Write>(&self, stream: &mut T) -> Result<(), ErrorKind> {
        self.fmt_raw(stream, format_args!("\\THnl\n"))
    }
    fn fmt_indent<T: Write>(&self, stream: &mut T, nest: usize) -> Result<(), ErrorKind> {
        self.fmt_raw(
            stream,
            format_args!("\\THin{{{}}}", self.get_indent_len(nest)),
        )
    }
    fn fmt_break<T: Write>(&self, stream: &mut T) -> Result<(), ErrorKind> {
        self.fmt_raw(stream, format_args!("\\THbk"))
    }
    fn fmt_cs<T: Write>(&self, stream: &mut T, cs: &ControlSequence) -> Result<(), ErrorKind> {
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
    fn fmt_chr<T: Write>(&self, stream: &mut T, chr: &Character) -> Result<(), ErrorKind> {
        if chr.catcode == CatCode::EndLine {
            self.fmt_newline(stream)
        } else {
            self.fmt_raw(
                stream,
                format_args!(
                    "\\THch{{{}}}{{\\{}}}",
                    self.get_chr_catogery(chr),
                    &chr.escape_control(b'^')
                ),
            )
        }
    }
    fn fmt_punct<T: Write>(&self, stream: &mut T, token: &Token) -> Result<(), ErrorKind> {
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
    fn fmt_string<T: Write>(&self, stream: &mut T, s: &str) -> Result<(), ErrorKind> {
        self.fmt_raw(
            stream,
            format_args!("\\THst{{{}}}{{{}}}", self.get_string_category(s), s),
        )
    }
    fn fmt_any<T: Write>(&self, stream: &mut T, any: u32) -> Result<(), ErrorKind> {
        if any == '\r' as u32 || any == '\r' as u32 {
            self.fmt_newline(stream)
        } else {
            unreachable!("Illegal Any Token.")
        }
    }
    fn fmt_token<T: Write>(&self, stream: &mut T, token: &Token) -> Result<(), ErrorKind> {
        match token {
            Token::Char(chr) => self.fmt_chr(stream, chr),
            Token::CS(cs) => self.fmt_cs(stream, cs),
            Token::Any(any) => self.fmt_any(stream, *any),
        }
    }
    fn fmt_tokenlist<T: Write>(
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
    fn fmt_tokenlist_with_raw<'a, T: Write, I>(
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
}
impl<'a> StandardFormatter<'a> {
    pub fn new<'b: 'a>(high_config: &'b HighConfig) -> Self {
        Self { high_config }
    }
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
        }
    }
}
