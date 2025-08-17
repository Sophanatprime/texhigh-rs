use std::num::NonZeroU8;

use smallvec::SmallVec;

use crate::tex::types::*;

#[derive(Debug, PartialEq, Clone)]
#[non_exhaustive]
pub enum ErrorKind {
    TooManyArgs { limits: usize, real: usize },
    InvalidArgSpec(Token),
    InvalidArgDelimiter(Token),
    InvalidTokenToTest(Token),
    UnsupportedArgSpec(char),
    InconsistentGroupStatus(ArgGroupStatus),
    MissingMandatoryArg(char),
    UncompletedArg(char),
    UnimplementedArgSpec(char),
    UnexceptedEndGroupOrEof,
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[non_exhaustive]
pub enum Argument {
    /// Optional argument is unpresent.
    /// `.0`: The current position after scanning this argument.
    UnPresent(usize),
    /// Optional argument is present.
    /// `.0`: The start position of this argument after skipping spaces.
    /// `.1`: The end position of this argument.
    Present(usize, usize),
    /// Mandatory argument.
    /// `.0`: The start position of this argument (may skip spaces, given arg spec).
    /// `.1`: The end position of this argument.
    Span(usize, usize),
    /// Mandatory argument ended by some tokens (i.e., the delimiter).
    /// `.0`: The position before the delimiter.
    /// `.1`: The end position of this argument (after the delimiter).
    Ending(usize, usize),
}

impl Argument {
    pub(crate) fn end(&self) -> usize {
        match self {
            Argument::UnPresent(last) => *last,
            Argument::Present(_, last) => *last,
            Argument::Span(_, last) => *last,
            Argument::Ending(_, last) => *last,
        }
    }
}

type Finder = Box<dyn Fn(&[Token], usize) -> Result<Argument, ErrorKind>>;

pub struct ArgFinder {
    state: Vec<Finder>,
}

impl ArgFinder {
    pub fn new() -> Self {
        let state = Vec::new();
        ArgFinder { state }
    }

    pub fn len(&self) -> usize {
        self.state.len()
    }

    pub fn push(&mut self, kind: ArgSpec) -> Result<&mut Self, ErrorKind> {
        self.state.push(kind.into_finder(true)?);
        Ok(self)
    }

    pub fn parse(tokens: &[Token]) -> Result<Self, ErrorKind> {
        let finder = Self::parse_with_specs(tokens)?;
        Ok(finder.0)
    }

    pub fn parse_with_specs(
        tokens: &[Token],
    ) -> Result<(Self, Vec<u8>), ErrorKind> {
        let mut state = Vec::new();
        let mut specs = Vec::new();
        let mut tokens_iter = tokens.iter();

        while let Some(token) = tokens_iter.next() {
            match token {
                Token::CS(_) => {
                    return Err(ErrorKind::InvalidArgSpec(token.clone()))
                }
                Token::Any(_) => continue,
                Token::Char(chr)
                    if matches!(
                        chr.catcode,
                        CatCode::EndLine | CatCode::Space | CatCode::Ignored
                    ) =>
                {
                    continue
                }
                Token::Char(chr) => match maybe_spec_name(chr) {
                    Some(chr) => {
                        match gen_arg_spec_finder(chr.get(), &mut tokens_iter)?
                        {
                            Some(finder) => {
                                state.push(finder);
                                specs.push(chr.get());
                            }
                            None => continue,
                        }
                    }
                    None => {
                        return Err(ErrorKind::InvalidArgSpec(token.clone()))
                    }
                },
            }
        }

        Ok((ArgFinder { state }, specs))
    }

    pub fn find_all(
        &self,
        tokens: &[Token],
    ) -> Result<Vec<Argument>, ErrorKind> {
        let mut last_pos = 0usize;
        let mut args = Vec::with_capacity(self.state.len());
        for finder in &self.state {
            let arg = finder(tokens, last_pos)?;
            last_pos = arg.end();
            args.push(arg);
        }
        Ok(args)
    }
}

fn maybe_spec_name(chr: &Character) -> Option<NonZeroU8> {
    if matches!(chr.charcode as u32, 1 ..= 127)
        && matches!(
            chr.catcode,
            CatCode::MathShift
                | CatCode::Alignment
                | CatCode::Parameter
                | CatCode::Superscript
                | CatCode::Subscript
                | CatCode::Letter
                | CatCode::Other
                | CatCode::Active
        )
    {
        Some(unsafe { NonZeroU8::new_unchecked(chr.charcode as u32 as u8) })
    } else {
        None
    }
}

fn gen_arg_spec_finder(
    spec_name: u8,
    token_iter: &mut std::slice::Iter<'_, Token>,
) -> Result<Option<Finder>, ErrorKind> {
    let invalid_spec = ErrorKind::InvalidArgSpec(Token::Char(Character::new(
        spec_name as char,
        CatCode::Other,
    )));

    // allowed but unsupported, require no extra arguments, ignore it silently.
    if matches!(spec_name, b'!' | b'+' | b'&' | b'#' | b'@') {
        return Ok(None);
    }

    // allowed but unsupported, require one argument, ignore it silently.
    if matches!(spec_name, b'>' | b'=' | b'?') {
        skip_one_arg(token_iter).map_err(|_| invalid_spec)?;
        return Ok(None);
    }

    // unsupported arg spec: b, c, C.
    if spec_name == b'b' {
        return Err(ErrorKind::UnsupportedArgSpec('b'));
    }
    if spec_name == b'c' {
        skip_maybe_one_arg(token_iter).map_err(|_| invalid_spec)?;
        return Err(ErrorKind::UnsupportedArgSpec('c'));
    }
    if spec_name == b'C' {
        skip_one_arg(token_iter).map_err(|_| invalid_spec)?;
        return Err(ErrorKind::UnsupportedArgSpec('C'));
    }

    let kind = match spec_name {
        b'm' => ArgSpec::m(ArgGroupStatus::Optional),
        b'r' => {
            let left =
                scan_one_token(token_iter, ArgGroupStatus::Optional, true)
                    .map_err(|_| invalid_spec.clone())?;
            let right =
                scan_one_token(token_iter, ArgGroupStatus::Optional, true)
                    .map_err(|_| invalid_spec.clone())?;
            ArgSpec::r(left, right)
        }
        b'R' => {
            let left =
                scan_one_token(token_iter, ArgGroupStatus::Optional, true)
                    .map_err(|_| invalid_spec.clone())?;
            let right =
                scan_one_token(token_iter, ArgGroupStatus::Optional, true)
                    .map_err(|_| invalid_spec.clone())?;
            skip_one_arg(token_iter).map_err(|_| invalid_spec.clone())?;
            ArgSpec::R(left, right, ArgMissingValue::default())
        }
        b'o' => ArgSpec::o,
        b'O' => {
            skip_one_arg(token_iter).map_err(|_| invalid_spec)?;
            ArgSpec::O(ArgMissingValue::default())
        }
        b'd' => {
            let left =
                scan_one_token(token_iter, ArgGroupStatus::Optional, true)
                    .map_err(|_| invalid_spec.clone())?;
            let right =
                scan_one_token(token_iter, ArgGroupStatus::Optional, true)
                    .map_err(|_| invalid_spec.clone())?;
            ArgSpec::d(left, right)
        }
        b'D' => {
            let left =
                scan_one_token(token_iter, ArgGroupStatus::Optional, true)
                    .map_err(|_| invalid_spec.clone())?;
            let right =
                scan_one_token(token_iter, ArgGroupStatus::Optional, true)
                    .map_err(|_| invalid_spec.clone())?;
            skip_one_arg(token_iter).map_err(|_| invalid_spec.clone())?;
            ArgSpec::D(left, right, ArgMissingValue::default())
        }
        b's' => ArgSpec::s,
        b't' => {
            let t = scan_one_token(token_iter, ArgGroupStatus::Optional, true)
                .map_err(|_| invalid_spec.clone())?;
            ArgSpec::t(t)
        }
        b'v' => ArgSpec::v,
        b'l' => ArgSpec::l,
        b'g' => ArgSpec::g,
        b'G' => {
            skip_one_arg(token_iter).map_err(|_| invalid_spec.clone())?;
            ArgSpec::G(ArgMissingValue::default())
        }
        b'u' => {
            let mut t_vec = SmallVec::new();
            let t =
                scan_tokens(token_iter).map_err(|_| invalid_spec.clone())?;
            t.iter().for_each(|t| t_vec.push(t.clone()));
            ArgSpec::u(t_vec)
        }
        b'U' => {
            let mut t_vec = SmallVec::new();
            let t =
                scan_tokens(token_iter).map_err(|_| invalid_spec.clone())?;
            t.iter().for_each(|t| t_vec.push(t.clone()));
            skip_one_arg(token_iter).map_err(|_| invalid_spec.clone())?;
            ArgSpec::U(t_vec, ArgMissingValue)
        }
        b'p' | b'P' | b'k' | b'T' | b'e' | b'E' | b'w' | b'W' => {
            return Err(ErrorKind::UnimplementedArgSpec(spec_name as char));
        }
        // a special spec: '\n'
        b'\n' => {
            let t = scan_one_token(token_iter, ArgGroupStatus::Optional, true)
                .map_err(|_| invalid_spec.clone())?;
            let balanced = match &t {
                Token::CS(cs) => {
                    if matches!(cs.get_csname(), "c_true_bool" | "BooleanTrue")
                    {
                        true
                    } else if matches!(
                        cs.get_csname(),
                        "c_false_bool" | "BooleanFalse"
                    ) {
                        false
                    } else {
                        return Err(invalid_spec);
                    }
                }
                Token::Any(_) => return Err(invalid_spec),
                Token::Char(chr) => {
                    if matches!(chr.charcode, '1' | 't') {
                        true
                    } else if matches!(chr.charcode, '0' | 'f') {
                        false
                    } else {
                        return Err(invalid_spec);
                    }
                }
            };
            ArgSpec::Line(balanced)
        }
        _ => return Err(ErrorKind::UnsupportedArgSpec(spec_name as char)),
    };

    let finder = kind.into_finder(true)?;
    Ok(Some(finder))
}

fn skip_one_arg(
    token_iter: &mut std::slice::Iter<'_, Token>,
) -> Result<usize, ()> {
    let step = skip_one_arg_impl(token_iter.as_slice(), true)?;
    token_iter.advance_by(step).unwrap();
    Ok(step)
}

fn skip_maybe_one_arg(
    token_iter: &mut std::slice::Iter<'_, Token>,
) -> Result<usize, ()> {
    let step = skip_one_arg_impl(token_iter.as_slice(), false)?;
    token_iter.advance_by(step).unwrap();
    Ok(step)
}

fn skip_one_arg_impl(slice: &[Token], required: bool) -> Result<usize, ()> {
    let mut index = 0;
    let mut encounter_space = false;
    while index < slice.len() {
        match &slice[index] {
            Token::Char(chr)
                if matches!(
                    chr.catcode,
                    CatCode::EndLine | CatCode::Space | CatCode::Ignored
                ) =>
            {
                encounter_space = true;
                index += 1;
                continue;
            }
            Token::Any(_) => {
                index += 1;
                continue;
            }
            Token::Char(_) | Token::CS(_) => break,
        }
    }
    if index >= slice.len() {
        if required {
            return Err(());
        }
    } else {
        if encounter_space && !required {
            return Ok(index);
        }
    }

    match &slice[index] {
        Token::Any(_) => unreachable!(),
        Token::CS(_) => index += 1,
        Token::Char(chr) if chr.catcode == CatCode::BeginGroup => {
            index += 1;
            skip_to_end_group(slice, &mut index)?;
        }
        Token::Char(chr) if chr.catcode == CatCode::EndGroup => {
            if required {
                return Err(());
            } else {
                index += 1;
            }
        }
        Token::Char(_) => {
            if required {
                index += 1;
            }
        }
    }

    Ok(index)
}

fn skip_to_end_group(tl: &[Token], index: &mut usize) -> Result<(), ()> {
    let mut group_nest = 1;
    while *index < tl.len() {
        match &tl[*index] {
            Token::Char(chr) if chr.catcode == CatCode::BeginGroup => {
                *index += 1;
                group_nest += 1;
            }
            Token::Char(chr) if chr.catcode == CatCode::EndGroup => {
                *index += 1;
                group_nest -= 1;
                if group_nest == 0 {
                    break;
                }
            }
            _ => *index += 1,
        }
    }
    if group_nest != 0 {
        return Err(());
    }
    Ok(())
}

fn skip_if_char_iter<F>(
    token_iter: &mut std::slice::Iter<'_, Token>,
    p: F,
) -> usize
where
    F: Fn(char, CatCode) -> bool,
{
    let n = skip_if_char(token_iter.as_slice(), p);
    token_iter.advance_by(n).unwrap();
    n
}

fn skip_if_char<F>(tl: &[Token], p: F) -> usize
where
    F: Fn(char, CatCode) -> bool,
{
    let mut index = 0;
    while index < tl.len() {
        match &tl[index] {
            Token::Any(_) => continue,
            Token::CS(_) => break,
            Token::Char(chr) => {
                if p(chr.charcode, chr.catcode) {
                    index += 1;
                    continue;
                } else {
                    break;
                }
            }
        }
    }
    index
}

fn scan_tokens<'t>(
    token_iter: &'t mut std::slice::Iter<'_, Token>,
) -> Result<&'t [Token], ()> {
    skip_if_char_iter(token_iter, |_, cat| {
        matches!(cat, CatCode::EndLine | CatCode::Space | CatCode::Ignored)
    });

    let slice = token_iter.as_slice();

    let mut step = 1;
    match token_iter.next().ok_or(())? {
        Token::CS(_) => {}
        Token::Char(chr) => {
            if chr.catcode == CatCode::EndGroup {
                return Err(());
            }

            if chr.catcode == CatCode::BeginGroup {
                skip_to_end_group(slice, &mut step)?;
                token_iter.advance_by(step - 1).unwrap();
            }
        }
        _ => return Err(()),
    }

    if step == 1 {
        Ok(&slice[0 .. step])
    } else {
        Ok(&slice[1 .. step - 1])
    }
}

fn scan_one_token(
    token_iter: &mut std::slice::Iter<'_, Token>,
    group: ArgGroupStatus,
    remove_spaces: bool,
) -> Result<Token, ()> {
    skip_if_char_iter(token_iter, |_, cat| {
        matches!(cat, CatCode::EndLine | CatCode::Space | CatCode::Ignored)
    });

    let token = token_iter.next().ok_or(())?;
    match token {
        Token::Char(chr) => {
            if chr.catcode == CatCode::EndGroup {
                return Err(());
            }

            if chr.catcode != CatCode::BeginGroup {
                if group == ArgGroupStatus::Disallowed {
                    return Err(());
                } else {
                    return Ok(token.clone());
                }
            }

            // type: {<token>}
            if group == ArgGroupStatus::Disallowed {
                return Err(());
            }

            if remove_spaces {
                skip_if_char_iter(token_iter, |_, cat| {
                    matches!(
                        cat,
                        CatCode::EndLine | CatCode::Space | CatCode::Ignored
                    )
                });
            } else {
                skip_if_char_iter(token_iter, |_, cat| {
                    cat == CatCode::Ignored
                });
            }

            let token = token_iter.next().ok_or(())?;
            match token {
                Token::Char(chr) => {
                    if matches!(
                        chr.catcode,
                        CatCode::BeginGroup | CatCode::EndGroup
                    ) {
                        return Err(());
                    }
                }
                _ => {}
            }

            if remove_spaces {
                skip_if_char_iter(token_iter, |_, cat| {
                    matches!(
                        cat,
                        CatCode::EndLine | CatCode::Space | CatCode::Ignored
                    )
                });
            } else {
                skip_if_char_iter(token_iter, |_, cat| {
                    cat == CatCode::Ignored
                });
            }

            match token_iter.next().ok_or(())? {
                Token::Char(chr) if chr.catcode == CatCode::EndGroup => {}
                _ => return Err(()),
            }
            Ok(token.clone())
        }
        _ => {
            if group == ArgGroupStatus::Required {
                Err(())
            } else {
                Ok(token.clone())
            }
        }
    }
}

#[derive(Debug, Default)]
pub struct ArgMissingValue;

impl ArgMissingValue {
    pub fn new(optional: TokenList) -> Self {
        let _ = optional;
        ArgMissingValue
    }
}

#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ArgGroupStatus {
    Optional,
    Required,
    Disallowed,
}
impl Default for ArgGroupStatus {
    fn default() -> Self {
        ArgGroupStatus::Optional
    }
}

#[non_exhaustive]
#[allow(non_camel_case_types)]
pub enum ArgSpec {
    m(ArgGroupStatus),
    r(Token, Token),
    R(Token, Token, ArgMissingValue),
    o,
    O(ArgMissingValue),
    d(Token, Token),
    D(Token, Token, ArgMissingValue),
    s,
    t(Token),
    v,
    l,
    g,
    G(ArgMissingValue),
    u(SmallVec<[Token; 2]>),
    U(SmallVec<[Token; 2]>, ArgMissingValue),
    /* unimplement, maybe never?
    p(SmallVec<[Token; 4]>),
    P(SmallVec<[Token; 4]>, ArgMissingValue),
    k(CompactString),
    T(SmallVec<[(Token, Option<Token>); 4]>, ArgMissingValue),
    e(SmallVec<[Token; 4]>),
    E(SmallVec<[Token; 4]>, ArgMissingValue),
    w(SmallVec<[(Token, Option<Token>); 4]>),
    W(SmallVec<[(Token, Option<Token>); 4]>, ArgMissingValue),
    */
    Line(bool),
    Any(Finder),
}

impl ArgSpec {
    pub fn into_finder(self, check: bool) -> Result<Finder, ErrorKind> {
        fn check_delimiter(
            test: &Token,
            allow_space: bool,
            f: impl Fn(Token) -> ErrorKind,
        ) -> Result<(), ErrorKind> {
            match test {
                Token::Any(_) => Err(f(test.clone())),
                Token::CS(_) => Ok(()),
                Token::Char(chr) => {
                    if matches!(
                        chr.catcode,
                        CatCode::BeginGroup
                            | CatCode::EndGroup
                            | CatCode::Parameter
                            | CatCode::Ignored
                    ) {
                        Err(f(test.clone()))
                    } else if !allow_space && chr.catcode == CatCode::Space {
                        Err(f(test.clone()))
                    } else {
                        Ok(())
                    }
                }
            }
        }

        let finder = match self {
            ArgSpec::m(g) => grabber_m(g),
            ArgSpec::r(left, right) => {
                if check {
                    check_delimiter(
                        &left,
                        false,
                        ErrorKind::InvalidArgDelimiter,
                    )?;
                    check_delimiter(
                        &right,
                        false,
                        ErrorKind::InvalidArgDelimiter,
                    )?;
                }
                grabber_d(left, right, true, 'r')
            }
            ArgSpec::R(left, right, _) => {
                if check {
                    check_delimiter(
                        &left,
                        false,
                        ErrorKind::InvalidArgDelimiter,
                    )?;
                    check_delimiter(
                        &right,
                        false,
                        ErrorKind::InvalidArgDelimiter,
                    )?;
                }
                grabber_d(left, right, true, 'R')
            }
            ArgSpec::o => grabber_d(
                Token::new_char('[', CatCode::Other),
                Token::new_char(']', CatCode::Other),
                false,
                'o',
            ),
            ArgSpec::O(_) => grabber_d(
                Token::new_char('[', CatCode::Other),
                Token::new_char(']', CatCode::Other),
                false,
                'O',
            ),
            ArgSpec::d(left, right) => {
                if check {
                    check_delimiter(
                        &left,
                        false,
                        ErrorKind::InvalidArgDelimiter,
                    )?;
                    check_delimiter(
                        &right,
                        false,
                        ErrorKind::InvalidArgDelimiter,
                    )?;
                }
                grabber_d(left, right, false, 'd')
            }
            ArgSpec::D(left, right, _) => {
                if check {
                    check_delimiter(
                        &left,
                        false,
                        ErrorKind::InvalidArgDelimiter,
                    )?;
                    check_delimiter(
                        &right,
                        false,
                        ErrorKind::InvalidArgDelimiter,
                    )?;
                }
                grabber_d(left, right, false, 'D')
            }
            ArgSpec::s => grabber_t(Token::new_char('*', CatCode::Other), 's'),
            ArgSpec::t(t) => {
                if check {
                    check_delimiter(&t, false, ErrorKind::InvalidTokenToTest)?;
                }
                grabber_t(t, 't')
            }
            ArgSpec::v => grabber_v(),
            ArgSpec::l => grabber_l(),
            ArgSpec::g => grabber_g('g'),
            ArgSpec::G(_) => grabber_g('G'),
            ArgSpec::u(t_vec) => {
                if check {
                    if t_vec.is_empty() {
                        return Err(ErrorKind::InvalidArgSpec(
                            Token::new_char('c', CatCode::Other),
                        ));
                    }
                    for t in t_vec.iter() {
                        check_delimiter(
                            t,
                            true,
                            ErrorKind::InvalidArgDelimiter,
                        )?;
                    }
                }
                grabber_u(t_vec, 'u')
            }
            ArgSpec::U(t_vec, _) => {
                if check {
                    if t_vec.is_empty() {
                        return Err(ErrorKind::InvalidArgSpec(
                            Token::new_char('c', CatCode::Other),
                        ));
                    }
                    for t in t_vec.iter() {
                        check_delimiter(
                            t,
                            true,
                            ErrorKind::InvalidArgDelimiter,
                        )?;
                    }
                }
                grabber_u(t_vec, 'U')
            }
            ArgSpec::Line(balanced) => grabber_current_line(balanced),
            ArgSpec::Any(finder) => finder,
        };
        Ok(finder)
    }

    pub fn grab_argument(
        self,
        tl: impl AsRef<[Token]>,
        start: usize,
    ) -> Result<Argument, ErrorKind> {
        self.into_finder(true)?(tl.as_ref(), start)
    }
}

fn is_balanced(tl: &[Token]) -> bool {
    let mut group_level = 0;
    for token in tl {
        if let Token::Char(c) = token {
            if c.catcode == CatCode::BeginGroup {
                group_level += 1;
            } else if c.catcode == CatCode::EndGroup {
                group_level -= 1;
            }
        }
    }
    group_level == 0
}

fn find_tl(haystack: &[Token], needle: &[Token]) -> Option<(usize, usize)> {
    if needle.len() <= 1 || !is_balanced(needle) {
        return find_tl_unbalanced(haystack, needle);
    }

    let mut ret = find_tl_unbalanced(haystack, needle)?;
    let mut index = 0;
    while index < haystack.len() {
        match &haystack[index] {
            Token::Char(chr) if chr.catcode == CatCode::EndGroup => {
                if ret.1 <= index {
                    return Some(ret);
                } else {
                    return None;
                }
            }
            Token::Char(chr) if chr.catcode == CatCode::BeginGroup => {
                if ret.1 <= index {
                    return Some(ret);
                }
                let tmp = index;
                index += 1;
                skip_to_end_group(haystack, &mut index).ok()?;
                if ret.0 <= tmp && ret.1 >= index {
                    // haystack: `ab{cd}ef`, needle: `b{cd}`, or `{cd}e`, etc.
                    continue;
                } else if ret.0 >= index {
                    index = ret.0;
                } else {
                    if index < haystack.len() {
                        ret = find_tl_unbalanced(&haystack[index ..], needle)?;
                        ret.0 += index;
                        ret.1 += index;
                    } else {
                        return None;
                    }
                }
            }
            _ => {
                index += 1;
            }
        }
    }

    Some(ret)
}

fn find_tl_unbalanced(
    haystack: &[Token],
    needle: &[Token],
) -> Option<(usize, usize)> {
    let n = needle.len();
    match n {
        0 => return Some((0, 0)),
        1 => {
            let test = &needle[0];
            let mut index = 0;
            while index < haystack.len() {
                match &haystack[index] {
                    Token::Char(chr) if chr.catcode == CatCode::EndGroup => {
                        return None;
                    }
                    Token::Char(chr) if chr.catcode == CatCode::BeginGroup => {
                        index += 1;
                        skip_to_end_group(haystack, &mut index).ok()?;
                    }
                    token @ _ => {
                        if token == test {
                            return Some((index, index + 1));
                        } else {
                            index += 1;
                        }
                    }
                }
            }
            return None;
        }
        _ if haystack.is_empty() => return None,
        _ => {}
    }

    fn skip_spaces(tl: &[Token], index: &mut usize) {
        *index += 1;
        while let Some(token) = tl.get(*index) {
            if token.is_catcodes([
                CatCode::EndLine,
                CatCode::Space,
                CatCode::Ignored,
            ]) {
                *index += 1;
            } else {
                break;
            }
        }
        *index -= 1;
    }

    'outer: for start in 0 .. haystack.len() {
        let mut h_i = start; // haystack index
        let mut n_i = 0; // needle index
        loop {
            let Some(needle_token) = needle.get(n_i) else {
                break;
            };
            if let Token::CS(cs) = needle_token {
                if cs.tag() != 1 {
                    // is cs and not control symbol
                    skip_spaces(needle, &mut n_i);
                }
            }

            let haystack_token = haystack.get(h_i);
            if let Some(Token::CS(cs)) = haystack_token {
                if cs.tag() != 1 {
                    skip_spaces(haystack, &mut h_i);
                }
            }

            match (needle_token, haystack_token) {
                (n_t, Some(h_t)) if n_t == h_t => {
                    h_i += 1;
                    n_i += 1;
                }
                _ => continue 'outer,
            }
        }

        return Some((start, h_i));
    }
    None
}

pub fn grab_normal_argument<'t>(
    tl: &'t [Token],
    start: usize,
    group: ArgGroupStatus,
) -> Result<(&'t [Token], usize), ErrorKind> {
    let arg = grabber_m(group)(tl, start)?;
    let Argument::Span(l, r) = arg else { unreachable!() };
    Ok((unsafe { tl.get_unchecked(l .. r) }, r))
}

pub fn grab_current_line<'t>(
    tl: &'t [Token],
    start: usize,
    balanced: bool,
) -> Result<(&'t [Token], usize), ErrorKind> {
    let arg = grabber_current_line(balanced)(tl, start)?;
    let Argument::Ending(s, e) = arg else { unreachable!() };
    Ok((unsafe { tl.get_unchecked(start .. s) }, e))
}

fn grabber_m(group: ArgGroupStatus) -> Finder {
    let finder =
        move |tl: &[Token], start: usize| -> Result<Argument, ErrorKind> {
            if start >= tl.len() {
                return Err(ErrorKind::MissingMandatoryArg('m'));
            }
            let mut index = start
                + skip_if_char(&tl[start ..], |_, cat| {
                    matches!(
                        cat,
                        CatCode::EndLine | CatCode::Space | CatCode::Ignored
                    )
                });
            let skipped_spaces = index;
            if start >= tl.len() {
                return Err(ErrorKind::MissingMandatoryArg('m'));
            }

            match &tl[index] {
                Token::Any(_) => { /* impossible, already skipped */ }
                Token::Char(chr) if chr.catcode == CatCode::EndGroup => {
                    return Err(ErrorKind::MissingMandatoryArg('m'));
                }
                Token::Char(chr) if chr.catcode == CatCode::BeginGroup => {
                    if group == ArgGroupStatus::Disallowed {
                        return Err(ErrorKind::InconsistentGroupStatus(group));
                    } else {
                        index += 1;
                        skip_to_end_group(tl, &mut index)
                            .map_err(|_| ErrorKind::UncompletedArg('m'))?;
                        return Ok(Argument::Span(skipped_spaces, index));
                    }
                }
                Token::CS(_) | Token::Char(_) => {
                    if group == ArgGroupStatus::Required {
                        return Err(ErrorKind::InconsistentGroupStatus(group));
                    } else {
                        index += 1;
                        return Ok(Argument::Span(skipped_spaces, index));
                    }
                }
            }

            Err(ErrorKind::UncompletedArg('m'))
        };
    Box::new(finder)
}

fn grabber_d(
    left: Token,
    right: Token,
    required: bool,
    spec_name: char,
) -> Finder {
    let finder = move |tl: &[Token],
                       start: usize|
          -> Result<Argument, ErrorKind> {
        if start >= tl.len() {
            if required {
                return Err(ErrorKind::MissingMandatoryArg(spec_name));
            } else {
                return Ok(Argument::UnPresent(start));
            }
        }
        let mut index = start
            + skip_if_char(&tl[start ..], |_, cat| {
                matches!(
                    cat,
                    CatCode::EndLine | CatCode::Space | CatCode::Ignored
                )
            });
        let skipped_spaces = index;
        if start >= tl.len() {
            if required {
                return Err(ErrorKind::MissingMandatoryArg(spec_name));
            } else {
                return Ok(Argument::UnPresent(start));
            }
        }

        let token = &tl[index];
        if token == &left {
            index += 1;
            let mut nest_level = 1;

            while index < tl.len() {
                let token = &tl[index];
                match token {
                    Token::Char(chr) if chr.catcode == CatCode::BeginGroup => {
                        index += 1;
                        skip_to_end_group(tl, &mut index).map_err(|_| {
                            ErrorKind::UncompletedArg(spec_name)
                        })?;
                    }
                    Token::Char(chr) if chr.catcode == CatCode::EndGroup => {
                        break; // error
                    }
                    _ => {
                        index += 1;
                        if token == &left {
                            nest_level += 1;
                        } else if token == &right {
                            nest_level -= 1;
                            if nest_level == 0 {
                                // only way to return ok
                                if required {
                                    return Ok(Argument::Span(
                                        skipped_spaces,
                                        index,
                                    ));
                                } else {
                                    return Ok(Argument::Present(
                                        skipped_spaces,
                                        index,
                                    ));
                                }
                            }
                        }
                    }
                }
            }

            Err(ErrorKind::UncompletedArg(spec_name))
        } else {
            if required {
                Err(ErrorKind::MissingMandatoryArg(spec_name))
            } else {
                Ok(Argument::UnPresent(start))
            }
        }
    };
    Box::new(finder)
}

fn grabber_t(test: Token, spec_name: char) -> Finder {
    let _ = spec_name;

    let finder =
        move |tl: &[Token], start: usize| -> Result<Argument, ErrorKind> {
            if start >= tl.len() {
                return Ok(Argument::UnPresent(start));
            }
            let mut index = start
                + skip_if_char(&tl[start ..], |_, cat| {
                    matches!(
                        cat,
                        CatCode::EndLine | CatCode::Space | CatCode::Ignored
                    )
                });
            let skipped_spaces = index;
            if start >= tl.len() {
                return Ok(Argument::UnPresent(start));
            }
            if &tl[index] == &test {
                index += 1;
                Ok(Argument::Present(skipped_spaces, index))
            } else {
                Ok(Argument::UnPresent(start))
            }
        };
    Box::new(finder)
}

fn grabber_v() -> Finder {
    let finder = |tl: &[Token], start: usize| -> Result<Argument, ErrorKind> {
        if start >= tl.len() {
            return Err(ErrorKind::MissingMandatoryArg('v'));
        }
        let mut index = start
            + skip_if_char(&tl[start ..], |_, cat| {
                matches!(
                    cat,
                    CatCode::EndLine | CatCode::Space | CatCode::Ignored
                )
            });
        let skipped_spaces = index;
        if start >= tl.len() {
            return Err(ErrorKind::MissingMandatoryArg('v'));
        }

        match &tl[index] {
            Token::Any(_) => { /* impossible, already skipped */ }
            Token::CS(_) => { /* no allowed */ }
            Token::Char(chr) if chr.catcode == CatCode::BeginGroup => {
                let (del_l, del_r) = ('{', '}');
                // if token is begin group, then it must be '{'
                if chr.charcode != del_l {
                    return Err(ErrorKind::UncompletedArg('v'));
                }

                index += 1;
                let mut group_nest = 1;
                while index < tl.len() {
                    match &tl[index] {
                        Token::CS(cs) if cs.tag() != 0 => {
                            index += 1;
                            let curr = cs.get_csname().chars().next();
                            if curr == Some(del_l) {
                                group_nest += 1;
                            } else if curr == Some(del_r) {
                                group_nest -= 1;
                                if group_nest == 0 {
                                    return Ok(Argument::Span(
                                        skipped_spaces,
                                        index,
                                    ));
                                }
                            }
                        }
                        Token::Char(chr) if chr.charcode == del_l => {
                            index += 1;
                            group_nest += 1;
                        }
                        Token::Char(chr) if chr.charcode == del_r => {
                            index += 1;
                            group_nest -= 1;
                            if group_nest == 0 {
                                return Ok(Argument::Span(
                                    skipped_spaces,
                                    index,
                                ));
                            }
                        }
                        _ => index += 1,
                    }
                }
            }
            Token::Char(chr) => {
                let delimiter = chr.charcode;
                index += 1;
                while index < tl.len() {
                    match &tl[index] {
                        Token::Any(any) if *any == delimiter as u32 => {
                            index += 1;
                            return Ok(Argument::Span(skipped_spaces, index));
                        }
                        Token::CS(cs) if cs.tag() != 0 => {
                            index += 1;
                            if cs.get_csname().chars().next()
                                == Some(delimiter)
                            {
                                return Ok(Argument::Span(
                                    skipped_spaces,
                                    index,
                                ));
                            }
                        }
                        Token::Char(chr) if chr.charcode == delimiter => {
                            index += 1;
                            return Ok(Argument::Span(skipped_spaces, index));
                        }
                        _ => index += 1,
                    }
                }
            }
        }

        Err(ErrorKind::UncompletedArg('v'))
    };
    Box::new(finder)
}

fn grabber_l() -> Finder {
    let finder = |tl: &[Token], start: usize| -> Result<Argument, ErrorKind> {
        if start >= tl.len() {
            return Err(ErrorKind::MissingMandatoryArg('l'));
        }
        let mut index = start;
        while index < tl.len() {
            match &tl[index] {
                Token::Char(chr) if chr.catcode == CatCode::BeginGroup => {
                    if chr.charcode == '{' {
                        // no need step index, we get: <tokens>{
                        return Ok(Argument::Span(start, index));
                    } else {
                        index += 1;
                        skip_to_end_group(tl, &mut index)
                            .map_err(|_| ErrorKind::UncompletedArg('l'))?;
                    }
                }
                Token::Char(chr) if chr.catcode == CatCode::EndGroup => {
                    // error, we get: <tokens>}
                    return Err(ErrorKind::UncompletedArg('l'));
                }
                _ => index += 1,
            }
        }
        Err(ErrorKind::UncompletedArg('l'))
    };
    Box::new(finder)
}

fn grabber_g(spec_name: char) -> Finder {
    let finder = move |tl: &[Token],
                       start: usize|
          -> Result<Argument, ErrorKind> {
        if start >= tl.len() {
            return Ok(Argument::UnPresent(start));
        }

        let mut index = start;
        while index < tl.len() {
            match &tl[index] {
                Token::Any(_) => index += 1,
                Token::CS(cs) => {
                    if matches!(cs.get_csname(), "relax" | "scan_stop:") {
                        index += 1;
                        return Ok(Argument::UnPresent(index));
                    } else if matches!(
                        cs.get_csname(),
                        "bgroup" | "c_group_begin_token"
                    ) {
                        let skipped_spaces = index;
                        index += 1;
                        skip_to_end_group(tl, &mut index).map_err(|_| {
                            ErrorKind::UncompletedArg(spec_name)
                        })?;
                        return Ok(Argument::Present(skipped_spaces, index));
                    } else {
                        break;
                    }
                }
                Token::Char(chr) => {
                    if matches!(
                        chr.catcode,
                        CatCode::Space | CatCode::EndLine | CatCode::Ignored
                    ) {
                        index += 1;
                        continue;
                    } else if chr.catcode == CatCode::BeginGroup {
                        let skipped_spaces = index;
                        index += 1;
                        skip_to_end_group(tl, &mut index).map_err(|_| {
                            ErrorKind::UncompletedArg(spec_name)
                        })?;
                        return Ok(Argument::Present(skipped_spaces, index));
                    } else {
                        break;
                    }
                }
            }
        }
        // use start, do not skip spaces
        Ok(Argument::UnPresent(start))
    };
    Box::new(finder)
}

fn grabber_u(test: SmallVec<[Token; 2]>, spec_name: char) -> Finder {
    assert!(!test.is_empty(), "delimiters of arg spec `u` cannot be empty");
    let finder =
        move |tl: &[Token], start: usize| -> Result<Argument, ErrorKind> {
            // test cannot be empty.
            if start >= tl.len() {
                return Err(ErrorKind::MissingMandatoryArg(spec_name));
            }

            let del_start = find_tl(&tl[start ..], &test)
                .ok_or_else(|| ErrorKind::UncompletedArg(spec_name))?;
            Ok(Argument::Ending(start + del_start.0, start + del_start.1))
        };
    Box::new(finder)
}

fn grabber_current_line(balanced: bool) -> Finder {
    let finder = move |tl: &[Token],
                       start: usize|
          -> Result<Argument, ErrorKind> {
        if start >= tl.len() {
            return Ok(Argument::Ending(start, start));
        }

        let mut index = start;
        while index < tl.len() {
            match &tl[index] {
                Token::Any(any) => {
                    if *any == '\n' as u32 || *any == '\r' as u32 {
                        return Ok(Argument::Ending(index, index + 1));
                    } else {
                        index += 1;
                    }
                }
                Token::CS(_) => index += 1,
                Token::Char(chr) => {
                    if chr.catcode == CatCode::EndLine {
                        return Ok(Argument::Ending(index, index + 1));
                    } else if chr.catcode == CatCode::EndGroup && balanced {
                        return Err(ErrorKind::UnexceptedEndGroupOrEof);
                    } else if chr.catcode == CatCode::BeginGroup && balanced {
                        index += 1;
                        skip_to_end_group(tl, &mut index)
                            .map_err(|_| ErrorKind::UnexceptedEndGroupOrEof)?;
                    } else {
                        index += 1;
                    }
                }
            }
        }
        Ok(Argument::Ending(index, index))
    };
    Box::new(finder)
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::CTab;

    #[test]
    fn test_find_tl() {
        let source = r##"\a bc{jk\l}\relax   \.oken\. end\l  \m\n"##;
        let catcode = CTab::document();
        let tl = TokenList::parse(source, &catcode);

        assert_eq!(
            find_tl(&tl, &TokenList::parse(r"", &catcode)),
            Some((0, 0))
        );
        // needle.len = 1, match needle exactly
        assert_eq!(
            find_tl(&tl, &TokenList::parse(r"\a", &catcode)),
            Some((0, 1))
        );
        // needle.len > 1, match spaces after control word as much as possible
        assert_eq!(
            find_tl(&tl, &TokenList::parse(r"\a ", &catcode)),
            Some((0, 2))
        );
        assert_eq!(
            find_tl(&tl, &TokenList::parse(r"bc", &catcode)),
            Some((2, 4))
        );
        assert_eq!(find_tl(&tl, &TokenList::parse(r"b c", &catcode)), None);
        // unbalanced tl, match needle exactly
        assert_eq!(
            find_tl(&tl, &TokenList::parse(r"bc{", &catcode)),
            Some((2, 5))
        );
        assert_eq!(
            find_tl(&tl, &TokenList::parse(r"bc{jk\l}", &catcode)),
            Some((2, 9))
        );
        assert_eq!(
            find_tl(&tl, &TokenList::parse(r"bc{jk\l  }", &catcode)),
            Some((2, 9))
        );
        // balanced tl, but is protected by { }
        assert_eq!(find_tl(&tl, &TokenList::parse(r"jk", &catcode)), None);
        assert_eq!(
            find_tl(&tl, &TokenList::parse(r"\relax", &catcode)),
            Some((9, 10))
        );
        assert_eq!(
            find_tl(&tl, &TokenList::parse(r"\relax ", &catcode)),
            Some((9, 13))
        );
        assert_eq!(
            find_tl(&tl, &TokenList::parse(r"\relax   ", &catcode)),
            Some((9, 13))
        );
        assert_eq!(
            find_tl(&tl, &TokenList::parse(r"\relax      ", &catcode)),
            Some((9, 13))
        );
        assert_eq!(
            find_tl(&tl, &TokenList::parse(r"}\relax", &catcode)),
            Some((8, 13))
        );
        assert_eq!(
            find_tl(&tl, &TokenList::parse(r"}\relax ", &catcode)),
            Some((8, 13))
        );
        assert_eq!(
            find_tl(&tl, &TokenList::parse(r"}\relax     ", &catcode)),
            Some((8, 13))
        );
        assert_eq!(
            find_tl(&tl, &TokenList::parse(r" \relax", &catcode)),
            None
        );
        assert_eq!(
            find_tl(&tl, &TokenList::parse(r"\.", &catcode)),
            Some((13, 14))
        );
        assert_eq!(
            find_tl(&tl, &TokenList::parse(r"  \.", &catcode)),
            Some((11, 14))
        );
        assert_eq!(
            find_tl(&tl, &TokenList::parse(r"\. ", &catcode)),
            Some((18, 20))
        );
        assert_eq!(find_tl(&tl, &TokenList::parse(r"\.    ", &catcode)), None);
        assert_eq!(
            find_tl(&tl, &TokenList::parse(r"\l", &catcode)),
            Some((23, 24))
        );
        assert_eq!(
            find_tl(&tl, &TokenList::parse(r"\l   ", &catcode)),
            Some((23, 26))
        );
        assert_eq!(
            find_tl(&tl, &TokenList::parse(r"\l\m\n", &catcode)),
            Some((23, 28))
        );
    }

    #[test]
    fn skip_args() {
        let source = r##"c{s} cs c O{} O} O"##;
        let catcode = CTab::document();
        let tl = TokenList::parse(source, &catcode);

        assert_eq!(skip_one_arg_impl(&tl[1 ..], false).unwrap(), 3);
        assert_eq!(skip_one_arg_impl(&tl[6 ..], false).unwrap(), 0);
        assert_eq!(skip_one_arg_impl(&tl[9 ..], false).unwrap(), 1);

        assert_eq!(skip_one_arg_impl(&tl[11 ..], true).unwrap(), 2);
        assert!(skip_one_arg_impl(&tl[15 ..], true).is_err());
        assert!(skip_one_arg_impl(&tl[18 ..], true).is_err());
    }

    #[test]
    fn scan_token() {
        let source = r##"t* t \l t{ @} t{aa} t} t{} t{{} t"##;
        let mut catcode = CTab::document();
        catcode.emplace_item('@', CatCode::Ignored);
        let tl = TokenList::parse(source, &catcode);

        assert_eq!(
            scan_one_token(
                &mut tl[1 ..].iter(),
                ArgGroupStatus::Optional,
                false
            )
            .unwrap(),
            Token::new_char('*', CatCode::Other)
        );
        assert_eq!(
            scan_one_token(
                &mut tl[4 ..].iter(),
                ArgGroupStatus::Optional,
                false
            )
            .unwrap(),
            Token::new_cs("l")
        );
        assert_eq!(
            scan_one_token(
                &mut tl[8 ..].iter(),
                ArgGroupStatus::Optional,
                false
            )
            .unwrap(),
            Token::new_char(' ', CatCode::Space)
        );
        assert!(scan_one_token(
            &mut tl[14 ..].iter(),
            ArgGroupStatus::Optional,
            false
        )
        .is_err());
        assert!(scan_one_token(
            &mut tl[20 ..].iter(),
            ArgGroupStatus::Optional,
            false
        )
        .is_err());
        assert!(scan_one_token(
            &mut tl[23 ..].iter(),
            ArgGroupStatus::Optional,
            false
        )
        .is_err());
        assert!(scan_one_token(
            &mut tl[27 ..].iter(),
            ArgGroupStatus::Optional,
            false
        )
        .is_err());
        assert!(scan_one_token(
            &mut tl[32 ..].iter(),
            ArgGroupStatus::Optional,
            false
        )
        .is_err());

        let tl = TokenList::parse(r" { ab\f } {.} \relax {}", &catcode);
        let mut iter = tl.iter();
        assert_eq!(scan_tokens(&mut iter), Ok(&tl[2 .. 7]));
        assert_eq!(scan_tokens(&mut iter), Ok(&tl[10 .. 11]));
        assert_eq!(scan_tokens(&mut iter), Ok(&tl[13 .. 14]));
        assert_eq!(scan_tokens(&mut iter), Ok(&tl[16 .. 16]));
        assert_eq!(scan_tokens(&mut iter), Err(()));
    }

    #[test]
    fn test_grab_m() {
        let catcode = CTab::document();
        let spec = TokenList::parse(r"m m m", &catcode);
        let finder = ArgFinder::parse(&spec).unwrap();

        let source = r##" a{ \relax a} {\iffalse}\fi"##;
        let source = TokenList::parse(source, &catcode);
        let args = finder.find_all(&source).unwrap();
        assert_eq!(args.len(), 3);
        assert_eq!(&args[0], &Argument::Span(1, 2));
        assert_eq!(&args[1], &Argument::Span(2, 8));
        assert_eq!(&args[2], &Argument::Span(9, 12));

        assert_eq!(
            grab_normal_argument(&source, 0, ArgGroupStatus::Optional)
                .unwrap(),
            (&source[1 .. 2], 2)
        );
        assert_eq!(
            grab_normal_argument(&source, 0, ArgGroupStatus::Required),
            Err(ErrorKind::InconsistentGroupStatus(ArgGroupStatus::Required))
        );
        assert_eq!(
            grab_normal_argument(&source, 0, ArgGroupStatus::Disallowed)
                .unwrap(),
            (&source[1 .. 2], 2)
        );
        assert_eq!(
            grab_normal_argument(&source, 2, ArgGroupStatus::Optional)
                .unwrap(),
            (&source[2 .. 8], 8)
        );
        assert_eq!(
            grab_normal_argument(&source, 2, ArgGroupStatus::Required)
                .unwrap(),
            (&source[2 .. 8], 8)
        );
        assert_eq!(
            grab_normal_argument(&source, 2, ArgGroupStatus::Disallowed),
            Err(ErrorKind::InconsistentGroupStatus(
                ArgGroupStatus::Disallowed
            ))
        );
        assert_eq!(
            grab_normal_argument(&source, 8, ArgGroupStatus::Optional)
                .unwrap(),
            (&source[9 .. 12], 12)
        );
        assert_eq!(
            grab_normal_argument(&source, 8, ArgGroupStatus::Required)
                .unwrap(),
            (&source[9 .. 12], 12)
        );
        assert_eq!(
            grab_normal_argument(&source, 8, ArgGroupStatus::Disallowed),
            Err(ErrorKind::InconsistentGroupStatus(
                ArgGroupStatus::Disallowed
            ))
        );

        let source = r##"a \bgroup }"##;
        let source = TokenList::parse(source, &catcode);
        assert!(finder.find_all(&source).is_err());

        let source = r##"a {~}"##;
        let source = TokenList::parse(source, &catcode);
        assert!(finder.find_all(&source).is_err());
    }

    #[test]
    fn test_grab_d() {
        let catcode = CTab::document();

        let spec = TokenList::parse(r#"o r() D{ \l }{ \r }{NN} s"#, &catcode);
        let finder = ArgFinder::parse(&spec).unwrap();

        let source = r##"[a[b]] ({(a}) \l aa \l\r\r"##;
        let source = TokenList::parse(source, &catcode);
        let args = finder.find_all(&source).unwrap();
        assert_eq!(&args[0], &Argument::Present(0, 6));
        assert_eq!(&args[1], &Argument::Span(7, 13));
        assert_eq!(&args[2], &Argument::Present(14, 22));
        assert_eq!(&args[3], &Argument::UnPresent(22));
        let args = finder.find_all(&source[6 ..]).unwrap();
        assert_eq!(&args[0], &Argument::UnPresent(0));
        assert_eq!(&args[1], &Argument::Span(1, 7));
        assert_eq!(&args[2], &Argument::Present(8, 16));
        assert_eq!(&args[3], &Argument::UnPresent(16));

        let source = TokenList::parse(r"[a]\l()\r", &catcode);
        assert_eq!(
            finder.find_all(&source),
            Err(ErrorKind::MissingMandatoryArg('r'))
        );

        let source = TokenList::parse(r"[a](\)", &catcode);
        assert_eq!(
            finder.find_all(&source),
            Err(ErrorKind::UncompletedArg('r'))
        );

        let source = TokenList::parse(r"[a]()\l\l..\r", &catcode);
        assert_eq!(
            finder.find_all(&source),
            Err(ErrorKind::UncompletedArg('D'))
        );
    }

    #[test]
    fn test_grab_t() {
        let catcode = CTab::document();

        let spec = TokenList::parse(r#"t* !s t\relax t&"#, &catcode);
        let finder = ArgFinder::parse(&spec).unwrap();
        let source = r##" * \relax & *"##;
        let source = TokenList::parse(source, &catcode);
        let args = finder.find_all(&source).unwrap();
        assert_eq!(&args[0], &Argument::Present(1, 2));
        assert_eq!(&args[1], &Argument::UnPresent(2));
        assert_eq!(&args[2], &Argument::Present(3, 4));
        assert_eq!(&args[3], &Argument::Present(5, 6));
    }

    #[test]
    fn test_grab_v() {
        let catcode = CTab::document();

        let spec = TokenList::parse(r"v v", &catcode);
        let finder = ArgFinder::parse(&spec).unwrap();

        let source = r##"|ab{\}\| {\{}\} |"##;
        let source = TokenList::parse(source, &catcode);
        let args = finder.find_all(&source).unwrap();
        assert_eq!(&args[0], &Argument::Span(0, 6));
        assert_eq!(&args[1], &Argument::Span(7, 11));

        let source = TokenList::parse(r"|ab{}| {{} |", &catcode);
        assert_eq!(
            finder.find_all(&source),
            Err(ErrorKind::UncompletedArg('v'))
        );

        let source = TokenList::parse(r"|ab{} {{} |", &catcode);
        assert_eq!(
            finder.find_all(&source),
            Err(ErrorKind::MissingMandatoryArg('v'))
        );
    }

    #[test]
    fn test_grab_l() {
        let mut catcode = CTab::document();
        catcode.emplace_item('(', CatCode::BeginGroup);

        let spec = TokenList::parse(r"l m o l", &catcode);
        let finder = ArgFinder::parse(&spec).unwrap();

        let source = r##" [dad\{ {[} [aa] ] {"##;
        let source = TokenList::parse(source, &catcode);
        let args = finder.find_all(&source).unwrap();
        assert_eq!(&args[0], &Argument::Span(0, 7));
        assert_eq!(&args[1], &Argument::Span(7, 10));
        assert_eq!(&args[2], &Argument::Present(11, 15));
        assert_eq!(&args[3], &Argument::Span(15, 18));

        let source = TokenList::parse(r"[dad {[} [{}] ", &catcode);
        assert_eq!(
            finder.find_all(&source),
            Err(ErrorKind::UncompletedArg('l'))
        );

        let source = TokenList::parse(r"[dad {[} [{}]", &catcode);
        assert_eq!(
            finder.find_all(&source),
            Err(ErrorKind::MissingMandatoryArg('l'))
        );

        let source = TokenList::parse(r"[dad {[} [{}] (", &catcode);
        assert_eq!(
            finder.find_all(&source),
            Err(ErrorKind::UncompletedArg('l'))
        );
    }

    #[test]
    fn test_grab_g() {
        let catcode = CTab::document();

        let spec = TokenList::parse(r"m g G{}", &catcode);
        let finder = ArgFinder::parse(&spec).unwrap();

        let source = r##" m {\{} \relax {!}"##;
        let source = TokenList::parse(source, &catcode);
        let args = finder.find_all(&source).unwrap();
        assert_eq!(&args[0], &Argument::Span(1, 2));
        assert_eq!(&args[1], &Argument::Present(3, 6));
        assert_eq!(&args[2], &Argument::UnPresent(8));

        let source = TokenList::parse(r"m {\} .", &catcode);
        assert_eq!(
            finder.find_all(&source),
            Err(ErrorKind::UncompletedArg('g'))
        );
    }

    #[test]
    fn test_grab_u() {
        let catcode = CTab::document();

        let spec = TokenList::parse(r"u\right U\#{##} s", &catcode);
        let finder = ArgFinder::parse(&spec).unwrap();

        let source = r##"aa{\right a#1}\{ \right ##\#"##;
        let source = TokenList::parse(source, &catcode);
        let args = finder.find_all(&source).unwrap();
        assert_eq!(&args[0], &Argument::Ending(11, 12));
        assert_eq!(&args[1], &Argument::Ending(15, 16));

        let source = TokenList::parse(r##"u{\right}"##, &catcode);
        assert_eq!(
            finder.find_all(&source),
            Err(ErrorKind::UncompletedArg('u'))
        );

        let source = TokenList::parse(r##""##, &catcode);
        assert_eq!(
            finder.find_all(&source),
            Err(ErrorKind::MissingMandatoryArg('u'))
        );

        let spec = TokenList::parse(r"u{^^M} u{^^M} u{^^J}", &catcode);
        let finder = ArgFinder::parse(&spec).unwrap();
        let source = "abc\\relax\r\n a\rb^^M^^J";
        let source = TokenList::parse(source, &catcode);
        assert_eq!(source.len(), 11);
        let args = finder.find_all(&source).unwrap();
        assert_eq!(&args[0], &Argument::Ending(4, 5));
        assert_eq!(&args[1], &Argument::Ending(7, 8));
        assert_eq!(&args[2], &Argument::Ending(10, 11));
    }

    #[test]
    fn test_grab_curr_line() {
        let catcode = CTab::document();

        let spec =
            TokenList::parse(r"^^M ^^J{t} ^^J{\BooleanFalse } ^^M", &catcode);
        let finder = ArgFinder::parse(&spec).unwrap();

        let source = r##"^^M \iffalse{\def"##;
        let source = TokenList::parse(source, &catcode);
        let args = finder.find_all(&source).unwrap();
        assert_eq!(&args[0], &Argument::Ending(0, 1));
        assert_eq!(&args[1], &Argument::Ending(5, 5));

        let source = TokenList::parse(r"{\def}^^M \iffasle{\fi}^^M", &catcode);
        let args = finder.find_all(&source).unwrap();
        assert_eq!(&args[0], &Argument::Ending(3, 4));
        assert_eq!(&args[1], &Argument::Ending(9, 10));

        let source = TokenList::parse(r"{\def^^M \iffasle\{\fi}^^M", &catcode);
        let args = finder.find_all(&source).unwrap();
        assert_eq!(&args[0], &Argument::Ending(8, 9));
        assert_eq!(&args[1], &Argument::Ending(9, 9));

        let source = TokenList::parse(r"{\def^^M\}\fi^^M", &catcode);
        assert_eq!(
            finder.find_all(&source),
            Err(ErrorKind::UnexceptedEndGroupOrEof)
        );
    }
}
