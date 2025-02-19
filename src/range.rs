use std::cmp::{Ord, Ordering};
use std::fmt::Debug;
use std::iter::{IntoIterator, Iterator, Step};
use std::ops::{self, RangeInclusive};

use compact_str::{format_compact, CompactString};

use crate::types::ErrorKind;

pub(crate) trait MinMaxValue: Copy + Ord + Step + Eq {
    const MIN: Self;
    const MAX: Self;
}
macro_rules! impl_minmaxvalue {
    ($($t:ty),+ $(,)?) => {
        $(impl MinMaxValue for $t {
            const MAX: Self = <$t>::MAX;
            const MIN: Self = <$t>::MIN;
        })+
    };
}
impl_minmaxvalue! {
    char, i128, u128, i64, u64, i32, u32, i16, u16, i8, u8, isize, usize
}

#[derive(Clone, PartialEq, Eq)]
pub struct NumberSpan<T: Debug + Copy + Ord + Step + Eq>(RangeInclusive<T>);

impl Debug for NumberSpan<char> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::write(
            f,
            format_args!(
                "NumberSpan(U+{:0X}..=U+{:0X})",
                *self.0.start() as u32,
                *self.0.end() as u32
            ),
        )
    }
}

impl<T: Debug + Copy + Ord + Step + Eq> NumberSpan<T> {
    pub fn cover(&self, rhs: &Self) -> Self {
        NumberSpan(
            *self.0.start().min(rhs.0.start())
                ..= *self.0.end().max(rhs.0.end()),
        )
    }
    pub fn contains(&self, item: &T) -> bool {
        self.0.contains(item)
    }
    pub fn cmp(&self, item: &T) -> Ordering {
        if self.0.start() > item {
            Ordering::Greater
        } else if self.0.end() < item {
            Ordering::Less
        } else {
            Ordering::Equal
        }
    }
    pub fn start(&self) -> T {
        *self.0.start()
    }
    pub fn end(&self) -> T {
        *self.0.end()
    }
}

impl<T: Debug + Copy + Ord + Step + Eq> IntoIterator for NumberSpan<T> {
    type Item = T;
    type IntoIter = RangeInclusive<T>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<T: Debug + Copy + Ord + Step + Eq> From<T> for NumberSpan<T> {
    fn from(value: T) -> Self {
        NumberSpan(value ..= value)
    }
}

impl<T: Debug + Copy + Ord + Step + Eq> From<RangeInclusive<T>>
    for NumberSpan<T>
{
    fn from(value: RangeInclusive<T>) -> Self {
        assert!(value.start() <= value.end());
        NumberSpan(value)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum RangeItem<T>
where
    T: Ord + Clone,
{
    Single(T),
    From(ops::RangeFrom<T>),
    Range(ops::RangeInclusive<T>),
    To(ops::RangeToInclusive<T>),
    Full(ops::RangeFull),
}

impl<T: MinMaxValue + Debug> From<RangeItem<T>> for NumberSpan<T> {
    fn from(value: RangeItem<T>) -> Self {
        match value {
            RangeItem::Single(v) => NumberSpan(v ..= v),
            RangeItem::From(v) => NumberSpan(v.start ..= MinMaxValue::MAX),
            RangeItem::Range(v) => NumberSpan(*v.start() ..= *v.end()),
            RangeItem::To(v) => NumberSpan(MinMaxValue::MIN ..= v.end),
            RangeItem::Full(_) => {
                NumberSpan(MinMaxValue::MIN ..= MinMaxValue::MAX)
            }
        }
    }
}

impl TryFrom<RangeItem<i64>> for NumberSpan<char> {
    type Error = ErrorKind;
    fn try_from(value: RangeItem<i64>) -> Result<Self, Self::Error> {
        let err = Err(ErrorKind::CharFromNumber);
        let res = match value {
            RangeItem::Single(v) => match try_get_char(v) {
                Some(v) => NumberSpan(v ..= v),
                _ => return err,
            },
            RangeItem::From(v) => match try_get_char(v.start) {
                Some(v) => NumberSpan(v ..= char::MAX),
                _ => return err,
            },
            RangeItem::Range(v) => {
                match (try_get_char(*v.start()), try_get_char(*v.end())) {
                    (Some(v1), Some(v2)) => NumberSpan(v1 ..= v2),
                    _ => return err,
                }
            }
            RangeItem::To(v) => match try_get_char(v.end) {
                Some(v) => NumberSpan(char::MIN ..= v),
                _ => return err,
            },
            RangeItem::Full(_) => NumberSpan(char::MIN ..= char::MAX),
        };
        Ok(res)
    }
}

pub(crate) fn is_valid_char(num: i64) -> bool {
    !(num < char::MIN as i64
        || num > char::MAX as i64
        || (num > 0xD7FF && num < 0xE000))
}

pub(crate) fn try_get_char(num: i64) -> Option<char> {
    unsafe {
        if is_valid_char(num) {
            Some(char::from_u32_unchecked(num as u32))
        } else {
            if num == i64::MAX {
                Some(char::MAX)
            } else if num == i64::MIN {
                Some(char::MIN)
            } else {
                None
            }
        }
    }
}

pub fn parse_num(text: &str) -> Result<i64, ErrorKind> {
    if text.is_empty() {
        return Err(ErrorKind::InvalidInput);
    }

    let mut chars = text.chars();
    let mut signal = 0;
    let mut c = 0 as char;
    while let Some(c_) = chars.next() {
        if c_ == '+' {
        } else if c_ == '-' {
            signal += 1;
        } else {
            c = c_;
            break;
        }
    }

    if c == (0 as char) {
        return Err(ErrorKind::InvalidInput);
    }

    if c == '0' {
        if let Some(c2) = chars.next() {
            let (radix, num_str) = {
                if c2 == 'x' || c2 == 'X' {
                    (16, chars.collect::<CompactString>())
                } else if c2 == 'o' || c2 == 'O' {
                    (8, chars.collect::<CompactString>())
                } else if c2 == 'b' || c2 == 'B' {
                    (2, chars.collect::<CompactString>())
                } else {
                    (
                        10,
                        format_compact!(
                            "{}{}",
                            c2,
                            chars.collect::<CompactString>()
                        ),
                    )
                }
            };
            if let Ok(num) = i64::from_str_radix(&num_str, radix) {
                Ok(if signal % 2 == 1 { -num } else { num })
            } else {
                Err(ErrorKind::InvalidInput)
            }
        } else {
            Ok(0i64)
        }
    } else if c == '`' {
        if let Some(c2) = chars.next() {
            if c2 == '\\' {
                if let (Some(c3), None) = (chars.next(), chars.next()) {
                    Ok(c3 as i64)
                } else {
                    Err(ErrorKind::InvalidInput)
                }
            } else {
                Ok(c2 as i64)
            }
        } else {
            Err(ErrorKind::InvalidInput)
        }
    } else {
        let signal = if signal % 2 == 1 { "-" } else { "" };
        let num_str = format_compact!(
            "{}{}{}",
            signal,
            c,
            chars.collect::<CompactString>()
        );
        if let Ok(num) = i64::from_str_radix(&num_str, 10) {
            Ok(num)
        } else {
            Err(ErrorKind::InvalidInput)
        }
    }
}

pub fn parse_num_range(
    text: &str,
    sep: &str,
) -> Result<RangeItem<i64>, ErrorKind> {
    let mut res = text.rsplitn(2, sep);
    let end = match res.next() {
        Some(end_str) if end_str.is_empty() => Err(ErrorKind::EmptyInput),
        Some(end_str) => parse_num(end_str),
        None => Err(ErrorKind::InvalidInput),
    };
    match end {
        Ok(end) => match res.next() {
            Some(start_str) if start_str.is_empty() => {
                Ok(RangeItem::To(..= end))
            }
            Some(start_str) => match parse_num(start_str) {
                Ok(start) => {
                    if start == end {
                        Ok(RangeItem::Single(start))
                    } else if start < end {
                        Ok(RangeItem::Range(start ..= end))
                    } else {
                        Err(ErrorKind::InvalidInput)
                    }
                }
                Err(_) => Err(ErrorKind::InvalidInput),
            },
            None => Ok(RangeItem::Single(end)),
        },
        Err(end) => match end {
            ErrorKind::EmptyInput => match res.next() {
                Some(start_str) if start_str.is_empty() => {
                    Ok(RangeItem::Full(..))
                }
                Some(start_str) => match parse_num(start_str) {
                    Ok(start) => Ok(RangeItem::From(start ..)),
                    Err(_) => Err(ErrorKind::InvalidInput),
                },
                None => Err(ErrorKind::InvalidInput),
            },
            _ => Err(ErrorKind::InvalidInput),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn number() {
        assert_eq!(parse_num("0xFF"), Ok(0xff_i64));
        assert_eq!(parse_num("0Xfef"), Ok(0xfef_i64));
        assert_eq!(parse_num("+0Xfef"), Ok(0xfef_i64));
        assert_eq!(parse_num("-+--0"), Ok(0i64));
        assert_eq!(parse_num("--+-0x1f"), Ok(-0x1f_i64));
        assert_eq!(parse_num("0xfi"), Err(ErrorKind::InvalidInput));
        assert_eq!(parse_num("Xfi"), Err(ErrorKind::InvalidInput));

        assert_eq!(parse_num("0o127"), Ok(0o127_i64));
        assert_eq!(parse_num("0O127"), Ok(0o127_i64));
        assert_eq!(parse_num("+0o127"), Ok(0o127_i64));
        assert_eq!(parse_num("--+-0o127"), Ok(-0o127_i64));
        assert_eq!(parse_num("0o178"), Err(ErrorKind::InvalidInput));
        assert_eq!(parse_num("0o"), Err(ErrorKind::InvalidInput));

        assert_eq!(parse_num("0b0011"), Ok(0b0011_i64));
        assert_eq!(parse_num("0B0011"), Ok(0b0011_i64));
        assert_eq!(parse_num("+0B0011"), Ok(0b0011_i64));
        assert_eq!(parse_num("--+-0B0011"), Ok(-0b0011_i64));
        assert_eq!(parse_num("0b012"), Err(ErrorKind::InvalidInput));
        assert_eq!(parse_num("0b"), Err(ErrorKind::InvalidInput));

        assert_eq!(parse_num("01236"), Ok(1236_i64));
        assert_eq!(parse_num("---+01236"), Ok(-1236_i64));
        assert_eq!(parse_num("+--++01236"), Ok(1236_i64));
        assert_eq!(parse_num("++=01236"), Err(ErrorKind::InvalidInput));
        assert_eq!(parse_num("0N12"), Err(ErrorKind::InvalidInput));

        assert_eq!(parse_num(r#"`\ "#), Ok(' ' as i64));
        assert_eq!(parse_num(r#"`\\"#), Ok('\\' as i64));
        assert_eq!(parse_num(r#"`\#"#), Ok('#' as i64));
        assert_eq!(parse_num(r#"`\?"#), Ok('?' as i64));
        assert_eq!(parse_num("`好"), Ok('好' as i64));
        assert_eq!(parse_num("`𪊑"), Ok('𪊑' as i64));
        assert_eq!(parse_num("`\u{10C7B7}"), Ok('\u{10C7B7}' as i64));
        assert_eq!(parse_num("`"), Err(ErrorKind::InvalidInput));
        assert_eq!(parse_num(r#"`\"#), Err(ErrorKind::InvalidInput));
        assert_eq!(parse_num(r#"`\@e"#), Err(ErrorKind::InvalidInput));

        assert_eq!(parse_num("-1002"), Ok(-1002_i64));
        assert_eq!(parse_num("-++---+-2001"), Ok(-2001_i64));
        assert_eq!(parse_num("+++20012"), Ok(20012_i64));
        assert_eq!(parse_num("+--+"), Err(ErrorKind::InvalidInput));
        assert_eq!(parse_num("+-=23"), Err(ErrorKind::InvalidInput));

        use RangeItem::*;
        assert_eq!(parse_num_range("", ".."), Err(ErrorKind::InvalidInput));
        assert_eq!(parse_num_range("0", ".."), Ok(Single(0)));
        assert_eq!(parse_num_range("`\\.", ".."), Ok(Single('.' as i64)));
        assert_eq!(
            parse_num_range("`\\...100", ".."),
            Ok(Range('.' as i64 ..= 100))
        );
        assert_eq!(parse_num_range("0..10", ".."), Ok(Range(0 ..= 10)));
        assert_eq!(parse_num_range("..10", ".."), Ok(To(..= 10)));
        assert_eq!(parse_num_range("0..", ".."), Ok(From(0 ..)));
        assert_eq!(parse_num_range("..", ".."), Ok(Full(..)));
        assert_eq!(
            parse_num_range("10..0", ".."),
            Err(ErrorKind::InvalidInput)
        );
        assert_eq!(
            parse_num_range("10..?0", ".."),
            Err(ErrorKind::InvalidInput)
        );
        assert_eq!(
            parse_num_range("?10..0", ".."),
            Err(ErrorKind::InvalidInput)
        );
        assert_eq!(
            parse_num_range("?10..?0", ".."),
            Err(ErrorKind::InvalidInput)
        );
    }
}
