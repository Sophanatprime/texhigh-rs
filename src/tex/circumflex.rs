use std::str::Chars;

use crate::types::CatCodeGetter;

/// Parse `^^c`, `^^hh`, `^^^^hhhh`, and `^^^^^^hhhhhh`.
///
/// Panic if `hhhh` or `hhhhhh` is not a valid Unicode scalar value.
pub fn circumflex_mechanism<'c, C: CatCodeGetter>(
    catcode: &C,
    chars: Chars<'c>,
    chr: char,
) -> (char, usize) {
    if chr.is_ascii() {
        return circumflex_ascii(catcode, chars.as_str(), chr as u8);
    }
    let advance = 0;
    //TODO: Allow non-ASCII value
    (chr, advance)
}


enum CircumflexKind {
    None,
    Char,
    TwoHex,
    FourHex,
    SixHex,
}

fn circumflex_ascii<'c, C: CatCodeGetter>(
    catcode: &C,
    chars: &'c str,
    chr: u8,
) -> (char, usize) {
    let _ = catcode;

    #[inline]
    fn is_cir_hex(v: u8) -> bool {
        matches!(v, b'0'..=b'9' | b'a'..=b'f')
    }

    if chars.is_empty() {
        return (chr as char, 0);
    }

    let bytes = chars.as_bytes();
    let cir_kind = match bytes.iter().take(6).position(|v| *v != chr) {
        Some(0) => CircumflexKind::None,
        Some(1) | Some(2) => {
            if chars.len() >= 3 && bytes[1 .. 3].iter().all(|v| is_cir_hex(*v))
            {
                CircumflexKind::TwoHex
            } else {
                CircumflexKind::Char
            }
        }
        Some(3) | Some(4) => {
            if chars.len() >= 7 && bytes[3 .. 7].iter().all(|v| is_cir_hex(*v))
            {
                CircumflexKind::FourHex
            } else {
                CircumflexKind::Char
            }
        }
        _ => {
            if chars.len() >= 11
                && bytes[5 .. 11].iter().all(|v| is_cir_hex(*v))
            {
                CircumflexKind::SixHex
            } else {
                CircumflexKind::Char
            }
        }
    };

    match cir_kind {
        CircumflexKind::None => (chr as char, 0),
        CircumflexKind::Char => match *unsafe { bytes.get_unchecked(1) } {
            chr @ 0 ..= 63 => ((chr + 64) as char, 2),
            chr @ 64 ..= 127 => ((chr - 64) as char, 2),
            _ => (chr as char, 0),
        },
        CircumflexKind::TwoHex => {
            let hex_s = unsafe {
                str::from_utf8_unchecked(bytes.get_unchecked(1 .. 3))
            };
            let num = u32::from_str_radix(hex_s, 16).unwrap();
            let chr = char::from_u32(num).unwrap();
            (chr, 3)
        }
        CircumflexKind::FourHex => {
            let hex_s = unsafe {
                str::from_utf8_unchecked(bytes.get_unchecked(3 .. 7))
            };
            let num = u32::from_str_radix(hex_s, 16).unwrap();
            let chr = char::from_u32(num).unwrap();
            (chr, 7)
        }
        CircumflexKind::SixHex => {
            let hex_s = unsafe {
                str::from_utf8_unchecked(bytes.get_unchecked(5 .. 11))
            };
            let num = u32::from_str_radix(hex_s, 16).unwrap();
            let chr = char::from_u32(num).unwrap();
            (chr, 11)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::CTab;

    #[test]
    fn hex_hh() {
        let mut chars = r"^^65lax".chars();
        let chr = chars.next().unwrap();
        let (new_chr, advance) =
            circumflex_mechanism(&CTab::document(), chars, chr);
        assert_eq!(advance, 3);
        assert_eq!(new_chr, '\x65');
    }

    #[test]
    fn hex_c() {
        let mut chars = r"^^Ilax".chars();
        let chr = chars.next().unwrap();
        let (new_chr, advance) =
            circumflex_mechanism(&CTab::document(), chars, chr);
        assert_eq!(advance, 2);
        assert_eq!(new_chr, '\t');

        let mut chars = r"^^elax".chars();
        let chr = chars.next().unwrap();
        let (new_chr, advance) =
            circumflex_mechanism(&CTab::document(), chars, chr);
        assert_eq!(advance, 2);
        assert_eq!(new_chr, '%');

        let mut chars = r"^^^lax".chars();
        let chr = chars.next().unwrap();
        let (new_chr, advance) =
            circumflex_mechanism(&CTab::document(), chars, chr);
        assert_eq!(advance, 2);
        assert_eq!(new_chr, '\u{1e}');

        let mut chars = r"^^^^^^lax".chars();
        let chr = chars.next().unwrap();
        let (new_chr, advance) =
            circumflex_mechanism(&CTab::document(), chars, chr);
        assert_eq!(advance, 2);
        assert_eq!(new_chr, '\u{1e}');
    }

    #[test]
    fn hex_hhhh() {
        let mut chars = r"^^^^6500lax".chars();
        let chr = chars.next().unwrap();
        let (new_chr, advance) =
            circumflex_mechanism(&CTab::document(), chars, chr);
        assert_eq!(advance, 7);
        assert_eq!(new_chr, '\u{6500}');
    }

    #[test]
    fn hex_hhhhhh() {
        let mut chars = r"^^^^^^01f004lax".chars();
        let chr = chars.next().unwrap();
        let (new_chr, advance) =
            circumflex_mechanism(&CTab::document(), chars, chr);
        assert_eq!(advance, 11);
        assert_eq!(new_chr, '\u{1f004}');
    }
}
