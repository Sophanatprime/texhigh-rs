use crate::range::NumberSpan;
use lazy_static::lazy_static;
use std::collections::HashMap;
pub use ub::UnicodeBlock;
use yeslogic_unicode_blocks as ub;

#[allow(dead_code)]
pub const VERSION: &'static str = ub::VERSION;
pub fn get_char_range_from_block_name(name: &str) -> Option<NumberSpan<char>> {
    match UNICODE_BLOCKS.get(&name) {
        Some(&rng) => {
            let chr_start = char::from_u32(rng.start())?;
            let chr_end = char::from_u32(rng.end())?;
            Some((chr_start..=chr_end).into())
        }
        None => None,
    }
}
pub fn get_char_block_name(c: u32) -> Option<&'static str> {
    if matches!(c, 0xD800..=0xDB7F) {
        Some("High Surrogates")
    } else if matches!(c, 0xDB80..=0xDBFF) {
        Some("High Private Use Surrogates")
    } else if matches!(c, 0xDC00..=0xDFFF) {
        Some("Low Surrogates")
    } else if c > 0x10FFFF {
        None
    } else {
        match &ub::find_unicode_block(unsafe { char::from_u32_unchecked(c) }) {
            Some(block) => Some(block.name()),
            None => Some("No Block"),
        }
    }
}

#[rustfmt::skip]
pub const fn get_cjk_ideographs_blocks() -> &'static [&'static UnicodeBlock] { CJK_IDEOGRAPHS }

#[rustfmt::skip]
const CJK_IDEOGRAPHS: &'static [&UnicodeBlock; 12] = &[
    &ub::CJK_UNIFIED_IDEOGRAPHS,
    &ub::CJK_UNIFIED_IDEOGRAPHS_EXTENSION_A,
    &ub::CJK_UNIFIED_IDEOGRAPHS_EXTENSION_B,
    &ub::CJK_UNIFIED_IDEOGRAPHS_EXTENSION_C,
    &ub::CJK_UNIFIED_IDEOGRAPHS_EXTENSION_D,
    &ub::CJK_UNIFIED_IDEOGRAPHS_EXTENSION_E,
    &ub::CJK_UNIFIED_IDEOGRAPHS_EXTENSION_F,
    &ub::CJK_UNIFIED_IDEOGRAPHS_EXTENSION_G,
    &ub::CJK_UNIFIED_IDEOGRAPHS_EXTENSION_H,
    &ub::CJK_UNIFIED_IDEOGRAPHS_EXTENSION_I,
    &ub::CJK_COMPATIBILITY_IDEOGRAPHS,
    &ub::CJK_COMPATIBILITY_IDEOGRAPHS_SUPPLEMENT
];

#[rustfmt::skip]
lazy_static! {
    pub static ref UNICODE_BLOCKS: HashMap<&'static str, &'static UnicodeBlock> = HashMap::from_iter([
        ("BASIC_LATIN", &ub::BASIC_LATIN),
        ("LATIN_1_SUPPLEMENT", &ub::LATIN_1_SUPPLEMENT),
        ("LATIN_EXTENDED_A", &ub::LATIN_EXTENDED_A),
        ("LATIN_EXTENDED_B", &ub::LATIN_EXTENDED_B),
        ("IPA_EXTENSIONS", &ub::IPA_EXTENSIONS),
        ("SPACING_MODIFIER_LETTERS", &ub::SPACING_MODIFIER_LETTERS),
        ("COMBINING_DIACRITICAL_MARKS", &ub::COMBINING_DIACRITICAL_MARKS),
        ("GREEK_AND_COPTIC", &ub::GREEK_AND_COPTIC),
        ("CYRILLIC", &ub::CYRILLIC),
        ("CYRILLIC_SUPPLEMENT", &ub::CYRILLIC_SUPPLEMENT),
        ("ARMENIAN", &ub::ARMENIAN),
        ("HEBREW", &ub::HEBREW),
        ("ARABIC", &ub::ARABIC),
        ("SYRIAC", &ub::SYRIAC),
        ("ARABIC_SUPPLEMENT", &ub::ARABIC_SUPPLEMENT),
        ("THAANA", &ub::THAANA),
        ("NKO", &ub::NKO),
        ("SAMARITAN", &ub::SAMARITAN),
        ("MANDAIC", &ub::MANDAIC),
        ("SYRIAC_SUPPLEMENT", &ub::SYRIAC_SUPPLEMENT),
        ("ARABIC_EXTENDED_B", &ub::ARABIC_EXTENDED_B),
        ("ARABIC_EXTENDED_A", &ub::ARABIC_EXTENDED_A),
        ("DEVANAGARI", &ub::DEVANAGARI),
        ("BENGALI", &ub::BENGALI),
        ("GURMUKHI", &ub::GURMUKHI),
        ("GUJARATI", &ub::GUJARATI),
        ("ORIYA", &ub::ORIYA),
        ("TAMIL", &ub::TAMIL),
        ("TELUGU", &ub::TELUGU),
        ("KANNADA", &ub::KANNADA),
        ("MALAYALAM", &ub::MALAYALAM),
        ("SINHALA", &ub::SINHALA),
        ("THAI", &ub::THAI),
        ("LAO", &ub::LAO),
        ("TIBETAN", &ub::TIBETAN),
        ("MYANMAR", &ub::MYANMAR),
        ("GEORGIAN", &ub::GEORGIAN),
        ("HANGUL_JAMO", &ub::HANGUL_JAMO),
        ("ETHIOPIC", &ub::ETHIOPIC),
        ("ETHIOPIC_SUPPLEMENT", &ub::ETHIOPIC_SUPPLEMENT),
        ("CHEROKEE", &ub::CHEROKEE),
        ("UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS", &ub::UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS),
        ("OGHAM", &ub::OGHAM),
        ("RUNIC", &ub::RUNIC),
        ("TAGALOG", &ub::TAGALOG),
        ("HANUNOO", &ub::HANUNOO),
        ("BUHID", &ub::BUHID),
        ("TAGBANWA", &ub::TAGBANWA),
        ("KHMER", &ub::KHMER),
        ("MONGOLIAN", &ub::MONGOLIAN),
        ("UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS_EXTENDED", &ub::UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS_EXTENDED),
        ("LIMBU", &ub::LIMBU),
        ("TAI_LE", &ub::TAI_LE),
        ("NEW_TAI_LUE", &ub::NEW_TAI_LUE),
        ("KHMER_SYMBOLS", &ub::KHMER_SYMBOLS),
        ("BUGINESE", &ub::BUGINESE),
        ("TAI_THAM", &ub::TAI_THAM),
        ("COMBINING_DIACRITICAL_MARKS_EXTENDED", &ub::COMBINING_DIACRITICAL_MARKS_EXTENDED),
        ("BALINESE", &ub::BALINESE),
        ("SUNDANESE", &ub::SUNDANESE),
        ("BATAK", &ub::BATAK),
        ("LEPCHA", &ub::LEPCHA),
        ("OL_CHIKI", &ub::OL_CHIKI),
        ("CYRILLIC_EXTENDED_C", &ub::CYRILLIC_EXTENDED_C),
        ("GEORGIAN_EXTENDED", &ub::GEORGIAN_EXTENDED),
        ("SUNDANESE_SUPPLEMENT", &ub::SUNDANESE_SUPPLEMENT),
        ("VEDIC_EXTENSIONS", &ub::VEDIC_EXTENSIONS),
        ("PHONETIC_EXTENSIONS", &ub::PHONETIC_EXTENSIONS),
        ("PHONETIC_EXTENSIONS_SUPPLEMENT", &ub::PHONETIC_EXTENSIONS_SUPPLEMENT),
        ("COMBINING_DIACRITICAL_MARKS_SUPPLEMENT", &ub::COMBINING_DIACRITICAL_MARKS_SUPPLEMENT),
        ("LATIN_EXTENDED_ADDITIONAL", &ub::LATIN_EXTENDED_ADDITIONAL),
        ("GREEK_EXTENDED", &ub::GREEK_EXTENDED),
        ("GENERAL_PUNCTUATION", &ub::GENERAL_PUNCTUATION),
        ("SUPERSCRIPTS_AND_SUBSCRIPTS", &ub::SUPERSCRIPTS_AND_SUBSCRIPTS),
        ("CURRENCY_SYMBOLS", &ub::CURRENCY_SYMBOLS),
        ("COMBINING_DIACRITICAL_MARKS_FOR_SYMBOLS", &ub::COMBINING_DIACRITICAL_MARKS_FOR_SYMBOLS),
        ("LETTERLIKE_SYMBOLS", &ub::LETTERLIKE_SYMBOLS),
        ("NUMBER_FORMS", &ub::NUMBER_FORMS),
        ("ARROWS", &ub::ARROWS),
        ("MATHEMATICAL_OPERATORS", &ub::MATHEMATICAL_OPERATORS),
        ("MISCELLANEOUS_TECHNICAL", &ub::MISCELLANEOUS_TECHNICAL),
        ("CONTROL_PICTURES", &ub::CONTROL_PICTURES),
        ("OPTICAL_CHARACTER_RECOGNITION", &ub::OPTICAL_CHARACTER_RECOGNITION),
        ("ENCLOSED_ALPHANUMERICS", &ub::ENCLOSED_ALPHANUMERICS),
        ("BOX_DRAWING", &ub::BOX_DRAWING),
        ("BLOCK_ELEMENTS", &ub::BLOCK_ELEMENTS),
        ("GEOMETRIC_SHAPES", &ub::GEOMETRIC_SHAPES),
        ("MISCELLANEOUS_SYMBOLS", &ub::MISCELLANEOUS_SYMBOLS),
        ("DINGBATS", &ub::DINGBATS),
        ("MISCELLANEOUS_MATHEMATICAL_SYMBOLS_A", &ub::MISCELLANEOUS_MATHEMATICAL_SYMBOLS_A),
        ("SUPPLEMENTAL_ARROWS_A", &ub::SUPPLEMENTAL_ARROWS_A),
        ("BRAILLE_PATTERNS", &ub::BRAILLE_PATTERNS),
        ("SUPPLEMENTAL_ARROWS_B", &ub::SUPPLEMENTAL_ARROWS_B),
        ("MISCELLANEOUS_MATHEMATICAL_SYMBOLS_B", &ub::MISCELLANEOUS_MATHEMATICAL_SYMBOLS_B),
        ("SUPPLEMENTAL_MATHEMATICAL_OPERATORS", &ub::SUPPLEMENTAL_MATHEMATICAL_OPERATORS),
        ("MISCELLANEOUS_SYMBOLS_AND_ARROWS", &ub::MISCELLANEOUS_SYMBOLS_AND_ARROWS),
        ("GLAGOLITIC", &ub::GLAGOLITIC),
        ("LATIN_EXTENDED_C", &ub::LATIN_EXTENDED_C),
        ("COPTIC", &ub::COPTIC),
        ("GEORGIAN_SUPPLEMENT", &ub::GEORGIAN_SUPPLEMENT),
        ("TIFINAGH", &ub::TIFINAGH),
        ("ETHIOPIC_EXTENDED", &ub::ETHIOPIC_EXTENDED),
        ("CYRILLIC_EXTENDED_A", &ub::CYRILLIC_EXTENDED_A),
        ("SUPPLEMENTAL_PUNCTUATION", &ub::SUPPLEMENTAL_PUNCTUATION),
        ("CJK_RADICALS_SUPPLEMENT", &ub::CJK_RADICALS_SUPPLEMENT),
        ("KANGXI_RADICALS", &ub::KANGXI_RADICALS),
        ("IDEOGRAPHIC_DESCRIPTION_CHARACTERS", &ub::IDEOGRAPHIC_DESCRIPTION_CHARACTERS),
        ("CJK_SYMBOLS_AND_PUNCTUATION", &ub::CJK_SYMBOLS_AND_PUNCTUATION),
        ("HIRAGANA", &ub::HIRAGANA),
        ("KATAKANA", &ub::KATAKANA),
        ("BOPOMOFO", &ub::BOPOMOFO),
        ("HANGUL_COMPATIBILITY_JAMO", &ub::HANGUL_COMPATIBILITY_JAMO),
        ("KANBUN", &ub::KANBUN),
        ("BOPOMOFO_EXTENDED", &ub::BOPOMOFO_EXTENDED),
        ("CJK_STROKES", &ub::CJK_STROKES),
        ("KATAKANA_PHONETIC_EXTENSIONS", &ub::KATAKANA_PHONETIC_EXTENSIONS),
        ("ENCLOSED_CJK_LETTERS_AND_MONTHS", &ub::ENCLOSED_CJK_LETTERS_AND_MONTHS),
        ("CJK_COMPATIBILITY", &ub::CJK_COMPATIBILITY),
        ("CJK_UNIFIED_IDEOGRAPHS_EXTENSION_A", &ub::CJK_UNIFIED_IDEOGRAPHS_EXTENSION_A),
        ("YIJING_HEXAGRAM_SYMBOLS", &ub::YIJING_HEXAGRAM_SYMBOLS),
        ("CJK_UNIFIED_IDEOGRAPHS", &ub::CJK_UNIFIED_IDEOGRAPHS),
        ("YI_SYLLABLES", &ub::YI_SYLLABLES),
        ("YI_RADICALS", &ub::YI_RADICALS),
        ("LISU", &ub::LISU),
        ("VAI", &ub::VAI),
        ("CYRILLIC_EXTENDED_B", &ub::CYRILLIC_EXTENDED_B),
        ("BAMUM", &ub::BAMUM),
        ("MODIFIER_TONE_LETTERS", &ub::MODIFIER_TONE_LETTERS),
        ("LATIN_EXTENDED_D", &ub::LATIN_EXTENDED_D),
        ("SYLOTI_NAGRI", &ub::SYLOTI_NAGRI),
        ("COMMON_INDIC_NUMBER_FORMS", &ub::COMMON_INDIC_NUMBER_FORMS),
        ("PHAGS_PA", &ub::PHAGS_PA),
        ("SAURASHTRA", &ub::SAURASHTRA),
        ("DEVANAGARI_EXTENDED", &ub::DEVANAGARI_EXTENDED),
        ("KAYAH_LI", &ub::KAYAH_LI),
        ("REJANG", &ub::REJANG),
        ("HANGUL_JAMO_EXTENDED_A", &ub::HANGUL_JAMO_EXTENDED_A),
        ("JAVANESE", &ub::JAVANESE),
        ("MYANMAR_EXTENDED_B", &ub::MYANMAR_EXTENDED_B),
        ("CHAM", &ub::CHAM),
        ("MYANMAR_EXTENDED_A", &ub::MYANMAR_EXTENDED_A),
        ("TAI_VIET", &ub::TAI_VIET),
        ("MEETEI_MAYEK_EXTENSIONS", &ub::MEETEI_MAYEK_EXTENSIONS),
        ("ETHIOPIC_EXTENDED_A", &ub::ETHIOPIC_EXTENDED_A),
        ("LATIN_EXTENDED_E", &ub::LATIN_EXTENDED_E),
        ("CHEROKEE_SUPPLEMENT", &ub::CHEROKEE_SUPPLEMENT),
        ("MEETEI_MAYEK", &ub::MEETEI_MAYEK),
        ("HANGUL_SYLLABLES", &ub::HANGUL_SYLLABLES),
        ("HANGUL_JAMO_EXTENDED_B", &ub::HANGUL_JAMO_EXTENDED_B),
        ("HIGH_SURROGATES", &ub::HIGH_SURROGATES),
        ("HIGH_PRIVATE_USE_SURROGATES", &ub::HIGH_PRIVATE_USE_SURROGATES),
        ("LOW_SURROGATES", &ub::LOW_SURROGATES),
        ("PRIVATE_USE_AREA", &ub::PRIVATE_USE_AREA),
        ("CJK_COMPATIBILITY_IDEOGRAPHS", &ub::CJK_COMPATIBILITY_IDEOGRAPHS),
        ("ALPHABETIC_PRESENTATION_FORMS", &ub::ALPHABETIC_PRESENTATION_FORMS),
        ("ARABIC_PRESENTATION_FORMS_A", &ub::ARABIC_PRESENTATION_FORMS_A),
        ("VARIATION_SELECTORS", &ub::VARIATION_SELECTORS),
        ("VERTICAL_FORMS", &ub::VERTICAL_FORMS),
        ("COMBINING_HALF_MARKS", &ub::COMBINING_HALF_MARKS),
        ("CJK_COMPATIBILITY_FORMS", &ub::CJK_COMPATIBILITY_FORMS),
        ("SMALL_FORM_VARIANTS", &ub::SMALL_FORM_VARIANTS),
        ("ARABIC_PRESENTATION_FORMS_B", &ub::ARABIC_PRESENTATION_FORMS_B),
        ("HALFWIDTH_AND_FULLWIDTH_FORMS", &ub::HALFWIDTH_AND_FULLWIDTH_FORMS),
        ("SPECIALS", &ub::SPECIALS),
        ("LINEAR_B_SYLLABARY", &ub::LINEAR_B_SYLLABARY),
        ("LINEAR_B_IDEOGRAMS", &ub::LINEAR_B_IDEOGRAMS),
        ("AEGEAN_NUMBERS", &ub::AEGEAN_NUMBERS),
        ("ANCIENT_GREEK_NUMBERS", &ub::ANCIENT_GREEK_NUMBERS),
        ("ANCIENT_SYMBOLS", &ub::ANCIENT_SYMBOLS),
        ("PHAISTOS_DISC", &ub::PHAISTOS_DISC),
        ("LYCIAN", &ub::LYCIAN),
        ("CARIAN", &ub::CARIAN),
        ("COPTIC_EPACT_NUMBERS", &ub::COPTIC_EPACT_NUMBERS),
        ("OLD_ITALIC", &ub::OLD_ITALIC),
        ("GOTHIC", &ub::GOTHIC),
        ("OLD_PERMIC", &ub::OLD_PERMIC),
        ("UGARITIC", &ub::UGARITIC),
        ("OLD_PERSIAN", &ub::OLD_PERSIAN),
        ("DESERET", &ub::DESERET),
        ("SHAVIAN", &ub::SHAVIAN),
        ("OSMANYA", &ub::OSMANYA),
        ("OSAGE", &ub::OSAGE),
        ("ELBASAN", &ub::ELBASAN),
        ("CAUCASIAN_ALBANIAN", &ub::CAUCASIAN_ALBANIAN),
        ("VITHKUQI", &ub::VITHKUQI),
        ("TODHRI", &ub::TODHRI),
        ("LINEAR_A", &ub::LINEAR_A),
        ("LATIN_EXTENDED_F", &ub::LATIN_EXTENDED_F),
        ("CYPRIOT_SYLLABARY", &ub::CYPRIOT_SYLLABARY),
        ("IMPERIAL_ARAMAIC", &ub::IMPERIAL_ARAMAIC),
        ("PALMYRENE", &ub::PALMYRENE),
        ("NABATAEAN", &ub::NABATAEAN),
        ("HATRAN", &ub::HATRAN),
        ("PHOENICIAN", &ub::PHOENICIAN),
        ("LYDIAN", &ub::LYDIAN),
        ("MEROITIC_HIEROGLYPHS", &ub::MEROITIC_HIEROGLYPHS),
        ("MEROITIC_CURSIVE", &ub::MEROITIC_CURSIVE),
        ("KHAROSHTHI", &ub::KHAROSHTHI),
        ("OLD_SOUTH_ARABIAN", &ub::OLD_SOUTH_ARABIAN),
        ("OLD_NORTH_ARABIAN", &ub::OLD_NORTH_ARABIAN),
        ("MANICHAEAN", &ub::MANICHAEAN),
        ("AVESTAN", &ub::AVESTAN),
        ("INSCRIPTIONAL_PARTHIAN", &ub::INSCRIPTIONAL_PARTHIAN),
        ("INSCRIPTIONAL_PAHLAVI", &ub::INSCRIPTIONAL_PAHLAVI),
        ("PSALTER_PAHLAVI", &ub::PSALTER_PAHLAVI),
        ("OLD_TURKIC", &ub::OLD_TURKIC),
        ("OLD_HUNGARIAN", &ub::OLD_HUNGARIAN),
        ("HANIFI_ROHINGYA", &ub::HANIFI_ROHINGYA),
        ("GARAY", &ub::GARAY),
        ("RUMI_NUMERAL_SYMBOLS", &ub::RUMI_NUMERAL_SYMBOLS),
        ("YEZIDI", &ub::YEZIDI),
        ("ARABIC_EXTENDED_C", &ub::ARABIC_EXTENDED_C),
        ("OLD_SOGDIAN", &ub::OLD_SOGDIAN),
        ("SOGDIAN", &ub::SOGDIAN),
        ("OLD_UYGHUR", &ub::OLD_UYGHUR),
        ("CHORASMIAN", &ub::CHORASMIAN),
        ("ELYMAIC", &ub::ELYMAIC),
        ("BRAHMI", &ub::BRAHMI),
        ("KAITHI", &ub::KAITHI),
        ("SORA_SOMPENG", &ub::SORA_SOMPENG),
        ("CHAKMA", &ub::CHAKMA),
        ("MAHAJANI", &ub::MAHAJANI),
        ("SHARADA", &ub::SHARADA),
        ("SINHALA_ARCHAIC_NUMBERS", &ub::SINHALA_ARCHAIC_NUMBERS),
        ("KHOJKI", &ub::KHOJKI),
        ("MULTANI", &ub::MULTANI),
        ("KHUDAWADI", &ub::KHUDAWADI),
        ("GRANTHA", &ub::GRANTHA),
        ("TULU_TIGALARI", &ub::TULU_TIGALARI),
        ("NEWA", &ub::NEWA),
        ("TIRHUTA", &ub::TIRHUTA),
        ("SIDDHAM", &ub::SIDDHAM),
        ("MODI", &ub::MODI),
        ("MONGOLIAN_SUPPLEMENT", &ub::MONGOLIAN_SUPPLEMENT),
        ("TAKRI", &ub::TAKRI),
        ("MYANMAR_EXTENDED_C", &ub::MYANMAR_EXTENDED_C),
        ("AHOM", &ub::AHOM),
        ("DOGRA", &ub::DOGRA),
        ("WARANG_CITI", &ub::WARANG_CITI),
        ("DIVES_AKURU", &ub::DIVES_AKURU),
        ("NANDINAGARI", &ub::NANDINAGARI),
        ("ZANABAZAR_SQUARE", &ub::ZANABAZAR_SQUARE),
        ("SOYOMBO", &ub::SOYOMBO),
        ("UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS_EXTENDED_A", &ub::UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS_EXTENDED_A),
        ("PAU_CIN_HAU", &ub::PAU_CIN_HAU),
        ("DEVANAGARI_EXTENDED_A", &ub::DEVANAGARI_EXTENDED_A),
        ("SUNUWAR", &ub::SUNUWAR),
        ("BHAIKSUKI", &ub::BHAIKSUKI),
        ("MARCHEN", &ub::MARCHEN),
        ("MASARAM_GONDI", &ub::MASARAM_GONDI),
        ("GUNJALA_GONDI", &ub::GUNJALA_GONDI),
        ("MAKASAR", &ub::MAKASAR),
        ("KAWI", &ub::KAWI),
        ("LISU_SUPPLEMENT", &ub::LISU_SUPPLEMENT),
        ("TAMIL_SUPPLEMENT", &ub::TAMIL_SUPPLEMENT),
        ("CUNEIFORM", &ub::CUNEIFORM),
        ("CUNEIFORM_NUMBERS_AND_PUNCTUATION", &ub::CUNEIFORM_NUMBERS_AND_PUNCTUATION),
        ("EARLY_DYNASTIC_CUNEIFORM", &ub::EARLY_DYNASTIC_CUNEIFORM),
        ("CYPRO_MINOAN", &ub::CYPRO_MINOAN),
        ("EGYPTIAN_HIEROGLYPHS", &ub::EGYPTIAN_HIEROGLYPHS),
        ("EGYPTIAN_HIEROGLYPH_FORMAT_CONTROLS", &ub::EGYPTIAN_HIEROGLYPH_FORMAT_CONTROLS),
        ("EGYPTIAN_HIEROGLYPHS_EXTENDED_A", &ub::EGYPTIAN_HIEROGLYPHS_EXTENDED_A),
        ("ANATOLIAN_HIEROGLYPHS", &ub::ANATOLIAN_HIEROGLYPHS),
        ("GURUNG_KHEMA", &ub::GURUNG_KHEMA),
        ("BAMUM_SUPPLEMENT", &ub::BAMUM_SUPPLEMENT),
        ("MRO", &ub::MRO),
        ("TANGSA", &ub::TANGSA),
        ("BASSA_VAH", &ub::BASSA_VAH),
        ("PAHAWH_HMONG", &ub::PAHAWH_HMONG),
        ("KIRAT_RAI", &ub::KIRAT_RAI),
        ("MEDEFAIDRIN", &ub::MEDEFAIDRIN),
        ("MIAO", &ub::MIAO),
        ("IDEOGRAPHIC_SYMBOLS_AND_PUNCTUATION", &ub::IDEOGRAPHIC_SYMBOLS_AND_PUNCTUATION),
        ("TANGUT", &ub::TANGUT),
        ("TANGUT_COMPONENTS", &ub::TANGUT_COMPONENTS),
        ("KHITAN_SMALL_SCRIPT", &ub::KHITAN_SMALL_SCRIPT),
        ("TANGUT_SUPPLEMENT", &ub::TANGUT_SUPPLEMENT),
        ("KANA_EXTENDED_B", &ub::KANA_EXTENDED_B),
        ("KANA_SUPPLEMENT", &ub::KANA_SUPPLEMENT),
        ("KANA_EXTENDED_A", &ub::KANA_EXTENDED_A),
        ("SMALL_KANA_EXTENSION", &ub::SMALL_KANA_EXTENSION),
        ("NUSHU", &ub::NUSHU),
        ("DUPLOYAN", &ub::DUPLOYAN),
        ("SHORTHAND_FORMAT_CONTROLS", &ub::SHORTHAND_FORMAT_CONTROLS),
        ("SYMBOLS_FOR_LEGACY_COMPUTING_SUPPLEMENT", &ub::SYMBOLS_FOR_LEGACY_COMPUTING_SUPPLEMENT),
        ("ZNAMENNY_MUSICAL_NOTATION", &ub::ZNAMENNY_MUSICAL_NOTATION),
        ("BYZANTINE_MUSICAL_SYMBOLS", &ub::BYZANTINE_MUSICAL_SYMBOLS),
        ("MUSICAL_SYMBOLS", &ub::MUSICAL_SYMBOLS),
        ("ANCIENT_GREEK_MUSICAL_NOTATION", &ub::ANCIENT_GREEK_MUSICAL_NOTATION),
        ("KAKTOVIK_NUMERALS", &ub::KAKTOVIK_NUMERALS),
        ("MAYAN_NUMERALS", &ub::MAYAN_NUMERALS),
        ("TAI_XUAN_JING_SYMBOLS", &ub::TAI_XUAN_JING_SYMBOLS),
        ("COUNTING_ROD_NUMERALS", &ub::COUNTING_ROD_NUMERALS),
        ("MATHEMATICAL_ALPHANUMERIC_SYMBOLS", &ub::MATHEMATICAL_ALPHANUMERIC_SYMBOLS),
        ("SUTTON_SIGNWRITING", &ub::SUTTON_SIGNWRITING),
        ("LATIN_EXTENDED_G", &ub::LATIN_EXTENDED_G),
        ("GLAGOLITIC_SUPPLEMENT", &ub::GLAGOLITIC_SUPPLEMENT),
        ("CYRILLIC_EXTENDED_D", &ub::CYRILLIC_EXTENDED_D),
        ("NYIAKENG_PUACHUE_HMONG", &ub::NYIAKENG_PUACHUE_HMONG),
        ("TOTO", &ub::TOTO),
        ("WANCHO", &ub::WANCHO),
        ("NAG_MUNDARI", &ub::NAG_MUNDARI),
        ("OL_ONAL", &ub::OL_ONAL),
        ("ETHIOPIC_EXTENDED_B", &ub::ETHIOPIC_EXTENDED_B),
        ("MENDE_KIKAKUI", &ub::MENDE_KIKAKUI),
        ("ADLAM", &ub::ADLAM),
        ("INDIC_SIYAQ_NUMBERS", &ub::INDIC_SIYAQ_NUMBERS),
        ("OTTOMAN_SIYAQ_NUMBERS", &ub::OTTOMAN_SIYAQ_NUMBERS),
        ("ARABIC_MATHEMATICAL_ALPHABETIC_SYMBOLS", &ub::ARABIC_MATHEMATICAL_ALPHABETIC_SYMBOLS),
        ("MAHJONG_TILES", &ub::MAHJONG_TILES),
        ("DOMINO_TILES", &ub::DOMINO_TILES),
        ("PLAYING_CARDS", &ub::PLAYING_CARDS),
        ("ENCLOSED_ALPHANUMERIC_SUPPLEMENT", &ub::ENCLOSED_ALPHANUMERIC_SUPPLEMENT),
        ("ENCLOSED_IDEOGRAPHIC_SUPPLEMENT", &ub::ENCLOSED_IDEOGRAPHIC_SUPPLEMENT),
        ("MISCELLANEOUS_SYMBOLS_AND_PICTOGRAPHS", &ub::MISCELLANEOUS_SYMBOLS_AND_PICTOGRAPHS),
        ("EMOTICONS", &ub::EMOTICONS),
        ("ORNAMENTAL_DINGBATS", &ub::ORNAMENTAL_DINGBATS),
        ("TRANSPORT_AND_MAP_SYMBOLS", &ub::TRANSPORT_AND_MAP_SYMBOLS),
        ("ALCHEMICAL_SYMBOLS", &ub::ALCHEMICAL_SYMBOLS),
        ("GEOMETRIC_SHAPES_EXTENDED", &ub::GEOMETRIC_SHAPES_EXTENDED),
        ("SUPPLEMENTAL_ARROWS_C", &ub::SUPPLEMENTAL_ARROWS_C),
        ("SUPPLEMENTAL_SYMBOLS_AND_PICTOGRAPHS", &ub::SUPPLEMENTAL_SYMBOLS_AND_PICTOGRAPHS),
        ("CHESS_SYMBOLS", &ub::CHESS_SYMBOLS),
        ("SYMBOLS_AND_PICTOGRAPHS_EXTENDED_A", &ub::SYMBOLS_AND_PICTOGRAPHS_EXTENDED_A),
        ("SYMBOLS_FOR_LEGACY_COMPUTING", &ub::SYMBOLS_FOR_LEGACY_COMPUTING),
        ("CJK_UNIFIED_IDEOGRAPHS_EXTENSION_B", &ub::CJK_UNIFIED_IDEOGRAPHS_EXTENSION_B),
        ("CJK_UNIFIED_IDEOGRAPHS_EXTENSION_C", &ub::CJK_UNIFIED_IDEOGRAPHS_EXTENSION_C),
        ("CJK_UNIFIED_IDEOGRAPHS_EXTENSION_D", &ub::CJK_UNIFIED_IDEOGRAPHS_EXTENSION_D),
        ("CJK_UNIFIED_IDEOGRAPHS_EXTENSION_E", &ub::CJK_UNIFIED_IDEOGRAPHS_EXTENSION_E),
        ("CJK_UNIFIED_IDEOGRAPHS_EXTENSION_F", &ub::CJK_UNIFIED_IDEOGRAPHS_EXTENSION_F),
        ("CJK_UNIFIED_IDEOGRAPHS_EXTENSION_I", &ub::CJK_UNIFIED_IDEOGRAPHS_EXTENSION_I),
        ("CJK_COMPATIBILITY_IDEOGRAPHS_SUPPLEMENT", &ub::CJK_COMPATIBILITY_IDEOGRAPHS_SUPPLEMENT),
        ("CJK_UNIFIED_IDEOGRAPHS_EXTENSION_G", &ub::CJK_UNIFIED_IDEOGRAPHS_EXTENSION_G),
        ("CJK_UNIFIED_IDEOGRAPHS_EXTENSION_H", &ub::CJK_UNIFIED_IDEOGRAPHS_EXTENSION_H),
        ("TAGS", &ub::TAGS),
        ("VARIATION_SELECTORS_SUPPLEMENT", &ub::VARIATION_SELECTORS_SUPPLEMENT),
        ("SUPPLEMENTARY_PRIVATE_USE_AREA_A", &ub::SUPPLEMENTARY_PRIVATE_USE_AREA_A),
        ("SUPPLEMENTARY_PRIVATE_USE_AREA_B", &ub::SUPPLEMENTARY_PRIVATE_USE_AREA_B),
    ]);
}
