/* unicode/mod.rs
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

use crate::range::NumberSpan;
use lazy_static::lazy_static;
use std::collections::HashMap;

pub mod unicode_block;
pub mod unicode_blocks;
use unicode_blocks as ub;

pub use unicode_block::UnicodeBlock;
pub use unicode_blocks::find_unicode_block;
pub use unicode_linebreak as linebreak;
pub use unicode_names2 as names;
pub use unicode_normalization as normalization;
pub use unicode_properties as properties;
pub use unicode_script as script;
pub use unicode_segmentation as segmentation;

pub fn get_char_range_from_block_name(name: &str) -> Option<NumberSpan<char>> {
    match UNICODE_BLOCKS.get(&name) {
        Some(&rng) => {
            let chr_start = char::from_u32(rng.start())?;
            let chr_end = char::from_u32(rng.end())?;
            Some((chr_start ..= chr_end).into())
        }
        None => None,
    }
}
pub fn get_char_block_name(c: u32) -> Option<&'static str> {
    if matches!(c, 0xD800 ..= 0xDB7F) {
        Some("High Surrogates")
    } else if matches!(c, 0xDB80 ..= 0xDBFF) {
        Some("High Private Use Surrogates")
    } else if matches!(c, 0xDC00 ..= 0xDFFF) {
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

pub const fn get_cjk_ideographs_blocks() -> &'static [&'static UnicodeBlock] {
    CJK_IDEOGRAPHS
}

const CJK_IDEOGRAPHS: &'static [&UnicodeBlock; 13] = &[
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
    &ub::CJK_UNIFIED_IDEOGRAPHS_EXTENSION_J,
    &ub::CJK_COMPATIBILITY_IDEOGRAPHS,
    &ub::CJK_COMPATIBILITY_IDEOGRAPHS_SUPPLEMENT,
];

lazy_static! {
    pub static ref UNICODE_BLOCKS: HashMap<&'static str, &'static UnicodeBlock> =
        HashMap::from_iter(ub::UNICODE_BLOCKS);
}
