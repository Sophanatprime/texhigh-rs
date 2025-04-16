#![allow(unused)]

use bitflags::bitflags;
use lazy_static::lazy_static;
use regex::RegexSet;

mod circumflex;
pub use circumflex::circumflex_mechanism;

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct LaTeXType: u16 {
        const L3Primitive = 1;
        const L3FunctionInternal = 1<<1;
        const L3FunctionPublic = 1<<2;
        const L3FunctionKernel = 1<<3;
        const L3VariableInternal = 1<<4;
        const L3VariablePublic = 1<<5;
        const L3VariableKernel = 1<<6;
        const DocumentSmallCarmel = 1<<7; // 小驼峰
        const DocumentPascal = 1<<8;      // 大驼峰，帕斯卡命名法
        const L2eKernel = 1<<9;           // 2e kernel
        const L2eInternal = 1<<10;        // 2e internal
        const Punctuation = 1<<11;

        const Document = Self::DocumentPascal.bits() |
            Self::DocumentSmallCarmel.bits();
        const Internal = Self::L3FunctionInternal.bits() |
            Self::L3VariableInternal.bits() |
            Self::L2eInternal.bits();
        const Kernel = Self::L3FunctionKernel.bits() |
            Self::L3VariableKernel.bits() |
            Self::L2eKernel.bits();
        const LaTeX3Public = Self::L3FunctionPublic.bits() |
            Self::L3VariablePublic.bits();
        const LaTeX3Internal = Self::L3FunctionInternal.bits() |
            Self::L3FunctionKernel.bits() |
            Self::L3Primitive.bits() |
            Self::L3VariableInternal.bits() |
            Self::L3VariableKernel.bits();
        const LaTeX3 = Self::L3FunctionInternal.bits() |
            Self::L3FunctionKernel.bits() |
            Self::L3FunctionPublic.bits() |
            Self::L3Primitive.bits() |
            Self::L3VariableInternal.bits() |
            Self::L3VariableKernel.bits() |
            Self::L3VariablePublic.bits();
        const Public = Self::Document.bits() | Self::LaTeX3Public.bits();

        const Other = 1<<12;
    }
}

lazy_static! {
    static ref CS_TYPE_REGEX: RegexSet = RegexSet::new(&[
        r"^@kernel@[@a-zA-Z]+$", // latex2e kernel
        r"^tex_[_a-zA-Z]+:D$", // latex3 primitive
        r"^__kernel_[_a-zA-Z]*:[:_a-zA-Z]*$", // latex3 func kernel
        r"^[lgcsq]__kernel_[_a-zA-Z]+$", // latex3 var kernel
        r"^@[@a-zA-Z]+|[a-zA-Z]+@[@a-zA-Z]*$", // latex2e internal
        r"^__[_a-zA-Z]*:[:_a-zA-Z]*$", // latex3 func internal
        r"^[lgcsq]__[_a-zA-Z]+$", // latex3 var internal
        r"^[a-zA-Z]+_*[_a-zA-Z]*:[:_a-zA-Z]*$", // latex3 func pub
        r"^[lgcsq]_[a-zA-Z][_a-zA-Z]*$", // latex3 var pub
        r"^[a-zA-Z]$|^[a-z]+([A-Z][a-zA-Z]*)+$", // carmal case small
        r"^[A-Z][a-zA-Z]*$", // pascal
        r#"^[!@#$%^\&*\-_+=(){}\[\]<>:;"'|\\,.?/]$"#, // punct
    ]).unwrap();
}

pub fn get_cs_type_re(input: &str) -> LaTeXType {
    if let Some(idx) = CS_TYPE_REGEX.matches(input).into_iter().next() {
        match idx {
            0 => LaTeXType::L2eKernel,
            1 => LaTeXType::L3Primitive,
            2 => LaTeXType::L3FunctionKernel,
            3 => LaTeXType::L3VariableKernel,
            4 => LaTeXType::L2eInternal,
            5 => LaTeXType::L3FunctionInternal,
            6 => LaTeXType::L3VariableInternal,
            7 => LaTeXType::L3FunctionPublic,
            8 => LaTeXType::L3VariablePublic,
            9 => LaTeXType::DocumentSmallCarmel,
            10 => LaTeXType::DocumentPascal,
            11 => LaTeXType::Punctuation,
            _ => LaTeXType::Other,
        }
    } else {
        LaTeXType::Other
    }
}

pub fn cs_is_type(cs: &str, t: LaTeXType) -> bool {
    t.contains(get_cs_type_re(cs))
}

include!("primitives_data.rs");

macro_rules! gen_prim_fn {
    ($id:ident, $fun:ident, $val:expr) => {
        pub fn $id(cs: &str) -> bool {
            PRIMITIVES_DATA.get(cs).map_or(false, |p| p.$fun($val))
        }
    };
    ($id:ident, !$fun:ident, $val:expr) => {
        pub fn $id(cs: &str) -> bool {
            PRIMITIVES_DATA.get(cs).map_or(false, |&p| $val.$fun(p))
        }
    };
    ($id:ident, $fun:ident) => {
        pub fn $id(cs: &str) -> bool {
            PRIMITIVES_DATA.get(cs).map_or(false, |p| p.$fun())
        }
    };
    ($id:ident, !$fun:ident) => {
        pub fn $id(cs: &str) -> bool {
            PRIMITIVES_DATA.get(cs).map_or(false, |p| !p.$fun())
        }
    };
    ($id:ident, ==, $val:expr) => {
        pub fn $id(cs: &str) -> bool {
            PRIMITIVES_DATA.get(cs).map_or(false, |&p| p == $val)
        }
    };
    ($id:ident, !=, $val:expr) => {
        pub fn $id(cs: &str) -> bool {
            PRIMITIVES_DATA.get(cs).map_or(false, |&p| p != &val)
        }
    };
}
gen_prim_fn!(is_primitive, !is_empty);
gen_prim_fn!(is_knuthtex_primitive, intersects, Primitives::KNUTHTEX);
gen_prim_fn!(is_etex_primitive, intersects, Primitives::ETEX);
gen_prim_fn!(is_pdftex_primitive, intersects, Primitives::PDFTEX);
gen_prim_fn!(is_xetex_primitive, intersects, Primitives::XETEX);
gen_prim_fn!(is_luatex_primitive, intersects, Primitives::LUATEX);
gen_prim_fn!(is_uptex_primitive, intersects, Primitives::UPTEX);
gen_prim_fn!(is_knuthtex_only_primitive, ==, Primitives::KNUTHTEX);
gen_prim_fn!(is_etex_only_primitive, ==, Primitives::ETEX);
gen_prim_fn!(is_pdftex_only_primitive, ==, Primitives::PDFTEX);
gen_prim_fn!(is_xetex_only_primitive, ==, Primitives::XETEX);
gen_prim_fn!(is_luatex_only_primitive, ==, Primitives::LUATEX);
gen_prim_fn!(is_uptex_only_primitive, ==, Primitives::UPTEX);
gen_prim_fn!(is_unicode_tex_primitive, ==, Primitives::UNICODE_TEX);
gen_prim_fn!(is_widely_primitive, ==, Primitives::WIDELY_USED);

pub fn is_luametatex_primitive(cs: &str) -> bool {
    LUAMETATEX_PRIMITIVES_DATA.contains(cs)
}

/// Return value: <knuthtex, etex, pdftex, xetex, luatex, uptex, widely, sometex, "">
pub fn primitive_engine(cs: &str) -> &'static str {
    match PRIMITIVES_DATA.get(cs) {
        Some(&t) => {
            if t.intersects(Primitives::KNUTHTEX) {
                "knuthtex"
            } else if t.intersects(Primitives::ETEX) {
                "etex"
            } else if t == Primitives::PDFTEX {
                "pdftex"
            } else if t == Primitives::XETEX {
                "xetex"
            } else if t == Primitives::LUATEX {
                "luatex"
            } else if t == Primitives::UPTEX {
                "uptex"
            } else if t == Primitives::WIDELY_USED {
                "widely"
            } else {
                "sometex"
            }
        }
        None => {
            if is_luametatex_primitive(cs) {
                "luametatex"
            } else {
                ""
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cs_name_type() {
        use super::LaTeXType as L;

        assert_eq!(get_cs_type_re(r"a"), L::DocumentSmallCarmel);
        assert_eq!(get_cs_type_re(r"A"), L::DocumentSmallCarmel);
        assert_eq!(get_cs_type_re(r"LaTeX"), L::DocumentPascal);
        assert_eq!(get_cs_type_re(r"relax"), L::Other);
        assert_eq!(
            get_cs_type_re(r"@expl@@@initialize@all@@"),
            L::L2eInternal
        );
        assert_eq!(get_cs_type_re(r"@kernel@eqno"), L::L2eKernel);
        assert_eq!(get_cs_type_re(r"@"), L::Punctuation);
        assert_eq!(get_cs_type_re(r"\"), L::Punctuation);
        assert_eq!(get_cs_type_re(r"_"), L::Punctuation);
        assert_eq!(get_cs_type_re(r"DeclareRobustCommand"), L::DocumentPascal);
        assert_eq!(get_cs_type_re(r"smallSkip"), L::DocumentSmallCarmel);
        assert_eq!(get_cs_type_re(r"smallSkipSSkip"), L::DocumentSmallCarmel);
        assert_eq!(get_cs_type_re(r"aBzWW"), L::DocumentSmallCarmel);
        assert_eq!(get_cs_type_re(r"tex_relax:D"), L::L3Primitive);
        assert_eq!(get_cs_type_re(r"tex_XeTeXinterclass:D"), L::L3Primitive);
        assert_eq!(
            get_cs_type_re(r"token_XeTeXinterclass:D"),
            L::L3FunctionPublic
        );
        assert_eq!(get_cs_type_re(r"group_begin:"), L::L3FunctionPublic);
        assert_eq!(
            get_cs_type_re(r"__text_change_case_codepoint:nnnnn"),
            L::L3FunctionInternal
        );
        assert_eq!(
            get_cs_type_re(r"text_declare_lowercase_mapping:nn"),
            L::L3FunctionPublic
        );
        assert_eq!(get_cs_type_re(r"c_space_token"), L::L3VariablePublic);
        assert_eq!(get_cs_type_re(r"q_nil"), L::L3VariablePublic);
        assert_eq!(get_cs_type_re(r"exp:w"), L::L3FunctionPublic);
        assert_eq!(get_cs_type_re(r"__kernel_exp_not:w"), L::L3FunctionKernel);
        assert_eq!(
            get_cs_type_re(r"g__kernel_prg_map_int"),
            L::L3VariableKernel
        );
        assert_eq!(
            get_cs_type_re(r"l__str_internal_tl"),
            L::L3VariableInternal
        );
        assert_eq!(
            get_cs_type_re(r"l___str__internal_tl_"),
            L::L3VariableInternal
        );
        assert_eq!(
            get_cs_type_re(r"___str___internal_tl:"),
            L::L3FunctionInternal
        );
        assert_eq!(
            get_cs_type_re(r"str___internal_tl_:_"),
            L::L3FunctionPublic
        );

        assert_eq!(get_cs_type_re(r"a"), L::DocumentSmallCarmel);
        assert_eq!(get_cs_type_re(r"A"), L::DocumentSmallCarmel);
        assert_eq!(get_cs_type_re(r"LaTeX"), L::DocumentPascal);
        assert_eq!(get_cs_type_re(r"relax"), L::Other);
        assert_eq!(
            get_cs_type_re(r"@expl@@@initialize@all@@"),
            L::L2eInternal
        );
        assert_eq!(get_cs_type_re(r"@kernel@eqno"), L::L2eKernel);
        assert_eq!(get_cs_type_re(r"@"), L::Punctuation);
        assert_eq!(get_cs_type_re(r"\"), L::Punctuation);
        assert_eq!(get_cs_type_re(r"_"), L::Punctuation);
        assert_eq!(get_cs_type_re(r"DeclareRobustCommand"), L::DocumentPascal);
        assert_eq!(get_cs_type_re(r"smallSkip"), L::DocumentSmallCarmel);
        assert_eq!(get_cs_type_re(r"smallSkipSSkip"), L::DocumentSmallCarmel);
        assert_eq!(get_cs_type_re(r"aBzWW"), L::DocumentSmallCarmel);
        assert_eq!(get_cs_type_re(r"tex_relax:D"), L::L3Primitive);
        assert_eq!(get_cs_type_re(r"tex_XeTeXinterclass:D"), L::L3Primitive);
        assert_eq!(
            get_cs_type_re(r"token_XeTeXinterclass:D"),
            L::L3FunctionPublic
        );
        assert_eq!(get_cs_type_re(r"group_begin:"), L::L3FunctionPublic);
        assert_eq!(
            get_cs_type_re(r"__text_change_case_codepoint:nnnnn"),
            L::L3FunctionInternal
        );
        assert_eq!(
            get_cs_type_re(r"text_declare_lowercase_mapping:nn"),
            L::L3FunctionPublic
        );
        assert_eq!(get_cs_type_re(r"c_space_token"), L::L3VariablePublic);
        assert_eq!(get_cs_type_re(r"q_nil"), L::L3VariablePublic);
        assert_eq!(get_cs_type_re(r"exp:w"), L::L3FunctionPublic);
        assert_eq!(get_cs_type_re(r"__kernel_exp_not:w"), L::L3FunctionKernel);
        assert_eq!(
            get_cs_type_re(r"g__kernel_prg_map_int"),
            L::L3VariableKernel
        );
        assert_eq!(
            get_cs_type_re(r"l__str_internal_tl"),
            L::L3VariableInternal
        );
        assert_eq!(
            get_cs_type_re(r"l___str__internal_tl_"),
            L::L3VariableInternal
        );
        assert_eq!(
            get_cs_type_re(r"___str___internal_tl:"),
            L::L3FunctionInternal
        );
        assert_eq!(
            get_cs_type_re(r"str___internal_tl_:_"),
            L::L3FunctionPublic
        );
    }

    #[test]
    fn tex_primitive() {
        assert_eq!(is_primitive("relax"), true);
        assert_eq!(is_primitive("expanded"), true);
        assert_eq!(is_primitive("norelax"), false);
        assert_eq!(is_knuthtex_primitive("relax"), true);
        assert_eq!(is_knuthtex_primitive("unexpanded"), false);
        assert_eq!(is_etex_primitive("unexpanded"), true);
        assert_eq!(is_etex_primitive("pdfsavepos"), false);
        assert_eq!(is_pdftex_primitive("pdfsavepos"), true);
        assert_eq!(is_pdftex_primitive("Uchar"), false);
        assert_eq!(is_xetex_primitive("uniformdeviate"), true);
        assert_eq!(is_xetex_primitive("pdfliteral"), false);
        assert_eq!(is_luatex_primitive("Uchar"), true);
        assert_eq!(is_luatex_primitive("savepos"), true);
        assert_eq!(is_uptex_primitive("tracinglostchars"), true);
        assert_eq!(is_uptex_primitive("norelax"), false);

        assert_eq!(is_knuthtex_only_primitive("relax"), false);
        assert_eq!(is_etex_only_primitive("noexpand"), false);
        assert_eq!(is_pdftex_only_primitive("unexpanded"), false);
        assert_eq!(is_pdftex_only_primitive("pdfliteral"), true);
        assert_eq!(is_xetex_only_primitive("unexpanded"), false);
        assert_eq!(is_xetex_only_primitive("XeTeXcharclass"), true);
        assert_eq!(is_luatex_only_primitive("unexpanded"), false);
        assert_eq!(is_luatex_only_primitive("immediateassigned"), true);
        assert_eq!(is_uptex_only_primitive("unexpanded"), false);
        assert_eq!(is_uptex_only_primitive("tate"), true);

        println!("{:08b}", PRIMITIVES_DATA.get("Uchar").unwrap().bits());
        assert_eq!(is_unicode_tex_primitive("Uchar"), true);
        assert_eq!(is_unicode_tex_primitive("pdfsavepos"), false);
        println!("{:08b}", PRIMITIVES_DATA.get("expanded").unwrap().bits());
        assert_eq!(is_widely_primitive("expanded"), true);
        assert_eq!(is_widely_primitive("partokenname"), true);
        println!("{:08b}", PRIMITIVES_DATA.get("Uchar").unwrap().bits());
        assert_eq!(is_widely_primitive("Uchar"), false);

        assert_eq!(is_luametatex_primitive("localcontrolled"), true);
        assert_eq!(is_luametatex_primitive("partokenname"), false);
    }
}
