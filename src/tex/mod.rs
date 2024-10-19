#![allow(unused)]

use bitflags::bitflags;
use pest::{iterators::Pairs, Parser};

#[derive(pest_derive::Parser)]
#[grammar = "tex/texcsname.pest"]
pub struct LaTeXParser;

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

pub fn get_cs_type(input: &str) -> LaTeXType {
    match LaTeXParser::parse(Rule::cs, input) {
        Ok(pairs) => {
            let pair = pairs
                .into_iter()
                .next()
                .unwrap()
                .into_inner()
                .into_iter()
                .next()
                .unwrap();
            let typ = match pair.as_rule() {
                Rule::csname_l3_primitive => LaTeXType::L3Primitive,
                Rule::csname_l3_func_internal => LaTeXType::L3FunctionInternal,
                Rule::csname_l3_func_pub => LaTeXType::L3FunctionPublic,
                Rule::csname_l3_func_kernel => LaTeXType::L3FunctionKernel,
                Rule::csname_l3_var_internal => LaTeXType::L3VariableInternal,
                Rule::csname_l3_var_pub => LaTeXType::L3VariablePublic,
                Rule::csname_l3_var_kernel => LaTeXType::L3VariableKernel,
                Rule::csname_carmal_case_small => LaTeXType::DocumentSmallCarmel,
                Rule::csname_pascal => LaTeXType::DocumentPascal,
                Rule::csname_l2e_internal => LaTeXType::L2eInternal,
                Rule::csname_l2e_kernel => LaTeXType::L2eKernel,
                Rule::csname_punct => LaTeXType::Punctuation,
                _ => LaTeXType::Other,
            };
            return typ;
        }
        Err(_) => return LaTeXType::Other,
    }
}

pub fn cs_is_rule(input: &str, rule: Rule) -> bool {
    get_cs_rules(input).contains(&rule)
}

pub fn get_cs_rules(input: &str) -> Vec<Rule> {
    match LaTeXParser::parse(Rule::cs, input) {
        Ok(pairs) => get_pairs_latex_rules(pairs),
        Err(_) => {
            vec![]
        }
    }
}

fn get_pairs_latex_rules<'a>(mut pairs: Pairs<'a, Rule>) -> Vec<Rule> {
    let mut rules_vec = vec![];
    loop {
        match pairs.into_iter().next() {
            Some(pair) => {
                rules_vec.push(pair.as_rule());
                pairs = pair.into_inner();
            }
            None => {
                return rules_vec;
            }
        }
    }
}

pub fn cs_is_type(cs: &str, t: LaTeXType) -> bool {
    t.contains(get_cs_type(cs))
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
