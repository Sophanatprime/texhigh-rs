#![feature(test)]
extern crate test;

use mimalloc::MiMalloc;
#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

#[cfg(test)]
mod basic {
    use super::test::{black_box, Bencher};
    use memchr::{memchr, memmem};
    use std::{fs::File, io::Read};

    #[bench]
    fn find_tab_small(b: &mut Bencher) {
        let mut s = String::new();
        File::open(r"benches/list.tex")
            .unwrap()
            .read_to_string(&mut s)
            .unwrap();
        b.iter(black_box(|| {
            s.contains('\t');
        }));
    }

    #[bench]
    fn find_tab_small_memchr(b: &mut Bencher) {
        let mut s = String::new();
        File::open(r"benches/list.tex")
            .unwrap()
            .read_to_string(&mut s)
            .unwrap();
        b.iter(black_box(|| memchr(b'\t', s.as_bytes()).is_some()));
    }

    #[bench]
    fn find_tab_small_memmem(b: &mut Bencher) {
        let mut s = String::new();
        File::open(r"benches/list.tex")
            .unwrap()
            .read_to_string(&mut s)
            .unwrap();
        b.iter(black_box(|| memmem::find(s.as_bytes(), b"\t").is_some()));
    }

    #[bench]
    fn find_tab_large(b: &mut Bencher) {
        let mut s = String::new();
        File::open(r"benches/expl3-code.tex")
            .unwrap()
            .read_to_string(&mut s)
            .unwrap();
        b.iter(black_box(|| {
            s.contains('\t');
        }));
    }

    #[bench]
    fn find_tab_large_memchr(b: &mut Bencher) {
        let mut s = String::new();
        File::open(r"benches/expl3-code.tex")
            .unwrap()
            .read_to_string(&mut s)
            .unwrap();
        b.iter(black_box(|| memchr(b'\t', s.as_bytes()).is_some()));
    }

    #[bench]
    fn find_tab_large_memmem(b: &mut Bencher) {
        let mut s = String::new();
        File::open(r"benches/expl3-code.tex")
            .unwrap()
            .read_to_string(&mut s)
            .unwrap();
        b.iter(black_box(|| memmem::find(s.as_bytes(), b"\t").is_some()));
    }
}

#[cfg(test)]
mod tex {
    use super::test::{black_box, Bencher};
    use texhigh::{get_cs_type, get_cs_type_re, LaTeXType};

    #[rustfmt::skip]
    const CS_NAMES: &[&str] = &[
        r"a", r"A", r"LaTeX", r"relax", r"@expl@@@initialize@all@@", r"@kernel@eqno",
        r"@", r"\", r"_", r"DeclareRobustCommand", r"smallSkip", r"aBzWW", 
        r"tex_relax:D", r"tex_XeTeXinterclass", r"group_begin:",
        r"__text_change_case_codepoint:nnnnn", r"text_declare_lowercase_mapping:nn",
        r"c_space_token", r"q_nil", r"exp:w", r"__kernel_exp_not:w",
        r"g__kernel_prg_map_int", r"l__str_internal_tl",
    ];

    #[bench]
    fn cs_name_pest(b: &mut Bencher) {
        b.iter(|| {
            black_box(CS_NAMES.iter().for_each(|&cs| {
                let _ = match get_cs_type(cs) {
                    LaTeXType::L3Primitive => "latex3.primitive",
                    LaTeXType::L3FunctionInternal => {
                        "latex3.function.internal"
                    }
                    LaTeXType::L3FunctionPublic => "latex3.function.public",
                    LaTeXType::L3FunctionKernel => "latex3.function.kernel",
                    LaTeXType::L3VariableInternal => {
                        "latex3.variable.internal"
                    }
                    LaTeXType::L3VariablePublic => "latex3.variable.public",
                    LaTeXType::L3VariableKernel => "latex3.variable.kernel",
                    LaTeXType::DocumentSmallCarmel => "latex.programming",
                    LaTeXType::DocumentPascal => "latex.programming",
                    LaTeXType::L2eInternal => "latex.internal",
                    LaTeXType::L2eKernel => "latex.internal",
                    LaTeXType::Punctuation => "?",
                    LaTeXType::Other | _ => "?",
                };
            }))
        });
    }

    #[bench]
    fn cs_name_regex(b: &mut Bencher) {
        b.iter(|| {
            black_box(CS_NAMES.iter().for_each(|&cs| {
                let _ = match get_cs_type_re(cs) {
                    LaTeXType::L3Primitive => "latex3.primitive",
                    LaTeXType::L3FunctionInternal => {
                        "latex3.function.internal"
                    }
                    LaTeXType::L3FunctionPublic => "latex3.function.public",
                    LaTeXType::L3FunctionKernel => "latex3.function.kernel",
                    LaTeXType::L3VariableInternal => {
                        "latex3.variable.internal"
                    }
                    LaTeXType::L3VariablePublic => "latex3.variable.public",
                    LaTeXType::L3VariableKernel => "latex3.variable.kernel",
                    LaTeXType::DocumentSmallCarmel => "latex.programming",
                    LaTeXType::DocumentPascal => "latex.programming",
                    LaTeXType::L2eInternal => "latex.internal",
                    LaTeXType::L2eKernel => "latex.internal",
                    LaTeXType::Punctuation => "?",
                    LaTeXType::Other | _ => "?",
                };
            }))
        });
    }
}

#[cfg(test)]
mod tl {
    use std::fs::File;
    use std::io::Read;
    use std::sync::Arc;

    use super::test::{black_box, Bencher};
    use texhigh::config::LexerType;
    use texhigh::tokenlist::SourcedTokenList;
    use texhigh::types::{CTab, CTabSet, CatCodeStack, TokenList};

    #[bench]
    fn parse_large_texfile(b: &mut Bencher) {
        let mut s = String::new();
        File::open(r"benches/expl3-code.tex")
            .unwrap()
            .read_to_string(&mut s)
            .unwrap();
        let ctab = &CTab::latex3package();
        b.iter(|| {
            black_box({
                let _ = TokenList::parse(&s, ctab);
            })
        });
    }

    #[bench]
    fn parse_mid_texfile(b: &mut Bencher) {
        let mut s = String::new();
        File::open(r"benches/texhigh.tex")
            .unwrap()
            .read_to_string(&mut s)
            .unwrap();
        let ctab = &CTab::latex3package();
        b.iter(|| {
            black_box({
                let _ = TokenList::parse(&s, ctab);
            })
        });
    }

    #[bench]
    fn parse_small_texfile(b: &mut Bencher) {
        let mut s = String::new();
        File::open(r"benches/list.tex")
            .unwrap()
            .read_to_string(&mut s)
            .unwrap();
        let ctab = &CTab::package();
        b.iter(|| {
            black_box({
                let _ = TokenList::parse(&s, ctab);
            })
        });
    }

    #[bench]
    fn parse_large_texfile_sourced(b: &mut Bencher) {
        let mut s = String::new();
        File::open(r"benches/expl3-code.tex")
            .unwrap()
            .read_to_string(&mut s)
            .unwrap();
        let s = Arc::new(s);
        b.iter(|| {
            let lexer = LexerType::default();
            let ctabset = CTabSet::new_empty();
            let mut cat = CatCodeStack::new();
            cat.push(CTab::latex3package());
            black_box({
                let _ = SourcedTokenList::parse(
                    s.clone(),
                    &mut cat,
                    (&lexer, &ctabset),
                );
            })
        });
    }

    #[bench]
    fn parse_mid_texfile_sourced(b: &mut Bencher) {
        let mut s = String::new();
        File::open(r"benches/texhigh.tex")
            .unwrap()
            .read_to_string(&mut s)
            .unwrap();
        let s = Arc::new(s);
        b.iter(|| {
            let lexer = LexerType::default();
            let ctabset = CTabSet::new_empty();
            let mut cat = CatCodeStack::new();
            cat.push(CTab::latex3package());
            black_box({
                let _ = SourcedTokenList::parse(
                    s.clone(),
                    &mut cat,
                    (&lexer, &ctabset),
                );
            })
        });
    }

    #[bench]
    fn parse_small_texfile_sourced(b: &mut Bencher) {
        let mut s = String::new();
        File::open(r"benches/list.tex")
            .unwrap()
            .read_to_string(&mut s)
            .unwrap();
        let s = Arc::new(s);
        b.iter(|| {
            let lexer = LexerType::default();
            let ctabset = CTabSet::new_empty();
            let mut cat = CatCodeStack::new();
            cat.push(CTab::package());
            black_box({
                let _ = SourcedTokenList::parse(
                    s.clone(),
                    &mut cat,
                    (&lexer, &ctabset),
                );
            })
        });
    }
}

#[cfg(test)]
mod high {
    use std::{fs::File, io::Read, sync::Arc};

    use super::test::{black_box, Bencher};
    use texhigh::{
        config::{HighConfig, LexerType},
        high::{Null, StandardFormatter},
        tokenlist::{SourcedFormatter, SourcedTokenList},
        types::{CTab, CTabSet, CatCodeStack, TokenList},
    };

    #[bench]
    fn high_large_texfile_no_out(b: &mut Bencher) {
        let mut s = String::new();
        File::open(r"benches/expl3-code.tex")
            .unwrap()
            .read_to_string(&mut s)
            .unwrap();
        let tl = TokenList::parse(s, &CTab::latex3package());
        let atl = Arc::new(tl);
        let cf = HighConfig::default();
        b.iter(|| {
            let f = StandardFormatter::new(&cf, atl.clone());
            black_box(f.format_now(&mut Null).unwrap());
        });
    }

    #[bench]
    fn high_mid_texfile_no_out(b: &mut Bencher) {
        let mut s = String::new();
        File::open(r"benches/texhigh.tex")
            .unwrap()
            .read_to_string(&mut s)
            .unwrap();
        let tl = TokenList::parse(s, &CTab::latex3package());
        let atl = Arc::new(tl);
        let cf = HighConfig::default();
        b.iter(|| {
            let f = StandardFormatter::new(&cf, atl.clone());
            black_box(f.format_now(&mut Null).unwrap());
        });
    }

    #[bench]
    fn high_small_texfile_no_out(b: &mut Bencher) {
        let mut s = String::new();
        File::open(r"benches/list.tex")
            .unwrap()
            .read_to_string(&mut s)
            .unwrap();
        let tl = TokenList::parse(s, &CTab::latex3package());
        let atl = Arc::new(tl);
        let cf = HighConfig::default();
        b.iter(|| {
            let f = StandardFormatter::new(&cf, atl.clone());
            black_box(f.format_now(&mut Null).unwrap());
        });
    }

    #[bench]
    fn sourced_high_large_texfile_no_out(b: &mut Bencher) {
        let mut s = String::new();
        File::open(r"benches/expl3-code.tex")
            .unwrap()
            .read_to_string(&mut s)
            .unwrap();
        let lexer = LexerType::default();
        let ctabset = CTabSet::new_empty();
        let mut cat = CatCodeStack::new();
        cat.push(CTab::latex3package());
        let stl =
            SourcedTokenList::parse(s.into(), &mut cat, (&lexer, &ctabset));
        let atl = Arc::new(stl);
        let cf = HighConfig::default();
        b.iter(|| {
            let f = SourcedFormatter::new(&cf, atl.clone());
            black_box(f.format_now(&mut Null).unwrap());
        });
    }

    #[bench]
    fn sourced_high_mid_texfile_no_out(b: &mut Bencher) {
        let mut s = String::new();
        File::open(r"benches/texhigh.tex")
            .unwrap()
            .read_to_string(&mut s)
            .unwrap();
        let lexer = LexerType::default();
        let ctabset = CTabSet::new_empty();
        let mut cat = CatCodeStack::new();
        cat.push(CTab::latex3package());
        let tl =
            SourcedTokenList::parse(s.into(), &mut cat, (&lexer, &ctabset));
        let atl = Arc::new(tl);
        let cf = HighConfig::default();
        b.iter(|| {
            let f = SourcedFormatter::new(&cf, atl.clone());
            black_box(f.format_now(&mut Null).unwrap());
        });
    }

    #[bench]
    fn sourced_high_small_texfile_no_out(b: &mut Bencher) {
        let mut s = String::new();
        File::open(r"benches/list.tex")
            .unwrap()
            .read_to_string(&mut s)
            .unwrap();
        let lexer = LexerType::default();
        let ctabset = CTabSet::new_empty();
        let mut cat = CatCodeStack::new();
        cat.push(CTab::latex3package());
        let tl =
            SourcedTokenList::parse(s.into(), &mut cat, (&lexer, &ctabset));
        let atl = Arc::new(tl);
        let cf = HighConfig::default();
        b.iter(|| {
            let f = SourcedFormatter::new(&cf, atl.clone());
            black_box(f.format_now(&mut Null).unwrap());
        });
    }
}

#[cfg(test)]
mod regtex {
    use super::test::{black_box, Bencher};
    use regex::Regex;
    use texhigh::{
        regtex::RegTEx,
        types::{CTab, CatCode, TokenList},
    };

    #[bench]
    fn regex_simple(b: &mut Bencher) {
        let re =
            Regex::new(r"^(\\([\p{Han}\pL]+|\pP|\p{Cc})\s*)+ad.relax?a+好?$")
                .unwrap();
        b.iter(|| {
            black_box(
                re.is_match(r"\controlsequence控制序列\.\。\\addrelaxaaaaa"),
            )
        });
    }
    #[bench]
    fn regtex_tl_simple(b: &mut Bencher) {
        let re = RegTEx::new(
            r"^(\c{([\p{Han}\pL]+|\pP|\p{Cc})}\s*)+ad.relax?a+好?$",
        )
        .unwrap();
        let mut ctab = CTab::document();
        ctab.extend(CTab::cjk_ideographs(CatCode::Letter));
        let tl = TokenList::parse(
            r"\controlsequence控制序列\.\。\\addrelaxaaaaa",
            &ctab,
        );
        let tb = tl.to_bytes();
        b.iter(|| black_box(re.is_match(&tb)));
    }
}
