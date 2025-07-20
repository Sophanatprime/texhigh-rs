use criterion::{criterion_group, criterion_main, Criterion};
use std::hint::black_box;

use texhigh::tex::args_parser::*;
use texhigh::types::*;
use texhigh::LaTeXType;

#[rustfmt::skip]
const CS_NAMES: &[&str] = &[
    r"a", r"A", r"LaTeX", r"relax", r"@expl@@@initialize@all@@", r"@kernel@eqno",
    r"@", r"\", r"_", r"DeclareRobustCommand", r"smallSkip", r"aBzWW", 
    r"tex_relax:D", r"tex_XeTeXinterclass", r"group_begin:",
    r"__text_change_case_codepoint:nnnnn", r"text_declare_lowercase_mapping:nn",
    r"c_space_token", r"q_nil", r"exp:w", r"__kernel_exp_not:w",
    r"g__kernel_prg_map_int", r"l__str_internal_tl",
];

fn cs_type_regex(cs: &str) -> &str {
    match texhigh::get_cs_type(cs) {
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
}

fn cs_name_regex(c: &mut Criterion) {
    c.bench_function("cs name regex", |b| {
        b.iter(|| {
            black_box(CS_NAMES.iter().for_each(|&cs| {
                let _ = cs_type_regex(cs);
            }))
        })
    });
}

fn cs_type_pest(cs: &str) -> &str {
    match pest_cs_type::get_cs_type(cs) {
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
}

pub fn cs_name_pest(c: &mut Criterion) {
    c.bench_function("cs name pest", |b| {
        b.iter(|| {
            black_box(CS_NAMES.iter().for_each(|&cs| {
                let _ = cs_type_pest(cs);
            }))
        })
    });
}

mod pest_cs_type {
    use pest::Parser;
    use texhigh::LaTeXType;

    #[derive(pest_derive::Parser)]
    #[grammar = "benches/texcsname.pest"]
    pub struct LaTeXParser;

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
                    Rule::csname_l3_func_internal => {
                        LaTeXType::L3FunctionInternal
                    }
                    Rule::csname_l3_func_pub => LaTeXType::L3FunctionPublic,
                    Rule::csname_l3_func_kernel => LaTeXType::L3FunctionKernel,
                    Rule::csname_l3_var_internal => {
                        LaTeXType::L3VariableInternal
                    }
                    Rule::csname_l3_var_pub => LaTeXType::L3VariablePublic,
                    Rule::csname_l3_var_kernel => LaTeXType::L3VariableKernel,
                    Rule::csname_carmal_case_small => {
                        LaTeXType::DocumentSmallCarmel
                    }
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
}

pub fn args_parse_spec(c: &mut Criterion) {
    let ref catcode = CTab::document();
    let spec = r##" s l m o O{long\relax argument}
        r\left\right R(){required} u{^^M} g"##;
    let spec = TokenList::parse(spec, catcode);

    let mut group = c.benchmark_group("parse-spec");
    group.sample_size(1_0000);
    group.bench_function("args parse spec", |b| {
        b.iter(|| {
            black_box(ArgFinder::parse(&spec).unwrap());
        })
    });
}

pub fn args_find_normal(c: &mut Criterion) {
    let ref catcode = CTab::latex3();
    let grabber =
        ArgSpec::m(ArgGroupStatus::default()).into_finder(false).unwrap();
    let source = r##" {\scan_stop: \def\newpage{this is a newpage command}.
        \tex_unskip:D \hskip 12bp plus 1bp minus 12bp}"##;
    let source = TokenList::parse(source, catcode);
    assert_eq!(grabber(&source, 0).unwrap(), Argument::Span(1, source.len()));

    let mut group = c.benchmark_group("find-m-args");
    group.sample_size(1_0000);
    group.bench_function("args find normal", |b| {
        b.iter(|| {
            black_box(grabber(&source, 0).unwrap());
        })
    });
}

pub fn args_find_all(c: &mut Criterion) {
    let ref catcode = CTab::document();
    let spec = r##" s l m o O{long\relax argument}
        r\left\right R(){required} u{^^M} g"##;
    let spec = TokenList::parse(spec, catcode);
    let finder = ArgFinder::parse(&spec).unwrap();
    let source = r##"
* {a mandatory\{ argument} [a [optional] argument]
\left[\left(some braces\right)\right (the (required) (delimited) argument)^^M
"##;
    let source = TokenList::parse(source, catcode);

    let result = finder.find_all(&source).unwrap();
    assert_eq!(result.len(), 9);
    assert_eq!(&result[0], &Argument::Present(1, 2));
    assert_eq!(&result[1], &Argument::Span(2, 3));
    assert_eq!(&result[2], &Argument::Span(3, 26));
    assert_eq!(&result[3], &Argument::Present(27, 50));
    assert_eq!(&result[4], &Argument::UnPresent(50));
    assert_eq!(&result[5], &Argument::Span(51, 69));
    assert_eq!(&result[6], &Argument::Span(70, 107));
    assert_eq!(&result[7], &Argument::Span(107, 108));
    assert_eq!(&result[8], &Argument::UnPresent(108));

    let mut group = c.benchmark_group("find-args");
    group.sample_size(1_0000);
    group.bench_function("args find all", |b| {
        b.iter(|| {
            black_box(finder.find_all(&source).unwrap());
        })
    });
}

criterion_group!(
    tex,
    cs_name_regex,
    cs_name_pest,
    args_parse_spec,
    args_find_normal,
    args_find_all,
);
criterion_main!(tex);
