use criterion::{criterion_group, criterion_main, Criterion};
use std::hint::black_box;

use regex::Regex;
use texhigh::{
    regtex::RegTEx,
    types::{CTab, CatCode, TokenList},
};

fn regex_simple(c: &mut Criterion) {
    let re = Regex::new(r"^(\\([\p{Han}\pL]+|\pP|\p{Cc})\s*)+ad.relax?a+好?$")
        .unwrap();
    c.bench_function("regex simple", |b| {
        b.iter(|| {
            black_box(
                re.is_match(r"\controlsequence控制序列\.\。\\addrelaxaaaaa"),
            )
        })
    });
}

fn regtex_tl_simple(c: &mut Criterion) {
    let re =
        RegTEx::new(r"^(\c{([\p{Han}\pL]+|\pP|\p{Cc})}\s*)+ad.relax?a+好?$")
            .unwrap();
    let mut ctab = CTab::document();
    ctab.extend(CTab::cjk_ideographs(CatCode::Letter));
    let tl = TokenList::parse(
        r"\controlsequence控制序列\.\。\\addrelaxaaaaa",
        &ctab,
    );
    let tb = tl.to_bytes();
    c.bench_function("regtex tl simple", |b| {
        b.iter(|| black_box(re.is_match(&tb)))
    });
}

criterion_group!(regtex, regex_simple, regtex_tl_simple);
criterion_main!(regtex);
