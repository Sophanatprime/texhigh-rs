use criterion::{criterion_group, criterion_main, Criterion};
use std::hint::black_box;

use std::fs::File;
use std::io::Read;
use std::sync::Arc;

use texhigh::config::LexerType;
use texhigh::tokenlist::SourcedTokenList;
use texhigh::types::{CTab, CTabSet, CatCodeStack, TokenList};

fn parse_large_texfile(c: &mut Criterion) {
    let mut s = String::new();
    File::open(r"benches/expl3-code.tex")
        .unwrap()
        .read_to_string(&mut s)
        .unwrap();
    let ctab = &CTab::latex3package();
    c.bench_function("parse large texfile", |b| {
        b.iter(|| {
            black_box({
                let _ = TokenList::parse(&s, ctab);
            })
        })
    });
}

fn parse_mid_texfile(c: &mut Criterion) {
    let mut s = String::new();
    File::open(r"benches/texhigh.tex")
        .unwrap()
        .read_to_string(&mut s)
        .unwrap();
    let ctab = &CTab::latex3package();
    c.bench_function("parse mid texfile", |b| {
        b.iter(|| {
            black_box({
                let _ = TokenList::parse(&s, ctab);
            })
        })
    });
}

fn parse_small_texfile(c: &mut Criterion) {
    let mut s = String::new();
    File::open(r"benches/list.tex").unwrap().read_to_string(&mut s).unwrap();
    let ctab = &CTab::package();
    c.bench_function("parse small texfile", |b| {
        b.iter(|| {
            black_box({
                let _ = TokenList::parse(&s, ctab);
            })
        })
    });
}

fn parse_large_texfile_sourced(c: &mut Criterion) {
    let mut s = String::new();
    File::open(r"benches/expl3-code.tex")
        .unwrap()
        .read_to_string(&mut s)
        .unwrap();
    let s = Arc::new(s);
    c.bench_function("parse large texfile sourced", |b| {
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
        })
    });
}

fn parse_mid_texfile_sourced(c: &mut Criterion) {
    let mut s = String::new();
    File::open(r"benches/texhigh.tex")
        .unwrap()
        .read_to_string(&mut s)
        .unwrap();
    let s = Arc::new(s);
    c.bench_function("parse mid texfile sourced", |b| {
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
        })
    });
}

fn parse_small_texfile_sourced(c: &mut Criterion) {
    let mut s = String::new();
    File::open(r"benches/list.tex").unwrap().read_to_string(&mut s).unwrap();
    let s = Arc::new(s);
    c.bench_function("parse small texfile sourced", |b| {
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
        })
    });
}

criterion_group!(
    tl,
    parse_large_texfile,
    parse_large_texfile_sourced,
    parse_mid_texfile,
    parse_mid_texfile_sourced,
    parse_small_texfile,
    parse_small_texfile_sourced
);
criterion_main!(tl);
