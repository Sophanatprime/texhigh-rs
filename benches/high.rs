/* high.rs
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

use criterion::{criterion_group, criterion_main, Criterion};
use std::hint::black_box;

use std::{fs::File, io::Read, sync::Arc};

use texhigh::{
    config::{HighConfig, LexerType},
    high::{Null, StandardFormatter},
    tokenlist::{SourcedFormatter, SourcedTokenList},
    types::{CTab, CTabSet, CatCodeStack, TokenList},
};

fn high_large_texfile_no_out(c: &mut Criterion) {
    let mut s = String::new();
    File::open(r"benches/expl3-code.tex")
        .unwrap()
        .read_to_string(&mut s)
        .unwrap();
    let tl = TokenList::parse(s, &CTab::latex3package());
    let atl = Arc::new(tl);
    let cf = HighConfig::default();
    c.bench_function("high large texfile no out", |b| {
        b.iter(|| {
            let f = StandardFormatter::new(&cf, atl.clone());
            black_box(f.format_now(&mut Null).unwrap());
        })
    });
}

fn high_mid_texfile_no_out(c: &mut Criterion) {
    let mut s = String::new();
    File::open(r"benches/texhigh.tex")
        .unwrap()
        .read_to_string(&mut s)
        .unwrap();
    let tl = TokenList::parse(s, &CTab::latex3package());
    let atl = Arc::new(tl);
    let cf = HighConfig::default();
    c.bench_function("high mid texfile no out", |b| {
        b.iter(|| {
            let f = StandardFormatter::new(&cf, atl.clone());
            black_box(f.format_now(&mut Null).unwrap());
        })
    });
}

fn high_small_texfile_no_out(c: &mut Criterion) {
    let mut s = String::new();
    File::open(r"benches/list.tex").unwrap().read_to_string(&mut s).unwrap();
    let tl = TokenList::parse(s, &CTab::latex3package());
    let atl = Arc::new(tl);
    let cf = HighConfig::default();
    c.bench_function("high small texfile no out", |b| {
        b.iter(|| {
            let f = StandardFormatter::new(&cf, atl.clone());
            black_box(f.format_now(&mut Null).unwrap());
        })
    });
}

fn sourced_high_large_texfile_no_out(c: &mut Criterion) {
    let mut s = String::new();
    File::open(r"benches/expl3-code.tex")
        .unwrap()
        .read_to_string(&mut s)
        .unwrap();
    let lexer = LexerType::default();
    let ctabset = CTabSet::new_empty();
    let mut cat = CatCodeStack::new();
    cat.push(CTab::latex3package());
    let stl = SourcedTokenList::parse(s.into(), &mut cat, (&lexer, &ctabset));
    let atl = Arc::new(stl);
    let cf = HighConfig::default();
    c.bench_function("sourced high large texfile no out", |b| {
        b.iter(|| {
            let f = SourcedFormatter::new(&cf, atl.clone());
            black_box(f.format_now(&mut Null).unwrap());
        })
    });
}

fn sourced_high_mid_texfile_no_out(c: &mut Criterion) {
    let mut s = String::new();
    File::open(r"benches/texhigh.tex")
        .unwrap()
        .read_to_string(&mut s)
        .unwrap();
    let lexer = LexerType::default();
    let ctabset = CTabSet::new_empty();
    let mut cat = CatCodeStack::new();
    cat.push(CTab::latex3package());
    let tl = SourcedTokenList::parse(s.into(), &mut cat, (&lexer, &ctabset));
    let atl = Arc::new(tl);
    let cf = HighConfig::default();
    c.bench_function("sourced high mid texfile no out", |b| {
        b.iter(|| {
            let f = SourcedFormatter::new(&cf, atl.clone());
            black_box(f.format_now(&mut Null).unwrap());
        })
    });
}

fn sourced_high_small_texfile_no_out(c: &mut Criterion) {
    let mut s = String::new();
    File::open(r"benches/list.tex").unwrap().read_to_string(&mut s).unwrap();
    let lexer = LexerType::default();
    let ctabset = CTabSet::new_empty();
    let mut cat = CatCodeStack::new();
    cat.push(CTab::latex3package());
    let tl = SourcedTokenList::parse(s.into(), &mut cat, (&lexer, &ctabset));
    let atl = Arc::new(tl);
    let cf = HighConfig::default();
    c.bench_function("sourced high small texfile no out", |b| {
        b.iter(|| {
            let f = SourcedFormatter::new(&cf, atl.clone());
            black_box(f.format_now(&mut Null).unwrap());
        })
    });
}

criterion_group!(
    high,
    high_large_texfile_no_out,
    sourced_high_large_texfile_no_out,
    high_mid_texfile_no_out,
    sourced_high_mid_texfile_no_out,
    high_small_texfile_no_out,
    sourced_high_small_texfile_no_out
);
criterion_main!(high);
