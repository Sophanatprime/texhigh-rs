/* regtex.rs
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
