/* bench.rs
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

use memchr::{memchr, memmem};
use std::{fs::File, io::Read};

fn find_tab_small(c: &mut Criterion) {
    let mut s = String::new();
    File::open(r"benches/list.tex").unwrap().read_to_string(&mut s).unwrap();
    c.bench_function("find tab small", |b| {
        b.iter(|| {
            s.contains(black_box('\t'));
        })
    });
}

fn find_tab_small_memchr(c: &mut Criterion) {
    let mut s = String::new();
    File::open(r"benches/list.tex").unwrap().read_to_string(&mut s).unwrap();
    c.bench_function("find tab small memchr", |b| {
        b.iter(black_box(|| memchr(b'\t', s.as_bytes()).is_some()))
    });
}

fn find_tab_small_memmem(c: &mut Criterion) {
    let mut s = String::new();
    File::open(r"benches/list.tex").unwrap().read_to_string(&mut s).unwrap();
    c.bench_function("find tab small memmem", |b| {
        b.iter(black_box(|| memmem::find(s.as_bytes(), b"\t").is_some()))
    });
}

fn find_tab_large(c: &mut Criterion) {
    let mut s = String::new();
    File::open(r"benches/expl3-code.tex")
        .unwrap()
        .read_to_string(&mut s)
        .unwrap();
    c.bench_function("find tab large", |b| {
        b.iter(|| {
            s.contains(black_box('\t'));
        })
    });
}

fn find_tab_large_memchr(c: &mut Criterion) {
    let mut s = String::new();
    File::open(r"benches/expl3-code.tex")
        .unwrap()
        .read_to_string(&mut s)
        .unwrap();
    c.bench_function("find tab large", |b| {
        b.iter(black_box(|| memchr(b'\t', s.as_bytes()).is_some()))
    });
}

fn find_tab_large_memmem(c: &mut Criterion) {
    let mut s = String::new();
    File::open(r"benches/expl3-code.tex")
        .unwrap()
        .read_to_string(&mut s)
        .unwrap();
    c.bench_function("find tab large memmem", |b| {
        b.iter(black_box(|| memmem::find(s.as_bytes(), b"\t").is_some()))
    });
}

criterion_group!(
    bench,
    find_tab_small,
    find_tab_small_memchr,
    find_tab_small_memmem,
    find_tab_large,
    find_tab_large_memchr,
    find_tab_large_memmem
);
criterion_main!(bench);
