use rayon::prelude::*;
use regex::Regex;
use std::sync::Mutex;

enum Kind {
    Empty,
    Char(char),
    String(String),
    Regex(Regex),
}

fn find_matches_char(c: char, s: &str, offset: usize) -> Vec<(usize, usize)> {
    let mut matches = Vec::new();
    let mut byte_idx = 0;
    for ch in s.chars() {
        if ch == c {
            let start = offset + byte_idx;
            let end = start + ch.len_utf8();
            matches.push((start, end));
        }
        byte_idx += ch.len_utf8();
    }
    matches
}

fn find_matches_string(
    pattern: &str,
    s: &str,
    offset: usize,
) -> Vec<(usize, usize)> {
    let mut matches = Vec::new();
    let pattern_len = pattern.len();
    if pattern_len == 0 {
        return matches;
    }
    let mut start = 0;
    while start <= s.len() {
        match s[start ..].find(pattern) {
            Some(pos) => {
                let abs_pos = start + pos;
                let end = abs_pos + pattern_len;
                if end > s.len() {
                    break;
                }
                matches.push((offset + abs_pos, offset + end));
                start = end; // 不重叠，继续搜索
            }
            None => break,
        }
    }
    matches
}

fn find_matches_regex(
    re: &Regex,
    s: &str,
    offset: usize,
) -> Vec<(usize, usize)> {
    re.find_iter(s)
        .filter(|m| !m.is_empty())
        .map(|m| {
            let start = offset + m.start();
            let end = offset + m.end();
            (start, end)
        })
        .collect()
}

fn find_patterns(s: &str, kinds: &[Kind]) -> Vec<(usize, usize)> {
    let mut current_uncovered = vec![0 .. s.len()];
    let mut results = Vec::new();

    for (kind_idx, kind) in kinds.iter().enumerate() {
        if let Kind::Empty = kind {
            continue;
        }

        let mut new_uncovered = Vec::new();

        for range in current_uncovered {
            let start = range.start;
            let end = range.end;
            if start >= end {
                continue;
            }

            let s_sub = &s[start .. end];
            let matches = match kind {
                Kind::Char(c) => find_matches_char(*c, s_sub, start),
                Kind::String(s_str) => {
                    find_matches_string(s_str.as_str(), s_sub, start)
                }
                Kind::Regex(re) => find_matches_regex(re, s_sub, start),
                Kind::Empty => unreachable!(),
            };

            let mut sorted_matches = matches;
            sorted_matches.sort_by_key(|&(s, _)| s);

            let mut filtered_matches = Vec::new();
            let mut prev_end = start;
            for (match_start, match_end) in sorted_matches {
                if match_start >= prev_end && match_end <= end {
                    filtered_matches.push((match_start, match_end));
                    prev_end = match_end;
                }
            }

            let mut prev_segment_end = start;
            for &(ms, me) in &filtered_matches {
                if ms > prev_segment_end {
                    new_uncovered.push(prev_segment_end .. ms);
                }
                prev_segment_end = me;
                results.push((ms, me, kind_idx));
            }
            if prev_segment_end < end {
                new_uncovered.push(prev_segment_end .. end);
            }
        }

        current_uncovered = new_uncovered;
    }

    results.sort_by_key(|&(s, _, _)| s);
    results.into_iter().map(|(s, e, _)| (s, e)).collect()
}

struct ParallelResult {
    matches: Vec<(usize, usize, usize)>,
    uncovered: Vec<std::ops::Range<usize>>,
}
impl ParallelResult {
    fn new() -> Self {
        Self { matches: Vec::new(), uncovered: Vec::new() }
    }

    fn merge(mut self, other: Self) -> Self {
        self.matches.extend(other.matches);
        self.uncovered.extend(other.uncovered);
        self
    }
}

fn find_patterns_parallel(s: &str, kinds: &[Kind]) -> Vec<(usize, usize)> {
    let s_len = s.len();
    let mut current_uncovered = vec![0 .. s_len];
    let results = Mutex::new(Vec::new());

    for (kind_idx, kind) in kinds.iter().enumerate() {
        if let Kind::Empty = kind {
            continue;
        }

        // 并行处理所有当前未覆盖区间
        let processed: Vec<ParallelResult> = current_uncovered
            .par_iter()
            .map(|range| {
                let start = range.start;
                let end = range.end;
                if start >= end {
                    return ParallelResult::new();
                }

                let s_sub = &s[start .. end];
                let matches = match kind {
                    Kind::Char(c) => find_matches_char(*c, s_sub, start),
                    Kind::String(s_str) => {
                        find_matches_string(s_str, s_sub, start)
                    }
                    Kind::Regex(re) => find_matches_regex(re, s_sub, start),
                    Kind::Empty => unreachable!(),
                };

                // 处理匹配结果
                let mut sorted_matches = matches;
                sorted_matches.sort_by_key(|&(s, _)| s);

                let mut pr = ParallelResult::new();
                let mut prev_segment_end = start;

                for (ms, me) in sorted_matches {
                    if ms >= prev_segment_end && me <= end {
                        // 记录非重叠匹配
                        if ms > prev_segment_end {
                            pr.uncovered.push(prev_segment_end .. ms);
                        }
                        pr.matches.push((ms, me, kind_idx));
                        prev_segment_end = me;
                    }
                }

                if prev_segment_end < end {
                    pr.uncovered.push(prev_segment_end .. end);
                }

                pr
            })
            .collect();

        // 合并并行结果
        let merged = processed
            .into_iter()
            .fold(ParallelResult::new(), |acc, x| acc.merge(x));

        // 更新未覆盖区间和结果
        current_uncovered = merged.uncovered;
        results.lock().unwrap().extend(merged.matches);
    }

    // 最终排序处理
    let mut final_results = results.into_inner().unwrap();
    final_results.sort_by(|a, b| {
        a.0.cmp(&b.0).then_with(|| a.2.cmp(&b.2)) // 保证相同位置小索引优先
    });

    final_results.into_iter().map(|(s, e, _)| (s, e)).collect()
}

fn find_patterns_pointer(s: &str, kinds: &[Kind]) -> Vec<(usize, usize)> {
    let mut results = Vec::new();
    let mut index = 0;

    while index < s.len() {
        let chr = s[index ..].chars().next().unwrap();
        let curr_index = index;
        for kind in kinds {
            match kind {
                Kind::Empty => {}
                Kind::Char(c) => {
                    if chr == *c {
                        results.push((index, index + chr.len_utf8()));
                        index += chr.len_utf8();
                        break;
                    }
                }
                Kind::String(s_sub) => {
                    if s[index ..].starts_with(s_sub) {
                        results.push((index, index + s_sub.len()));
                        index += s_sub.len();
                        break;
                    }
                }
                Kind::Regex(re) => {
                    if let Some(m) = re.find(&s[index ..]) {
                        if m.start() == 0 {
                            results.push((index, index + m.range().len()));
                            index += m.range().len();
                            break;
                        }
                    }
                }
            }
        }
        if curr_index == index {
            index += chr.len_utf8();
        }
    }

    results
}


#[test]
fn test_pattern() {
    let kinds = &[
        Kind::Char('#'),
        Kind::String("->".to_string()),
        Kind::String("::=".to_string()),
        Kind::String(":=".to_string()),
        Kind::String("~>".to_string()),
        Kind::Regex(Regex::new(r"\p{Lo}").unwrap()),
    ];

    let s = r###"abcd#
let := abc + de;
const ::= [da + dl];
map ~> a;
好 + 我 := d;
"###;

    let timer = std::time::Instant::now();
    // for (start, end) in find_patterns(s, kinds) {
    //     println!("Pattern: {}", &s[start..end]);
    // }
    assert_eq!(find_patterns(s, kinds), find_patterns_parallel(s, kinds));
    assert_eq!(find_patterns(s, kinds), find_patterns_pointer(s, kinds));

    for _ in 0 .. 10_0000 {
        let _ = find_patterns(s, kinds);
    }
    println!("Takes {}ms", timer.elapsed().as_millis());

    let timer = std::time::Instant::now();
    for _ in 0 .. 10_0000 {
        let _ = find_patterns_parallel(s, kinds);
    }
    println!("Takes {}ms", timer.elapsed().as_millis());

    let timer = std::time::Instant::now();
    for _ in 0 .. 10_0000 {
        let _ = find_patterns_pointer(s, kinds);
    }
    println!("Takes {}ms", timer.elapsed().as_millis());
}
