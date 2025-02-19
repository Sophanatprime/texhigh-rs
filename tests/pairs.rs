use regex::Regex;
use std::collections::HashMap;

enum Kind {
    Empty,
    Char(char),
    String(String),
    Regex(Regex),
}

struct EnvMatcher<'k> {
    // 预处理结构加速匹配
    start_map: Vec<(&'k Kind, usize)>, // (开始模式, 对应结束模式的索引)
    end_map: HashMap<usize, &'k Kind>, // 结束模式索引 -> 实际模式
    all_starts: Vec<&'k Kind>,         // 所有独立开始模式
    all_ends: Vec<&'k Kind>,           // 所有独立结束模式
}

impl<'k> EnvMatcher<'k> {
    fn new(pairs: &'k [(Kind, Kind)]) -> Self {
        let mut start_map = Vec::new();
        let mut end_map = HashMap::new();
        let mut all_starts = Vec::new();
        let mut all_ends = Vec::new();

        for (idx, (start, end)) in pairs.into_iter().enumerate() {
            start_map.push((start, idx));
            end_map.insert(idx, end);
            all_starts.push(start);
            all_ends.push(end);
        }

        Self { start_map, end_map, all_starts, all_ends }
    }

    fn find_envs(&self, s: &str) -> Vec<((usize, usize), (usize, usize))> {
        let mut stack: Vec<(usize, usize, usize)> = Vec::new(); // (start_pos, start_end, kind_idx)
        let mut results = Vec::new();
        let mut index = 0;

        while index < s.len() {
            // 优先检查栈顶环境的结束标记
            let mut matched_end = false;
            if let Some(&(start_pos, start_end, kind_idx)) = stack.last() {
                if let Some(end_kind) = self.end_map.get(&kind_idx) {
                    if let Some(end_len) = self.match_kind(end_kind, s, index)
                    {
                        let end_start = index;
                        let end_end = index + end_len;
                        results.push((
                            (start_pos, start_end),
                            (end_start, end_end),
                        ));
                        stack.pop();
                        index = end_end;
                        matched_end = true;
                    }
                }
            }

            if matched_end {
                continue;
            }

            // 扫描所有可能的起始标记（按索引顺序）
            let mut best_start = None;
            for (start_kind, kind_idx) in &self.start_map {
                if let Some(start_len) = self.match_kind(start_kind, s, index)
                {
                    best_start = Some((index, index + start_len, *kind_idx));
                    break; // 取索引最小的匹配
                }
            }

            if let Some((start_pos, start_end, kind_idx)) = best_start {
                stack.push((start_pos, start_end, kind_idx));
                index = start_end;
            } else {
                // 安全移动至下一个字符起始位置
                index +=
                    s[index ..].chars().next().map_or(1, |c| c.len_utf8());
            }
        }

        // 按起始位置排序结果
        results.sort_by(|a, b| (a.0).0.cmp(&(b.0).0));
        results
    }

    fn match_kind(&self, kind: &Kind, s: &str, pos: usize) -> Option<usize> {
        let substr = &s[pos ..];
        match kind {
            Kind::Empty => None,
            Kind::Char(c) => substr
                .chars()
                .next()
                .filter(|&ch| ch == *c)
                .map(|ch| ch.len_utf8()),
            Kind::String(s_str) => {
                substr.starts_with(s_str).then(|| s_str.len())
            }
            Kind::Regex(re) => {
                re.find(substr).filter(|m| m.start() == 0).map(|m| m.end())
            }
        }
    }
}

#[test]
fn test_pairs() {
    let pairs = &[
        (Kind::Char('#'), Kind::Char('#')),
        (
            Kind::String(r"\starttype".to_string()),
            Kind::String(r"\stoptype".to_string()),
        ),
        (Kind::String(r"\begin{doc}".to_string()), Kind::Char('X')),
        (
            Kind::Regex(Regex::new(r"^\\Start\{[^\}]+\}").unwrap()),
            Kind::Regex(Regex::new(r"\\end\{[^\}]+\}").unwrap()),
        ),
        (Kind::String(r"e}".to_string()), Kind::String(r"e}".to_string())),
    ];
    let s = r###"
    #avb#
    \starttype
    \Start{type}
    aaaa
    \end{type}
    \begin{doc}aaaaX\end{}
    \stoptype
"###;

    let e = EnvMatcher::new(pairs);
    let res = e.find_envs(s);
    for (st, et) in res {
        println!("S: {}, E: {}", &s[st.0 .. st.1], &s[et.0 .. et.1]);
    }

    let timer = std::time::Instant::now();
    for _ in 0 .. 10_0000 {
        let _ = e.find_envs(s);
    }
    println!("Takes {}ms", timer.elapsed().as_millis());
}
