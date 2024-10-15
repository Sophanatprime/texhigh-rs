use crate::types::*;

pub struct Span {
    start: usize, // inclusive
    end: usize,   // exclusive
}

pub enum HighItemType<'a> {
    CS(&'a ControlSequence),
    Punct(char),
    Escaped(char),
}

pub struct HighItem<'a> {
    value: HighItemType<'a>,
    pos: [Span; 2],
    class: u32,
}
