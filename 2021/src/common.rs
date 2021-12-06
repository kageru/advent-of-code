use std::env;

pub fn read_file(day: usize) -> String {
    std::fs::read_to_string(env::var("AOC_INPUT").unwrap_or_else(|_| format!("inputs/day{:0>2}", day))).unwrap()
}

pub fn parse_nums(l: &str) -> Vec<usize> {
    l.lines().map(parse_num).collect()
}

pub fn parse_nums_comma(l: &str) -> Vec<usize> {
    l.trim().split(',').map(parse_num).collect()
}

#[inline]
fn parse_num(s: &str) -> usize {
    s.parse().unwrap_or_else(|e| panic!("Invalid number {s}: {e:?}"))
}
