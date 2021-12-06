use std::env;

pub fn read_file(day: usize) -> String {
    std::fs::read_to_string(env::var("AOC_INPUT").unwrap_or_else(|_| format!("inputs/day{:0>2}", day))).unwrap()
}

#[inline]
pub fn parse_nums(l: &str) -> Vec<usize> {
    l.lines().map(parse_num).collect()
}

#[inline]
pub fn parse_nums_comma(l: &str) -> Vec<usize> {
    l.split(',').map(parse_num).collect()
}

fn parse_num(s: &str) -> usize {
    s.trim().parse().unwrap_or_else(|e| panic!("Invalid number {s}: {e:?}"))
}
