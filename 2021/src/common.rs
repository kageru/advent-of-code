use std::env;

pub fn read_file(day: usize) -> String {
    std::fs::read_to_string(env::var("AOC_INPUT").unwrap_or_else(|_| format!("inputs/day{:0>2}", day))).unwrap()
}

#[inline]
pub fn parse_nums(l: &str) -> Vec<usize> {
    l.lines().filter_map(|n| n.parse().ok()).collect()
}

#[inline]
pub fn parse_nums_comma(l: &str) -> Vec<usize> {
    l.split(',').filter_map(|n| n.parse().ok()).collect()
}
