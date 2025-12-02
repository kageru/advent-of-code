#![feature(test)]
extern crate test;
use aoc2025::{boilerplate, common::*};

const DAY: usize = 2;
type I = usize;
type Parsed = Vec<(I, I)>;

fn parse_input(raw: &str) -> Parsed {
    raw.trim().split(',').filter_map(|s| s.split_once('-')).map(|(start, end)| (parse_num(start), parse_num(end))).collect()
}

fn part1(parsed: &Parsed) -> usize {
    parsed.iter().map(|&(start, end)| (start..=end).filter(is_invalid).sum::<I>()).sum()
}

fn is_invalid(n: &I) -> bool {
    let num_digits = n.ilog10() as usize + 1;
    if num_digits.is_odd() {
        return false;
    }
    let s = n.to_string();
    let (left, right) = s.as_bytes().split_at(num_digits / 2);
    left == right
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
    for tests: {
        part1: { TEST_INPUT => 1227775554 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 28846518423,
    bench2 == 0,
    bench_parse: Vec::len => 38,
}
