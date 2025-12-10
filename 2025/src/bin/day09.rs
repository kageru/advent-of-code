#![feature(test)]
extern crate test;
use aoc2025::{boilerplate, common::*};
use itertools::Itertools;

const DAY: usize = 9;
type I = usize;
type Parsed = Vec<(I, I)>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines().filter_map(|l| l.split_once(',')).map(|(x, y)| (parse_num(x), parse_num(y))).collect()
}

fn part1(parsed: &Parsed) -> I {
    parsed.iter().tuple_combinations().map(|((x1, y1), (x2, y2))| (x1.abs_diff(*x2) + 1) * (y1.abs_diff(*y2) + 1)).max().unwrap()
}

fn part2(parsed: &Parsed) -> I {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "\
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3"
    for tests: {
        part1: { TEST_INPUT => 50 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 4750092396,
    bench2 == 0,
    bench_parse: Vec::len => 496,
}
