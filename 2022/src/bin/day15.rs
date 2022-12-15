#![feature(test)]
extern crate test;
use aoc2022::{boilerplate, common::*};

const DAY: usize = 15;
type Parsed = Vec<usize>;

fn parse_input(raw: &str) -> Parsed {
    parse_nums(raw)
}

fn part1(parsed: &Parsed, row: i64) -> usize {
    unimplemented!()
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "",
    tests: {
        part1: { TEST_INPUT, 10 => 0 },
        part2: { TEST_INPUT => 0 },
    },
    bench1(2_000_000) == 0,
    bench2 == 0,
    bench_parse: Vec::len => 0,
}
