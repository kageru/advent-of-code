#![feature(test)]
extern crate test;
use aoc2025::{boilerplate, common::*};
use itertools::Itertools;

const DAY: usize = 3;
type I = u32;
type Parsed = Vec<Vec<u8>>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines().map(|l| l.as_bytes().iter().map(|b| b - b'0').collect()).collect()
}

fn part1(parsed: &Parsed) -> I {
    parsed.iter().map(|batch| highest_battery(batch)).sum()
}

fn highest_battery(batch: &[u8]) -> I {
    let max = batch.iter().rev().skip(1).rev().max().unwrap();
    (max * 10 + batch.iter().skip(batch.iter().position(|n| n == max).unwrap() + 1).max().unwrap()) as I
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "\
987654321111111
811111111111119
234234234234278
818181911112111"
    for tests: {
        part1: { TEST_INPUT => 357 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 17087,
    bench2 == 0,
    bench_parse: Vec::len => 200,
}
