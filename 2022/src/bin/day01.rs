#![feature(test)]
extern crate test;
use std::collections::BinaryHeap;

use aoc2022::{boilerplate, common::*};

const DAY: usize = 01;
type Parsed = Vec<usize>;

fn parse_input(raw: &str) -> Parsed {
    raw.split("\n\n").map(|elf| elf.lines().map(parse_num::<usize>).sum()).collect()
}

fn part1(parsed: &Parsed) -> usize {
    *parsed.iter().max().unwrap_or(&0)
}

fn part2(parsed: &Parsed) -> usize {
    let mut sorted: BinaryHeap<_> = parsed.iter().copied().collect();
    (0..3).map(|_| sorted.pop().unwrap()).sum()
}

boilerplate! {
    TEST_INPUT == "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000",
    tests: {
        part1: { TEST_INPUT => 24000 },
        part2: { TEST_INPUT => 45000 },
    },
    bench1 == 72017,
    bench2 == 212520,
    bench_parse: Vec::len => 242,
}
