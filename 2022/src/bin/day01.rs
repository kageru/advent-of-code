#![feature(binary_heap_into_iter_sorted, test)]
extern crate test;
use std::collections::BinaryHeap;

use aoc2022::{boilerplate, common::*};

const DAY: usize = 1;
type Parsed = BinaryHeap<usize>;

fn parse_input(raw: &str) -> Parsed {
    raw.split("\n\n").map(|elf| elf.lines().map(parse_num::<usize>).sum()).collect()
}

fn part1(parsed: &Parsed) -> usize {
    *parsed.peek().unwrap()
}

fn part2(parsed: &Parsed) -> usize {
    parsed.clone().into_iter_sorted().take(3).sum()
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
    bench_parse: BinaryHeap::len => 242,
}
