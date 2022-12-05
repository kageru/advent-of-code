#![feature(test)]
extern crate test;
use std::ops::RangeInclusive;

use aoc2022::{boilerplate, common::*};

const DAY: usize = 4;
type Parsed = Vec<(RangeInclusive<usize>, RangeInclusive<usize>)>;

fn parse_range(r: &str) -> RangeInclusive<usize> {
    let (a, b) = r.split_once('-').unwrap();
    parse_num(a)..=parse_num(b)
}

fn parse_input(raw: &str) -> Parsed {
    raw.lines()
        .map(|line| {
            let (a, b) = line.split_once(',').unwrap();
            (parse_range(a), parse_range(b))
        })
        .collect()
}

fn part1(parsed: &Parsed) -> usize {
    parsed.iter().filter(|(a, b)| (a.start() <= b.start() && a.end() >= b.end()) || (b.start() <= a.start() && b.end() >= a.end())).count()
}

fn part2(parsed: &Parsed) -> usize {
    parsed.iter().filter(|(a, b)| a.contains(b.start()) || a.contains(b.end()) || b.contains(a.start()) || b.contains(a.end())).count()
}

boilerplate! {
    TEST_INPUT == "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8",
    tests: {
        part1: { TEST_INPUT => 2 },
        part2: { TEST_INPUT => 4 },
    },
    bench1 == 494,
    bench2 == 833,
    bench_parse: Vec::len => 1000,
}
