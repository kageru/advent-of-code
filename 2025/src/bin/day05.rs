#![feature(test)]
extern crate test;
use aoc2025::{boilerplate, common::*};
use std::ops::RangeInclusive;

const DAY: usize = 5;
type I = i64;
type Parsed = (Vec<RangeInclusive<I>>, Vec<I>);

fn parse_input(raw: &str) -> Parsed {
    let (fresh, items) = raw.split_once("\n\n").unwrap();
    let fresh = fresh.lines().filter_map(|l| l.split_once('-')).map(|(a, b)| parse_num(a)..=parse_num(b)).collect();
    (fresh, parse_nums_separator(items, '\n'))
}

fn part1((fresh, items): &Parsed) -> usize {
    items.iter().filter(|i| fresh.iter().any(|range| range.contains(i))).count()
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "\
3-5
10-14
16-20
12-18

1
5
8
11
17
32"
    for tests: {
        part1: { TEST_INPUT => 3 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 558,
    bench2 == 0,
    bench_parse: |(a, b): &Parsed| (a.len(), b.len()) => (187, 1000),
}
