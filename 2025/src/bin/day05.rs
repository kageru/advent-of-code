#![feature(test, get_disjoint_mut_helpers)]
extern crate test;
use aoc2025::{boilerplate, common::*};
use core::slice::GetDisjointMutIndex;
use std::ops::RangeInclusive;

const DAY: usize = 5;
type I = usize;
type Parsed = (Vec<RangeInclusive<I>>, Vec<I>);

fn parse_input(raw: &str) -> Parsed {
    let (fresh, items) = raw.split_once("\n\n").unwrap();
    let fresh = fresh.lines().filter_map(|l| l.split_once('-')).map(|(a, b)| parse_num(a)..=parse_num(b)).collect();
    (fresh, parse_nums_separator(items, '\n'))
}

fn part1((fresh, items): &Parsed) -> usize {
    items.iter().filter(|i| fresh.iter().any(|range| range.contains(i))).count()
}

fn part2((ranges, _): &Parsed) -> usize {
    let mut ranges = ranges.clone();
    ranges.sort_unstable_by_key(|r| *r.start());
    let mut i = 0;
    while i < ranges.len() {
        let current = ranges[i].clone();
        match ranges.iter().enumerate().skip(i + 1).find(|(_, r)| r.is_overlapping(&current)) {
            Some((idx, range)) => {
                ranges[i] = *current.start()..=*(current.end().max(range.end()));
                ranges.remove(idx);
            }
            None => i += 1,
        }
    }
    ranges.into_iter().map(|r| r.count()).sum()
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
        part2: { TEST_INPUT => 14 },
    },
    bench1 == 558,
    bench2 == 344813017450467,
    bench_parse: |(a, b): &Parsed| (a.len(), b.len()) => (187, 1000),
}
