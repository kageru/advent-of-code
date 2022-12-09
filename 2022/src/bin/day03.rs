#![feature(test, iter_array_chunks)]
extern crate test;
use std::collections::HashSet;

use aoc2022::{boilerplate, common::*};

const DAY: usize = 3;
type Parsed = Vec<(HashSet<u8>, HashSet<u8>)>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines()
        .map(|line| {
            let bytes = line.as_bytes();
            let (a, b) = bytes.split_at(bytes.len() / 2);
            (a.iter().copied().collect(), b.iter().copied().collect())
        })
        .collect()
}

fn part1(parsed: &Parsed) -> usize {
    parsed.iter().map(|(a, b)| a.intersection(b).next().unwrap()).map(priority).sum()
}

fn priority(n: &u8) -> usize {
    (match n {
        b'a'..=b'z' => n - b'a' + 1,
        b'A'..=b'Z' => n - b'A' + 27,
        _ => unreachable!(),
    }) as usize
}

fn part2(parsed: &Parsed) -> usize {
    parsed
        .iter()
        .map(|(a, b)| a.union(b).collect::<HashSet<_>>())
        .array_chunks()
        .map(|[a, b, c]| *a.intersection(&b).copied().collect::<HashSet<_>>().intersection(&c).next().unwrap())
        .map(priority)
        .sum()
}

boilerplate! {
    TEST_INPUT == "\
        vJrwpWtwJgWrhcsFMMfFFhFp\n\
        jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
        PmmdzqPrVvPwwTWBwg\n\
        wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
        ttgJtRGJQctTZtZT\n\
        CrZsJsPPZsGzwwsLwLmpwMDw\
    ",
    tests: {
        part1: { TEST_INPUT => 157 },
        part2: { TEST_INPUT => 70 },
    },
    bench1 == 8202,
    bench2 == 2864,
    bench_parse: Vec::len => 300,
}
