#![feature(iter_array_chunks, test)]
extern crate test;
use aoc2022::{boilerplate, common::*};

const DAY: usize = 2;

fn round([other, _, own, _]: [u8; 4]) -> usize {
    (match other.wrapping_sub(own - b'X' + b'A') {
        0 => 3 + own - b'W',
        1 | 254 => own - b'W',
        _ => 6 + own - b'W',
    }) as usize
}

fn round_p2([other, _, own, _]: [u8; 4]) -> usize {
    (match (own, other) {
        (b'Y', _) => other - b'A' + 4,
        (b'X', b'A') => 3,
        (b'X', _) => other - b'A',
        (_, b'C') => 7,
        _ => other - b'A' + 8,
    }) as usize
}

fn parse_input(raw: &str) -> &str {
    raw
}

fn part1(parsed: &str) -> usize {
    parsed.bytes().array_chunks().map(round).sum()
}

fn part2(parsed: &str) -> usize {
    parsed.bytes().array_chunks().map(round_p2).sum()
}

boilerplate! {
    TEST_INPUT == "A Y
B X
C Z
",
    tests: {
        part1: { TEST_INPUT => 15 },
        part2: {
            TEST_INPUT => 12,
            "A X\n" => 3,
            "C X\n" => 2,
            "A Y\n" => 4,
            "A Z\n" => 8,
        },
    },
    bench1 == 13268,
    bench2 == 15508,
    bench_parse: str::len => 10000,
}
