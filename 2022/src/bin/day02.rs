#![feature(iter_array_chunks, test)]
extern crate test;
use aoc2022::{boilerplate, common::*};

const DAY: usize = 2;

#[rustfmt::skip]
fn round(input: [u8; 4]) -> usize {
    match (input[0], input[2]) {
        (b'A', b'X') => 4, (b'A', b'Y') => 8, (b'A', b'Z') => 3, (b'B', b'X') => 1, (b'B', b'Y') => 5, (b'B', b'Z') => 9, (b'C', b'X') => 7, (b'C', b'Y') => 2, (b'C', b'Z') => 6,
        _ => unreachable!(),
    }
}

#[rustfmt::skip]
fn round_p2(input: [u8; 4]) -> usize {
    match (input[0], input[2]) {
        (b'A', b'X') => 3, (b'A', b'Y') => 4, (b'A', b'Z') => 8, (b'B', b'X') => 1, (b'B', b'Y') => 5, (b'B', b'Z') => 9, (b'C', b'X') => 2, (b'C', b'Y') => 6, (b'C', b'Z') => 7,
        _ => unreachable!(),
    }
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
        part2: { TEST_INPUT => 12 },
    },
    bench1 == 13268,
    bench2 == 15508,
    bench_parse: str::len => 10000,
}
