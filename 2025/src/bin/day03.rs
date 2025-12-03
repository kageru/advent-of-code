#![feature(test)]
extern crate test;
use aoc2025::{boilerplate, common::*};
use itertools::Itertools;

const DAY: usize = 3;
type I = usize;
type Parsed = Vec<Vec<u8>>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines().map(|l| l.as_bytes().iter().map(|b| b - b'0').collect()).collect()
}

fn part1(parsed: &Parsed) -> I {
    solve(parsed, 2)
}

fn part2(parsed: &Parsed) -> I {
    solve(parsed, 12)
}

fn solve(parsed: &Parsed, digits: usize) -> I {
    parsed.iter().map(|batch| max_joltage(batch, digits, 0)).sum()
}

fn max_joltage(batch: &[u8], remaining_digits: usize, sum: I) -> I {
    if remaining_digits == 0 {
        return sum;
    }
    let max_pos = batch.len() - batch[..=(batch.len() - remaining_digits)].iter().rev().position_max().unwrap() - remaining_digits;
    max_joltage(&batch[max_pos + 1..], remaining_digits - 1, sum * 10 + batch[max_pos] as I)
}

boilerplate! {
    TEST_INPUT == "\
987654321111111
811111111111119
234234234234278
818181911112111"
    for tests: {
        part1: { TEST_INPUT => 357 },
        part2: {
            TEST_INPUT => 3121910778619,
            "987654321111111" => 987654321111,
            "811111111111119" => 811111111119,
            "234234234234278" => 434234234278,
            "818181911112111" => 888911112111,
        },
    },
    bench1 == 17087,
    bench2 == 169019504359949,
    bench_parse: Vec::len => 200,
}
