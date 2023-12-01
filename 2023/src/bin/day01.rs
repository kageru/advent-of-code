#![feature(test)]
extern crate test;
use aoc2023::{boilerplate, common::*};

const DAY: usize = 1;
type Parsed<'a> = Vec<&'a str>;

fn parse_input(raw: &str) -> Parsed<'_> {
    raw.lines().collect()
}

fn part1(parsed: &Parsed) -> usize {
    parsed
        .iter()
        .map(|l| {
            let l: Vec<_> = l.bytes().filter_map(|b| b.is_ascii_digit().then_some((b - b'0') as usize)).collect();
            l[0] * 10 + l.last().unwrap()
        })
        .sum()
}

const DIGITS: [&str; 9] = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];

fn find_digit(s: &str) -> Option<usize> {
    if s.as_bytes()[0].is_ascii_digit() {
        return Some((s.as_bytes()[0] - b'0') as usize);
    }
    DIGITS.iter().zip(1usize..).find_map(|(&d, i)| s.starts_with(d).then_some(i))
}

fn part2(parsed: &Parsed) -> usize {
    parsed
        .iter()
        .map(|l| {
            let first = (0..l.len()).find_map(|s| find_digit(&l[s..])).unwrap();
            let last = (0..l.len()).rev().find_map(|s| find_digit(&l[s..])).unwrap();
            first * 10 + last
        })
        .sum()
}

#[cfg(test)]
const TEST_INPUT_2: &str = "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen";

boilerplate! {
    TEST_INPUT == "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet",
    tests: {
        part1: { TEST_INPUT => 142 },
        part2: { TEST_INPUT_2 => 281 },
    },
    bench1 == 54916,
    bench2 == 54728,
    bench_parse: Vec::len => 1000,
}
