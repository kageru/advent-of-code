#![feature(test)]
extern crate test;
use aoc2025::{boilerplate, common::*};

const DAY: usize = 1;
type I = i32;
type Parsed = Vec<I>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines()
        .map(|l| match l.as_bytes()[0] {
            b'L' => -parse_num::<I>(&l[1..]),
            b'R' => parse_num(&l[1..]),
            _ => unreachable!(),
        })
        .collect()
}

fn part1(parsed: &Parsed) -> usize {
    parsed
        .iter()
        .scan(50, |pos, rot| {
            *pos = (*pos + rot + 100) % 100;
            Some(*pos)
        })
        .filter(|&n| n == 0)
        .count()
}

fn part2(parsed: &Parsed) -> usize {
    parsed
        .iter()
        .scan(50, |pos, rot| {
            *pos += rot;
            Some(match *pos {
                (..0) => {
                    *pos = (*pos + 100) % 100;
                    true
                }
                (0..=99) => false,
                (100..) => {
                    *pos -= 100;
                    true
                }
            })
        })
        .inspect(|b| println!("{b}"))
        .filter(|&b| b)
        .count()
}

boilerplate! {
    TEST_INPUT == "\
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82"
    for tests: {
        part1: { TEST_INPUT => 3 },
        part2: { TEST_INPUT => 6 },
    },
    bench1 == 1158,
    bench2 == 0, // 3418 too low
    bench_parse: Vec::len => 4659,
}
