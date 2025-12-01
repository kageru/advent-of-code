#![feature(test)]
extern crate test;
use aoc2025::{boilerplate, common::*};
use itertools::Itertools;

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
            *pos += rot;
            Some(*pos)
        })
        .filter(|&n| n % 100 == 0)
        .count()
}

fn part2(parsed: &Parsed) -> I {
    parsed
        .iter()
        .copied()
        .coalesce(|a, b| if (a >= 0) == (b >= 0) /* faster than signum() */ { Ok(a + b) } else { Err((a, b)) })
        .scan(50, |pos, rot| {
            *pos += rot;
            Some(match *pos {
                p @ ..0 => {
                    let rotations = p.abs() / 100 + 1;
                    *pos = (p + rotations * 100) % 100; // `% 100` because this can be exactly 100 after the movement
                    rotations
                }
                0 => 1,
                1..=99 => 0,
                p @ 100.. => {
                    let rotations = p / 100;
                    *pos = p - rotations * 100;
                    rotations - (*pos == 0) as I // if we end on exactly 0, that will be counted as a cross by the next left movement
                }
            })
        })
        .sum()
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
    bench2 == 6860,
    bench_parse: Vec::len => 4659,
}
