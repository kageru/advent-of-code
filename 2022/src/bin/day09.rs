#![feature(test)]
extern crate test;
use fnv::FnvHashSet as HashSet;

use aoc2022::{boilerplate, common::*};

const DAY: usize = 9;
type Parsed = Vec<(u8, usize)>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines().map(|l| (l.as_bytes()[0], parse_num(&l[2..]))).collect()
}

fn part1(parsed: &Parsed) -> usize {
    rope::<2>(parsed)
}

fn part2(parsed: &Parsed) -> usize {
    rope::<10>(parsed)
}

fn rope<const KNOTS: usize>(parsed: &Parsed) -> usize {
    let mut knots = [(0i32, 0i32); KNOTS];
    let mut positions = HashSet::default();
    positions.insert((0, 0));
    for &(dir, n) in parsed {
        'outer: for _ in 0..n {
            match dir {
                b'U' => knots[0].1 += 1,
                b'D' => knots[0].1 -= 1,
                b'L' => knots[0].0 -= 1,
                b'R' => knots[0].0 += 1,
                _ => unreachable!(),
            }
            for i in 0..KNOTS - 1 {
                if let Some(p) = step_towards(knots[i + 1], knots[i]) {
                    knots[i + 1] = p;
                } else {
                    continue 'outer;
                }
            }
            positions.insert(knots[KNOTS - 1]);
        }
    }
    positions.len()
}

#[inline]
fn step_towards(tail: (i32, i32), head: (i32, i32)) -> Option<(i32, i32)> {
    let xdiff = head.0 - tail.0;
    let ydiff = head.1 - tail.1;
    match (xdiff, ydiff) {
        (-1 | 0 | 1, -1 | 0 | 1) => None,
        _ => Some((tail.0 + xdiff.signum(), tail.1 + ydiff.signum())),
    }
}

boilerplate! {
    TEST_INPUT == "\
        R 4\n\
        U 4\n\
        L 3\n\
        D 1\n\
        R 4\n\
        D 1\n\
        L 5\n\
        R 2\
    ",
    tests: {
        part1: { TEST_INPUT => 13 },
        part2: {
            TEST_INPUT => 1,
            "R 5\n\
            U 8\n\
            L 8\n\
            D 3\n\
            R 17\n\
            D 10\n\
            L 25\n\
            U 20" => 36,
        },
    },
    bench1 == 6367,
    bench2 == 2536,
    bench_parse: Vec::len => 2000,
}
