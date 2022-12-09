#![feature(test)]
extern crate test;
use std::collections::HashSet;

use aoc2022::{boilerplate, common::*};

const DAY: usize = 9;
type Parsed = Vec<(u8, usize)>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines().map(|l| (l.as_bytes()[0], parse_num(&l[2..]))).collect()
}

fn part1(parsed: &Parsed) -> usize {
    let mut positions = HashSet::from([(0, 0)]);
    let mut head = (0, 0);
    let mut tail = (0, 0);
    for &(dir, n) in parsed {
        for _ in 0..n {
            match dir {
                b'U' => head.1 += 1,
                b'D' => head.1 -= 1,
                b'L' => head.0 -= 1,
                b'R' => head.0 += 1,
                _ => unreachable!(),
            }
            if step_towards(&mut tail, &head) {
                positions.insert(tail);
            }
        }
    }
    positions.len()
}

fn step_towards(tail: &mut (i32, i32), head: &(i32, i32)) -> bool {
    let xdiff = head.0 - tail.0;
    let ydiff = head.1 - tail.1;
    match (xdiff.abs(), ydiff.abs()) {
        (0 | 1, 0 | 1) => return false,
        (_, 0) => tail.0 += xdiff.signum(),
        (0, _) => tail.1 += ydiff.signum(),
        (1, _) | (_, 1) => {
            tail.0 += xdiff.signum();
            tail.1 += ydiff.signum();
        }
        _ => return false,
    }
    true
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2",
    tests: {
        part1: { TEST_INPUT => 13 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 6367,
    bench2 == 0,
    bench_parse: Vec::len => 2000,
}
