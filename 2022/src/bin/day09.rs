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
        for _ in 0..n {
            match dir {
                b'U' => knots[0].1 += 1,
                b'D' => knots[0].1 -= 1,
                b'L' => knots[0].0 -= 1,
                b'R' => knots[0].0 += 1,
                _ => unreachable!(),
            }
            let mut tail_moved = true;
            for i in 0..KNOTS - 1 {
                let ro = knots[i];
                if !step_towards(&mut knots[i + 1], ro) {
                    tail_moved = false;
                    break;
                }
            }
            if tail_moved {
                positions.insert(knots[KNOTS - 1]);
            }
        }
    }
    positions.len()
}

#[inline]
fn step_towards(tail: &mut (i32, i32), head: (i32, i32)) -> bool {
    let xdiff = head.0 - tail.0;
    let ydiff = head.1 - tail.1;
    match (xdiff, ydiff) {
        (-1 | 0 | 1, -1 | 0 | 1) => false,
        _ => {
            tail.0 += xdiff.signum();
            tail.1 += ydiff.signum();
            true
        }
    }
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
        part2: {
            TEST_INPUT => 1,
            "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20" => 36,
        },
    },
    bench1 == 6367,
    bench2 == 2536,
    bench_parse: Vec::len => 2000,
}
