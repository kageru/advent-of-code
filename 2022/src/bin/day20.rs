#![feature(test)]
extern crate test;
use aoc2022::{boilerplate, common::*};
use std::{collections::VecDeque, iter::repeat};

const DAY: usize = 20;
type Parsed = VecDeque<(isize, bool)>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines().map(|l| l.parse().unwrap()).zip(repeat(false)).collect()
}

fn part1(parsed: &Parsed) -> isize {
    let mut xs = parsed.to_owned();
    let mut moved = 0;
    while moved < parsed.len() {
        let x = xs.pop_front().unwrap();
        if x.1 {
            xs.push_back(x);
        } else {
            xs.insert(((x.0 + (xs.len() * 2) as isize) % xs.len() as isize) as usize, (x.0, true));
            moved += 1;
        }
    }
    let i = xs.iter().position(|(x, _)| x == &0).unwrap();
    xs.rotate_left(i);
    xs.rotate_left(1000 % parsed.len());
    let a = xs.front().unwrap().0;
    xs.rotate_left(1000 % parsed.len());
    let b = xs.front().unwrap().0;
    xs.rotate_left(1000 % parsed.len());
    let c = xs.front().unwrap().0;
    a + b + c
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "1
2
-3
3
-2
0
4",
    tests: {
        part1: { TEST_INPUT => 3 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 23321,
    bench2 == 0,
    bench_parse: VecDeque::len => 5000,
}
