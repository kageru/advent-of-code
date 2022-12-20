#![feature(test)]
extern crate test;
use aoc2022::{boilerplate, common::*};
use std::{collections::VecDeque, iter::repeat};

const DAY: usize = 20;
type Parsed = Vec<isize>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines().map(|l| l.parse().unwrap()).collect()
}

fn part1(parsed: &Parsed) -> isize {
    let mut xs: VecDeque<_> = parsed.iter().copied().zip(repeat(false)).collect();
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

const DECRYPTION_KEY: isize = 811589153;

fn part2(parsed: &Parsed) -> isize {
    let mut xs: VecDeque<_> = parsed.iter().enumerate().map(|(p, x)| (p, x * DECRYPTION_KEY)).collect();
    for i in 0..10 {
        dbg!(i, &xs);
        let mut moved = 0;
        while moved < parsed.len() {
            let i = xs.iter().position(|(x, _)| x == &moved).unwrap();
            xs.rotate_left(i);
            let x = xs.pop_front().unwrap();
            let idx = ((x.1 + (xs.len() * 2 * DECRYPTION_KEY as usize) as isize) % xs.len() as isize) as usize;
            dbg!("Inserting at", idx);
            xs.insert(idx, x);
            moved += 1;
        }
    }
    let i = xs.iter().position(|(_, x)| x == &0).unwrap();
    xs.rotate_left(i);
    xs.rotate_left(1000 % parsed.len());
    let a = xs.front().unwrap().1;
    xs.rotate_left(1000 % parsed.len());
    let b = xs.front().unwrap().1;
    xs.rotate_left(1000 % parsed.len());
    let c = xs.front().unwrap().1;
    a + b + c
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
        part2: { TEST_INPUT => 1623178306 },
    },
    bench1 == 23321,
    bench2 == 1428396909280,
    bench_parse: Vec::len => 5000,
}
