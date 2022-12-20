#![feature(test)]
extern crate test;
use aoc2022::{boilerplate, common::*};
use std::{
    collections::VecDeque,
    iter::{repeat, repeat_with},
};

const DAY: usize = 20;
type Parsed = Vec<isize>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines().map(|l| l.parse().unwrap()).collect()
}

fn part1(parsed: &Parsed) -> isize {
    let mut xs: VecDeque<_> = repeat(false).zip(parsed.iter().copied()).collect();
    let mut moved = 0;
    while moved < parsed.len() {
        let x = xs.pop_front().unwrap();
        if x.0 {
            // already processed, just put it in the back
            xs.push_back(x);
        } else {
            // casting :notLikeMiya:
            xs.insert(((x.1 + (xs.len() * 2) as isize) % xs.len() as isize) as usize, (true, x.1));
            moved += 1;
        }
    }
    get_coords(&mut xs)
}

const DECRYPTION_KEY: isize = 811589153;

fn part2(parsed: &Parsed) -> isize {
    let mut xs: VecDeque<_> = parsed.iter().enumerate().map(|(p, x)| (p, x * DECRYPTION_KEY)).collect();
    // Effective length is reduced by one because the element that’s being moved is no longer in the collection.
    let len = xs.len() as isize - 1;
    // Add this to each element to make it guaranteed positive before modulo-ing for the index.
    let make_positive = len * 2 * DECRYPTION_KEY;
    for _ in 0..10 {
        for current in 0..parsed.len() {
            let i = xs.iter().position(|(x, _)| x == &current).unwrap();
            xs.rotate_left(i);
            let x = xs.pop_front().unwrap();
            xs.insert(((x.1 + make_positive) % len) as usize, x);
        }
    }
    get_coords(&mut xs)
}

fn get_coords<T>(xs: &mut VecDeque<(T, isize)>) -> isize {
    xs.rotate_left(xs.iter().position(|(_, x)| x == &0).unwrap());
    repeat_with(|| {
        xs.rotate_left(1000 % xs.len());
        xs.front().unwrap().1
    })
    .take(3)
    .sum()
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
