#![feature(test)]
extern crate test;
use aoc2024::{boilerplate, common::*};

const DAY: usize = 11;
type I = usize;
type Parsed = Vec<I>;

fn parse_input(raw: &str) -> Parsed {
    parse_nums_separator(raw, ' ')
}

fn split(n: I) -> Vec<I> {
    let x = n.ilog10() / 2 + 1;
    let exp = 10usize.pow(x);
    let lhs = n / exp;
    vec![lhs, n - lhs * exp]
}

fn part1(parsed: &Parsed) -> I {
    let mut stones = parsed.clone();
    let mut more_stones = Vec::<I>::new();
    for _ in 0..25 {
        println!("{stones:?}");
        more_stones.extend(stones.drain(..).flat_map(|s| match s {
            0 => vec![1],
            n if n.ilog10() & 1 == 1 => split(n),
            n => vec![n * 2024],
        }));
        stones.extend(more_stones.drain(..));
    }
    stones.len()
}

fn part2(parsed: &Parsed) -> I {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "125 17"
    for tests: {
        part1: { TEST_INPUT => 55312 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 222461,
    bench2 == 0,
    bench_parse: Vec::len => 8,
}
