#![feature(test)]
extern crate test;
use aoc2024::{boilerplate, common::*};
use std::collections::VecDeque;

const DAY: usize = 1;
type I = u32;
type Parsed = (VecDeque<I>, VecDeque<I>);

fn parse_input(raw: &str) -> Parsed {
    let (mut a, mut b): (Vec<I>, Vec<I>) =
        raw.lines().filter_map(|l| l.split_once("   ")).map(|(a, b)| (parse_num::<I>(a), parse_num::<I>(b))).unzip();
    a.sort();
    b.sort();
    (a.into(), b.into())
}

fn part1((a, b): &Parsed) -> u32 {
    a.iter().zip(b).map(|(&a, &b)| a.abs_diff(b)).sum()
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "3   4
4   3
2   5
1   3
3   9
3   3"
    for tests: {
        part1: { TEST_INPUT => 11 },
        part2: { TEST_INPUT => 31 },
    },
    bench1 == 1580061,
    bench2 == 0,
    bench_parse: |(a, b): &Parsed| { a.len() == b.len(); a.len() } => 1000,
}
