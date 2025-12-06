#![feature(test)]
extern crate test;
use aoc2025::{boilerplate, common::*};

const DAY: usize = 6;
type I = usize;
type Parsed = (Vec<Op>, Vec<Vec<I>>);

enum Op {
    Add,
    Mul,
}

fn parse_input(raw: &str) -> Parsed {
    let mut lines = raw.lines().rev();
    let operators = lines
        .next()
        .unwrap()
        .split_ascii_whitespace()
        .map(|c| match c {
            "*" => Op::Mul,
            "+" => Op::Add,
            _ => unreachable!(),
        })
        .collect();
    let lines = lines.map(|l| l.split_ascii_whitespace().map(parse_num).collect()).collect();
    (operators, lines)
}

fn part1((ops, nums): &Parsed) -> usize {
    ops.iter()
        .enumerate()
        .map(|(i, op)| match op {
            &Op::Add => nums.iter().map(|ns| ns[i]).sum::<I>(),
            &Op::Mul => nums.iter().map(|ns| ns[i]).product(),
        })
        .sum()
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "\
123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  "
    for tests: {
        part1: { TEST_INPUT => 4277556 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 4805473544166,
    bench2 == 0,
    bench_parse: |(ops, nums): &Parsed| (ops.len(), nums.len()) => (1000, 4),
}
