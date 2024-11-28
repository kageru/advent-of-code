#!/bin/sh

today=$(date +%d)
aocd "$today" > inputs/day$today

echo '#![feature(test)]
extern crate test;
use aoc2024::{boilerplate, common::*};

const DAY: usize = '$today';
type I = u32;
type Parsed = Vec<I>;

fn parse_input(raw: &str) -> Parsed {
    parse_nums(raw)
}

fn part1(parsed: &Parsed) -> usize {
    unimplemented!()
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == ""
    for tests: {
        part1: { TEST_INPUT => 0 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 0,
    bench2 == 0,
    bench_parse: Vec::len => 0,
}' > src/bin/day$today.rs
