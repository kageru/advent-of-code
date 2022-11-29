#!/bin/sh

today=$(date +%d)
aocd > inputs/day$today

echo '#![feature(test)]
extern crate test;
use aoc2022::{boilerplate, common::*};

const DAY: usize = 0;
type Parsed = Vec<usize>;

boilerplate! {
    TEST_INPUT == "",
    tests: {
        part1: { TEST_INPUT => 0 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 0,
    bench2 == 0,
    parse: Vec::len => 0,
}

fn parse_input(raw: &str) -> Parsed {
    parse_nums()
}

fn part1(parsed: &Parsed) -> usize {
    unimplemented!()
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}' > src/bin/day$today.rs
