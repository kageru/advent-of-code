#![feature(test)]
extern crate test;
use aoc2025::{boilerplate, common::*};

const DAY: usize = 6;
type I = usize;
type Parsed<'a> = (&'a str, Vec<&'a str>);

fn parse_input(raw: &str) -> Parsed<'_> {
    let mut lines = raw.lines().rev();
    (lines.next().unwrap(), lines.rev().collect())
}

fn part1((ops, lines): &Parsed) -> usize {
    let nums = transpose(&lines.iter().map(|l| l.split_ascii_whitespace().map(parse_num::<I>).collect()).collect());
    ops.split_ascii_whitespace()
        .zip(nums)
        .map(|(op, ns)| match op {
            "+" => ns.iter().sum::<I>(),
            "*" => ns.iter().product(),
            _ => unreachable!(),
        })
        .sum()
}

fn part2((ops, lines): &Parsed) -> usize {
    unimplemented!()
}

fn transpose<T: Copy + Default>(v: &Vec<Vec<T>>) -> Vec<Vec<T>> {
    let size = v.len();
    let inner_size = v[0].len();
    let mut out = vec![vec![T::default(); size]; inner_size];
    for i in 0..size {
        for j in 0..inner_size {
            out[j][i] = v[i][j];
        }
    }
    out
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
    bench_parse: |(ops, nums): &Parsed| (ops.len(), nums.len()) => (3727, 4),
}
