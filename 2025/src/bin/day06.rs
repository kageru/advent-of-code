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
    let lines = transpose(&lines.iter().map(|l| l.as_bytes().to_vec()).collect());
    ops.split_ascii_whitespace()
        .zip(
            lines.split(|l| l.iter().all(|&b| b == b' ')).map(|bytes| {
                bytes.iter().map(|bs| String::from_utf8(bs.clone()).unwrap().trim().parse::<I>().unwrap()).collect::<Vec<_>>()
            }),
        )
        // .zip(lines)
        .map(|(op, ns)| match op {
            "+" => ns.iter().sum::<I>(),
            "*" => ns.iter().product(),
            _ => unreachable!(),
        })
        .sum()
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
        part2: { TEST_INPUT => 3263827 },
    },
    bench1 == 4805473544166,
    bench2 == 8907730960817,
    bench_parse: |(ops, nums): &Parsed| (ops.len(), nums.len()) => (3727, 4),
}
