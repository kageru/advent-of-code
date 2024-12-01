#![feature(test)]
extern crate test;
use aoc2024::{boilerplate, common::*};
use itertools::Itertools;

const DAY: usize = 1;
type I = u32;
type Parsed = (Vec<I>, Vec<I>);

fn parse_input(raw: &str) -> Parsed {
    let (mut a, mut b): Parsed =
        raw.lines().filter_map(|l| l.split_once("   ")).map(|(a, b)| (parse_num::<I>(a), parse_num::<I>(b))).unzip();
    a.sort_unstable();
    b.sort_unstable();
    (a, b)
}

fn part1((a, b): &Parsed) -> u32 {
    a.iter().zip(b).map(|(&a, &b)| a.abs_diff(b)).sum()
}

fn part2((first, second): &Parsed) -> u32 {
    let mut second = second
        .iter()
        .map(|&c| (c, 1))
        .coalesce(|(c1, n1), (c2, n2)| if c1 == c2 { Ok((c1, n1 + n2)) } else { Err(((c1, n1), (c2, n2))) })
        .peekable();
    first
        .iter()
        .map(|x| {
            loop {
                match second.peek() {
                    Some((y, count)) if x == y => break x * count,
                    Some((y, _)) if x > y => second.next(),
                    _ => break 0,
                };
            }
        })
        .sum()
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
    bench2 == 23046913,
    bench_parse: |(a, b): &Parsed| { assert!(a.len() == b.len()); a.len() } => 1000,
}
