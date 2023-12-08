#![feature(test)]
extern crate test;
use std::collections::HashMap;

use aoc2023::{boilerplate, common::*};

const DAY: usize = 08;
type Parsed<'a> = (Vec<Direction>, HashMap<&'a str, (&'a str, &'a str)>);

#[derive(Debug, PartialEq, Copy, Clone)]
enum Direction {
    Left,
    Right,
}

fn parse_input(raw: &str) -> Parsed {
    let (directions, map) = raw.split_once("\n\n").unwrap();
    let directions = directions
        .bytes()
        .map(|i| match i {
            b'L' => Direction::Left,
            b'R' => Direction::Right,
            _ => unreachable!(),
        })
        .collect();
    let map = map.lines().map(|l| (&l[0..=2], (&l[7..=9], &l[12..=14]))).collect();
    (directions, map)
}

fn part1((directions, map): &Parsed) -> usize {
    directions
        .iter()
        .cycle()
        .scan("AAA", |pos, dir| {
            let next = match dir {
                Direction::Left => map.get(pos)?.0,
                Direction::Right => map.get(pos)?.1,
            };
            if next == "ZZZ" {
                None
            } else {
                *pos = next;
                Some(next)
            }
        })
        .count()
        + 1
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)",
    tests: {
        part1: { TEST_INPUT => 6 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 12083,
    bench2 == 0,
    bench_parse: |(v, m): &Parsed| { assert_eq!(m["AAA"], ("MJJ", "QBJ")); (v.len(), v[0]) } => (281, Direction::Left),
}
