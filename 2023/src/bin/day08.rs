#![feature(test)]
extern crate test;
use std::collections::HashMap;

use aoc2023::{boilerplate, common::*};

const DAY: usize = 08;
type Parsed<'a> = (Vec<Direction>, HashMap<&'a str, (&'a str, &'a str)>);

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
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

fn part1(parsed: &Parsed) -> usize {
    steps_until(parsed, "AAA", "ZZZ")
}

fn steps_until((directions, map): &Parsed, start: &str, target: &str) -> usize {
    directions
        .iter()
        .cycle()
        .scan(start, |pos, dir| {
            let next = match dir {
                Direction::Left => map.get(pos)?.0,
                Direction::Right => map.get(pos)?.1,
            };
            (!next.ends_with(target)).then(|| {
                *pos = next;
                Some(next)
            })
        })
        .count()
        + 1
}

// I’m honestly not sure why this works. It seems each path only has a single ghost node, and that
// node occurs right before looping, so we can just compute the least common multiple of their step counts.
// I assume this holds true for other inputs (it can’t have been random),
// but I don’t see it anywhere in the task and only found out by experimentation.
fn part2(parsed: &Parsed) -> usize {
    parsed.1.keys().filter(|start| start.ends_with("A")).map(|start| steps_until(parsed, start, "Z")).fold(1, |acc, n| lcm(acc, n))
}

#[cfg(test)]
const TEST_INPUT_P2: &str = "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)";

boilerplate! {
    TEST_INPUT == "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)",
    tests: {
        part1: { TEST_INPUT => 6 },
        part2: { TEST_INPUT_P2 => 6 },
    },
    bench1 == 12083,
    bench2 == 13385272668829,
    bench_parse: |(v, m): &Parsed| { assert_eq!(m["AAA"], ("MJJ", "QBJ")); (v.len(), v[0]) } => (281, Direction::Left),
}
