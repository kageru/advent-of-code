#![feature(test)]
extern crate test;

use std::hint::unreachable_unchecked;

use aoc2023::{boilerplate, common::*};

const DAY: usize = 8;
type Parsed<'a> = (Vec<Direction>, Vec<u16>, [[u16; 2]; LUT_SIZE]);

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
enum Direction {
    Left = 0,
    Right = 1,
}

const LUT_SIZE: usize = 27483; // pack("ZZZ") + 1

fn pack(s: &[u8; 3]) -> u16 {
    s.iter().cloned().fold(0, |acc, n| (acc << 5) + (n as u16 & 0b11111))
}

fn parse_input(raw: &str) -> Parsed {
    let (directions, map) = raw.split_once("\n\n").unwrap();
    let directions = directions
        .bytes()
        .map(|i| match i {
            b'L' => Direction::Left,
            b'R' => Direction::Right,
            _ => unsafe { unreachable_unchecked() },
        })
        .collect();
    let mut lut = [[0u16, 0]; LUT_SIZE];
    let mut indices = Vec::new();
    for x in map.lines().map(|l| l.as_bytes()) {
        unsafe {
            let idx = pack(&*x[0..=2].as_ptr().cast());
            let left = pack(&*x[7..=9].as_ptr().cast());
            let right = pack(&*x[12..=14].as_ptr().cast());
            lut[idx as usize] = [left, right];
            if x[2] == b'A' {
                indices.push(idx);
            }
        }
    }
    (directions, indices, lut)
}

#[inline]
fn ends_with(packed: u16, suffix: u8) -> bool {
    packed & 0b11111 == suffix as u16 & 0b11111
}

fn steps_until((directions, _, map): &Parsed, start: u16) -> usize {
    directions
        .iter()
        .cycle()
        .scan(start, |pos, dir| {
            let next = map[*pos as usize][*dir as usize];
            (!ends_with(next, b'Z')).then(|| *pos = next)
        })
        .count()
        + 1
}

fn part1(parsed: &Parsed) -> usize {
    steps_until(parsed, pack(&[b'A'; 3]))
}

// I’m honestly not sure why this works. It seems each path only has a single ghost node, and that
// node occurs right before looping, so we can just compute the least common multiple of their step counts.
// I assume this holds true for other inputs (it can’t have been random),
// but I don’t see it anywhere in the task and only found out by experimentation.
fn part2(parsed: &Parsed) -> usize {
    parsed.1.iter().filter(|&&n| ends_with(n, b'A')).fold(1, |acc, n| lcm(acc, steps_until(parsed, *n)))
}

boilerplate! {
    TEST_INPUT == "\
LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)",
    TEST_INPUT_P2 == "\
LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"
    for tests: {
        part1: { TEST_INPUT => 6 },
        part2: { TEST_INPUT_P2 => 6 },
    },
    bench1 == 12083,
    bench2 == 13385272668829,
    bench_parse: |(v, v2, m): &Parsed| { assert_eq!(m[0], [0, 0]); (v.len(), v2.len(), v[0]) } => (281, 6, Direction::Left),
}
