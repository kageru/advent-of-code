#![feature(test, try_blocks)]
extern crate test;
use aoc2023::{boilerplate, common::*};

const DAY: usize = 13;
type Parsed<'a> = Vec<Vec<Vec<u8>>>;

fn parse_input(raw: &str) -> Parsed {
    raw.split("\n\n").map(|block| block.lines().map(|l| l.as_bytes().to_vec()).collect()).collect()
}

fn find_reflection(block: &[Vec<u8>]) -> Option<usize> {
    'outer: for i in 0..block.len() - 1 {
        let mut offset = 0;
        loop {
            match try { (block.get(i.checked_sub(offset)?)?, block.get(i + 1 + offset)?) } {
                None => return Some(i + 1),
                Some((a, b)) if a == b => offset += 1,
                Some(_) => continue 'outer,
            }
        }
    }
    None
}

fn part1(blocks: &Parsed) -> usize {
    blocks.iter().map(|block| find_reflection(block).map(|n| n * 100).or_else(|| find_reflection(&transpose(&block))).unwrap()).sum()
}

fn transpose(orig: &[Vec<u8>]) -> Vec<Vec<u8>> {
    let mut out = vec![vec![0; orig.len()]; orig[0].len()];
    for i in 0..orig.len() {
        for j in 0..orig[0].len() {
            out[j][i] = orig[i][j];
        }
    }
    out
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT_HORIZONTAL == "\
#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#",
    TEST_INPUT_VERTICAL == "\
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#."
    for tests: {
        part1: {
            TEST_INPUT_HORIZONTAL => 400,
            TEST_INPUT_VERTICAL => 5,
        },
        part2: { TEST_INPUT_VERTICAL => 0 },
    },
    bench1 == 37561,
    bench2 == 0,
    bench_parse: Vec::len => 100,
}
