#![feature(test, try_blocks)]
extern crate test;
use aoc2023::{boilerplate, common::*};

const DAY: usize = 13;
type Parsed = Vec<Vec<I>>;
type I = u32;

fn parse_input(raw: &str) -> Parsed {
    raw.split("\n\n")
        .map(|block| block.lines().map(|l| l.bytes().map(|b| b == b'#').fold(0, |acc, n| (acc << 1) | n as I)).collect())
        .collect()
}

fn find_reflection<const DEVIATIONS: u32>(block: &[I]) -> Option<usize> {
    'outer: for i in 0..block.len() - 1 {
        let mut offset = 0;
        let mut deviations = DEVIATIONS;
        loop {
            match try { (block.get(i.checked_sub(offset)?)? ^ block.get(i + 1 + offset)?).count_ones() } {
                None if deviations == 0 => return Some(i + 1),
                Some(0) => offset += 1,
                Some(diff) if diff <= deviations => {
                    offset += 1;
                    deviations -= diff;
                }
                _ => continue 'outer,
            }
        }
    }
    None
}

fn transpose(orig: &[I]) -> Vec<I> {
    let max = orig.iter().max().unwrap();
    let len = (I::BITS - max.leading_zeros()) as usize;
    let mut out = vec![0; len];
    for j in 0..len {
        for (i, &value) in orig.iter().enumerate() {
            let src_bit = value & (1 << j);
            let dst_position = orig.len() - 1 - i;
            out[len - 1 - j] |= ((src_bit != 0) as I) << dst_position;
        }
    }
    out
}

fn solve<const DEV: u32>(blocks: &Parsed) -> usize {
    blocks
        .iter()
        .map(|block| find_reflection::<DEV>(block).map(|n| n * 100).or_else(|| find_reflection::<DEV>(&transpose(block))).unwrap())
        .sum()
}

fn part1(blocks: &Parsed) -> usize {
    solve::<0>(blocks)
}

fn part2(blocks: &Parsed) -> usize {
    solve::<1>(blocks)
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
#.#.##.#.",
    TEST_INPUT_475 == "\
#..##....
.#.##....
##....##.
#.#.#.##.
.####.##.
###..#..#
#.#.#.##.
.#.#.####
..#.#.##.
.##..####
#####.##.
.#.#.####
.#...####
.#.#.####
.#.#.####
#####.##.
.##..####"
    for tests: {
        part1: {
            TEST_INPUT_HORIZONTAL => 400,
            TEST_INPUT_VERTICAL => 5,
            TEST_INPUT_475 => 7,
        },
        part2: {
            TEST_INPUT_HORIZONTAL => 100,
            TEST_INPUT_VERTICAL => 300,
            TEST_INPUT_475 => 1300,
        },
    },
    unittests: {
        transpose: {
            &[
                0b100,
                0b010,
            ] => TRANSPOSE_42,
            &[
                0b101,
                0b010,
            ] => TRANSPOSE_52,
        },
    },
    bench1 == 37561,
    bench2 == 31108,
    bench_parse: Vec::len => 100,
}

#[cfg(test)]
const TRANSPOSE_52: &[I] = &[0b10, 0b01, 0b10];
#[cfg(test)]
const TRANSPOSE_42: &[I] = &[0b10, 0b01, 0b00];
