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

fn find_reflection(block: &[I], blacklist: Option<usize>) -> Option<usize> {
    'outer: for i in 0..block.len() - 1 {
        let mut offset = 0;
        loop {
            match try { (block.get(i.checked_sub(offset)?)?, block.get(i + 1 + offset)?) } {
                None if Some(i) != blacklist => return Some(i + 1),
                Some((a, b)) if a == b => offset += 1,
                _ => continue 'outer,
            }
        }
    }
    None
}

fn part1(blocks: &Parsed) -> usize {
    blocks
        .iter()
        .map(|block| find_reflection(block, None).map(|n| n * 100).or_else(|| find_reflection(&transpose(block), None)).unwrap())
        .sum()
}

fn transpose(orig: &[I]) -> Vec<I> {
    let max = orig.iter().max().unwrap();
    let len = (I::BITS - max.leading_zeros()) as usize;
    let mut out = vec![0; len];
    for j in 0..len {
        for (i, &value) in orig.iter().enumerate() {
            if (value & (1 << j)) != 0 {
                out[len - 1 - j] |= 1 << (orig.len() - 1) >> i;
            }
        }
    }
    out
}

fn part2(blocks: &Parsed) -> usize {
    blocks
        .iter()
        .map(|block| {
            let mut block = block.to_owned();
            let mut transposed = transpose(&block);
            let p1 = find_reflection(&block, None).map(|n| n - 1);
            let p1_transposed = find_reflection(&transposed, None).map(|n| n - 1);
            let max = block.iter().max().unwrap();
            let len = (I::BITS - max.leading_zeros()) as usize;
            for i in 0..block.len() {
                for j in 0..len {
                    block[i] ^= 1 << j;
                    transposed[j] ^= 1 << i;
                    if let Some(reflection) =
                        find_reflection(&block, p1).map(|n| n * 100).or_else(|| find_reflection(&transposed, p1_transposed))
                    {
                        return reflection;
                    }
                    block[i] ^= 1 << j;
                    transposed[j] ^= 1 << i;
                }
            }
            unreachable!()
        })
        .sum()
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
        part2: {
            TEST_INPUT_HORIZONTAL => 100,
            TEST_INPUT_VERTICAL => 300,
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
