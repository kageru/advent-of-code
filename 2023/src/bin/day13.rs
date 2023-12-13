#![feature(test, try_blocks)]
extern crate test;
use aoc2023::{boilerplate, common::*};

const DAY: usize = 13;
type Parsed = Vec<Vec<Vec<bool>>>;

fn parse_input(raw: &str) -> Parsed {
    raw.split("\n\n").map(|block| block.lines().map(|l| l.bytes().map(|b| b == b'#').collect()).collect()).collect()
}

fn find_reflection(block: &[Vec<bool>], blacklist: Option<usize>) -> Option<usize> {
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

fn transpose(orig: &[Vec<bool>]) -> Vec<Vec<bool>> {
    let mut out = vec![vec![false; orig.len()]; orig[0].len()];
    for i in 0..orig.len() {
        #[allow(clippy::needless_range_loop)]
        for j in 0..orig[0].len() {
            out[j][i] = orig[i][j];
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
            for i in 0..block.len() {
                for j in 0..block[0].len() {
                    block[i][j] ^= true;
                    transposed[j][i] ^= true;
                    if let Some(reflection) =
                        find_reflection(&block, p1).map(|n| n * 100).or_else(|| find_reflection(&transposed, p1_transposed))
                    {
                        return reflection;
                    }
                    block[i][j] ^= true;
                    transposed[j][i] ^= true;
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
    bench1 == 37561,
    bench2 == 31108,
    bench_parse: Vec::len => 100,
}
