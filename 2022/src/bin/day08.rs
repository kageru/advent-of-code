#![feature(test)]
extern crate test;
use std::iter::repeat;

use aoc2022::{boilerplate, common::*};

const DAY: usize = 08;
type Parsed = Vec<Vec<u8>>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines().map(|l| l.as_bytes().to_vec()).collect()
}

fn is_visible_1d<'a>(iter: impl IntoIterator<Item = &'a u8>) -> Vec<bool> {
    iter.into_iter()
        .scan(0, |max, tree| {
            let visible = tree > max;
            *max = *tree.max(max);
            Some(visible)
        })
        .collect()
}

fn part1(parsed: &Parsed) -> usize {
    let a: Vec<_> = parsed.iter().map(is_visible_1d).collect();
    let b: Vec<_> = parsed
        .iter()
        .map(|l| {
            let mut v = is_visible_1d(l.iter().rev());
            v.reverse();
            v
        })
        .collect();
    let c: Vec<_> = (0..parsed[0].len()).map(|i| is_visible_1d(parsed.iter().map(|row| &row[i]))).collect();
    let d: Vec<_> = (0..parsed[0].len())
        .map(|i| {
            let mut v = is_visible_1d(parsed.iter().rev().map(|row| &row[i]));
            v.reverse();
            v
        })
        .collect();
    (0..parsed.len()).flat_map(|i| repeat(i).zip(0..parsed[0].len())).filter(|&(i, j)| a[i][j] || b[i][j] || c[j][i] || d[j][i]).count()
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

#[cfg(test)]
const TEST_OUTPUT: &[bool] = &[true, true, false, true, false];

boilerplate! {
    TEST_INPUT == "30373
25512
65332
33549
35390",
    tests: {
        part1: { TEST_INPUT => 21 },
        part2: { TEST_INPUT => 0 },
    },
    unittests: {
        is_visible_1d: { [1, 3, 2, 4, 2] => TEST_OUTPUT, },
    },
    bench1 == 1543,
    bench2 == 0,
    bench_parse: Vec::len => 99,
}
