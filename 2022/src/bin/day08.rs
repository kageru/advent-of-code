#![feature(test, get_many_mut)]
extern crate test;
use itertools::Itertools;
use rayon::prelude::*;
use std::iter::repeat;

use aoc2022::{boilerplate, common::*};

const DAY: usize = 8;
type Parsed = Vec<Vec<u8>>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines().map(|l| l.as_bytes().to_vec()).collect()
}

fn is_visible_1d<'a>(iter: impl IntoIterator<Item = &'a u8>, rev: bool) -> Vec<bool> {
    let mut v: Vec<_> = iter
        .into_iter()
        .scan(0, |max, tree| {
            let visible = tree > max;
            *max = *tree.max(max);
            Some(visible)
        })
        .collect();
    if rev {
        v.reverse();
    }
    v
}

fn transpose<T: Copy>(v: &[Vec<T>]) -> Vec<Vec<T>> {
    let mut v = v.to_vec();
    let size = v.len();
    for i in 0..size {
        for j in i + 1..size {
            let [a, b] = v.get_many_mut([i, j]).unwrap();
            std::mem::swap(&mut a[j], &mut b[i]);
        }
    }
    v
}

fn horizontally_visible(v: &[Vec<u8>]) -> Vec<Vec<bool>> {
    v.iter().map(|l| is_visible_1d(l, false).into_iter().zip(is_visible_1d(l.iter().rev(), true)).map(|(a, b)| a || b).collect()).collect()
}

fn part1(parsed: &Parsed) -> usize {
    let horizontal = horizontally_visible(parsed);
    let vertical = horizontally_visible(&transpose(parsed));
    (0..parsed.len()).flat_map(|i| repeat(i).zip(0..parsed.len())).filter(|&(i, j)| horizontal[i][j] || vertical[j][i]).count()
}

fn part2(parsed: &Parsed) -> usize {
    let size = parsed.len(); // input is always square
    let transposed = transpose(parsed);
    (1..size - 1)
        .into_par_iter()
        .flat_map(|i| rayon::iter::repeat(i).zip(1..size - 1))
        .map(|(i, j)| {
            let tree = parsed[i][j];
            let a = visible_trees(tree, transposed[j][(i + 1)..size].iter());
            let b = visible_trees(tree, transposed[j][0..i].iter().rev());
            let c = visible_trees(tree, parsed[i][(j + 1)..size].iter());
            let d = visible_trees(tree, parsed[i][0..j].iter().rev());
            a * b * c * d
        })
        .max()
        .unwrap()
}

#[inline] // this inline actually saves ~40% runtime
fn visible_trees<'a, I: Iterator<Item = &'a u8>>(tree: u8, heights: I) -> usize {
    let mut heights = heights.copied().peekable();
    heights.peeking_take_while(|&t| t < tree).count() + heights.peek().map(|_| 1).unwrap_or(0)
}

// These have to be separate variables because the test outputs are used as function names and
// can’t contain special characters.
#[cfg(test)]
const TEST_OUTPUT: &[bool] = &[true, true, false, true, false];
#[cfg(test)]
const TRANSPOSE_OUTPUT: &[[u8; 3]; 3] = &[[1, 4, 7], [2, 5, 8], [3, 6, 9]];

boilerplate! {
    TEST_INPUT == "\
        30373\n\
        25512\n\
        65332\n\
        33549\n\
        35390\
    ",
    tests: {
        part1: { TEST_INPUT => 21 },
        part2: { TEST_INPUT => 8 },
    },
    unittests: {
        is_visible_1d: { [1, 3, 2, 4, 2].iter(), false => TEST_OUTPUT, },
        transpose: { &vec![vec![1, 2, 3], vec![4, 5, 6], vec![7, 8, 9]] => TRANSPOSE_OUTPUT },
    },
    bench1 == 1543,
    bench2 == 595080,
    bench_parse: Vec::len => 99,
}
