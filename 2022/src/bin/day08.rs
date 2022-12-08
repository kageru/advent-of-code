#![feature(test)]
extern crate test;
use itertools::Itertools;
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

fn transpose<T: Copy>(v: &Vec<Vec<T>>) -> Vec<Vec<T>> {
    let len = v.len();
    let mut iters: Vec<_> = v.iter().map(|n| n.iter()).collect();
    (0..len).map(|_| iters.iter_mut().map(|i| i.next().unwrap()).copied().collect::<Vec<T>>()).collect()
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
    (1..size - 1)
        .flat_map(|i| repeat(i).zip(1..size - 1))
        .map(|(i, j)| {
            let tree = parsed[i][j];
            let a = visible_trees(((i + 1)..size).map(|i| parsed[i][j]), tree);
            let b = visible_trees((0..i).rev().map(|i| parsed[i][j]), tree);
            let c = visible_trees(((j + 1)..size).map(|j| parsed[i][j]), tree);
            let d = visible_trees((0..j).rev().map(|j| parsed[i][j]), tree);
            a * b * c * d
        })
        .max()
        .unwrap()
}

#[inline] // this inline actually saves ~40% runtime
fn visible_trees(heights: impl Iterator<Item = u8>, tree: u8) -> usize {
    let mut heights = heights.peekable();
    heights.peeking_take_while(|&t| t < tree).count() + heights.peek().map(|_| 1).unwrap_or(0)
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
        part2: { TEST_INPUT => 8 },
    },
    unittests: {
        is_visible_1d: { [1, 3, 2, 4, 2].iter(), false => TEST_OUTPUT, },
    },
    bench1 == 1543,
    bench2 == 595080,
    bench_parse: Vec::len => 99,
}
