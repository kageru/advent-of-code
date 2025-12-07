#![feature(test)]
extern crate test;
use std::mem::transmute;

use Tile::*;
use aoc2025::{
    boilerplate,
    common::*,
    grid::{Grid, HashGrid},
    position::{Neighbors, Pos},
};
use itertools::Itertools;

const DAY: usize = 4;
type Parsed = HashGrid<Tile>;

#[derive(Debug, PartialEq, Eq, Copy, Clone, Default)]
#[repr(u8)]
enum Tile {
    Paper = b'@',
    #[default]
    Empty = b'.',
}

fn parse_input(raw: &str) -> Parsed {
    HashGrid::from_bytes_2d(raw, |b| unsafe { transmute(b) })
}

fn part1(parsed: &Parsed) -> usize {
    find_accessible(parsed).count()
}

fn find_accessible(parsed: &Parsed) -> impl Iterator<Item = Pos<i64>> {
    parsed
        .indices()
        .filter(|p| parsed.get(p) == Some(&Paper) && { p.neighbors().into_iter().filter(|np| parsed.get(np) == Some(&Paper)).count() < 4 })
}

fn part2(parsed: &Parsed) -> usize {
    let mut modified = parsed.clone();
    let mut removed = 0;
    while let accessible @ [_, ..] = find_accessible(&modified).collect_vec().as_slice() {
        for p in accessible {
            *modified.get_mut(p).unwrap() = Empty;
        }
        removed += accessible.len();
    }
    removed
}

boilerplate! {
    TEST_INPUT == "\
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@."
    for tests: {
        part1: { TEST_INPUT => 13 },
        part2: { TEST_INPUT => 43 },
    },
    bench1 == 1409,
    bench2 == 8366,
    bench_parse: aoc2025::grid::Grid::len => 19321,
}
