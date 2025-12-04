#![feature(test)]
extern crate test;
use Tile::*;
use aoc2025::{
    boilerplate,
    common::*,
    grid::{Grid, HashGrid},
};

const DAY: usize = 4;
type Parsed = HashGrid<Tile, 2>;

#[derive(Debug, PartialEq, Eq, Copy, Clone, Default)]
#[repr(u8)]
enum Tile {
    Paper = b'@',
    #[default]
    Empty = b'.',
}

fn parse_input(raw: &str) -> Parsed {
    HashGrid::from_bytes_2d(raw, |b| unsafe { std::mem::transmute::<u8, Tile>(b) })
}

fn part1(parsed: &Parsed) -> usize {
    parsed
        .indices()
        .filter(|p| parsed.get(p) == Some(&Paper) && { p.neighbors().into_iter().filter(|np| parsed.get(np) == Some(&Paper)).count() < 4 })
        .count()
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
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
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 1409,
    bench2 == 0,
    bench_parse: aoc2025::grid::Grid::len => 19321,
}
