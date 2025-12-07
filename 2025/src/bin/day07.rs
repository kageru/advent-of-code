#![feature(test, try_blocks)]
extern crate test;
use std::collections::HashSet;

use aoc2025::{
    boilerplate,
    common::*,
    direction::Direction,
    grid::{Grid, VecGrid},
    position::Pos,
};

const DAY: usize = 7;
type Parsed = (Pos<usize>, VecGrid<Tile>);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(u8)]
enum Tile {
    Empty = b'.',
    Splitter = b'^',
    Start = b'S',
}

fn parse_input(raw: &str) -> Parsed {
    let grid = VecGrid::transmute_from_lines(raw);
    let p = grid.find(&Tile::Start).unwrap();
    (p, grid)
}

fn part1((start, grid): &Parsed) -> usize {
    let mut splits = HashSet::new();
    count_splits(grid, *start, &mut splits);
    splits.len()
}

// there are no splitters at the left/right edge of the input
fn count_splits(grid: &VecGrid<Tile>, pos: Pos<usize>, splits: &mut HashSet<Pos<usize>>) {
    try {
        let down = pos.checked_add(Direction::Down)?;
        match grid.get(&down)? {
            Tile::Empty => count_splits(grid, down, splits),
            Tile::Splitter => {
                if splits.insert(down) {
                    count_splits(grid, down + Direction::Right, splits);
                    count_splits(grid, down + Direction::Left, splits);
                }
            }
            Tile::Start => unreachable!(),
        }
    };
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "\
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
..............."
    for tests: {
        part1: { TEST_INPUT => 21 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 1579,
    bench2 == 0,
    bench_parse: |(p, grid): &Parsed| (*p, grid.len()) => (Pos(141, 70), 142),
}
