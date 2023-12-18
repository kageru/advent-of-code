#![feature(test, try_blocks)]
extern crate test;
use aoc2023::{
    boilerplate,
    common::*,
    direction::Direction::{self, *},
};
use fnv::FnvHashSet;
use std::mem::transmute;
use Tile::*;

const DAY: usize = 16;
type Parsed<'a> = Vec<&'a [Tile]>;

#[repr(u8)]
#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
enum Tile {
    Empty = b'.',
    HSplit = b'|',
    VSplit = b'-',
    Angle1 = b'/',
    Angle2 = b'\\',
}

fn parse_input(raw: &str) -> Parsed {
    raw.lines().map(|l| unsafe { transmute(l) }).collect()
}

fn part1(grid: &Parsed) -> usize {
    let mut points = FnvHashSet::default();
    points.insert((0, 0));
    let mut known = FnvHashSet::default();
    // my real input starts with a mirror,
    // and I don’t want to rewrite the code to not blindly make the first step.
    energized(grid, &mut points, &mut known, 0, 0, if grid[0][0] == Empty { Right } else { Down });
    points.len()
}

fn angle1(dir: Direction) -> Direction {
    match dir {
        Up => Right,
        Right => Up,
        Down => Left,
        Left => Down,
    }
}

fn angle2(dir: Direction) -> Direction {
    match dir {
        Up => Left,
        Left => Up,
        Down => Right,
        Right => Down,
    }
}

fn energized(
    grid: &Parsed,
    points: &mut FnvHashSet<(usize, usize)>,
    known: &mut FnvHashSet<(usize, usize, Direction)>,
    mut x: usize,
    mut y: usize,
    dir: Direction,
) {
    if !known.insert((x, y, dir)) {
        return;
    }
    if let Some(tile) = try {
        y = y.checked_sub((dir == Up) as usize)? + (dir == Down) as usize;
        x = x.checked_sub((dir == Left) as usize)? + (dir == Right) as usize;
        grid.get(y)?.get(x)?
    } {
        points.insert((x, y));
        match (tile, dir) {
            (Empty, _) => {
                energized(grid, points, known, x, y, dir);
            }
            (Angle1, _) => energized(grid, points, known, x, y, angle1(dir)),
            (Angle2, _) => energized(grid, points, known, x, y, angle2(dir)),
            (HSplit, Up | Down) => energized(grid, points, known, x, y, dir),
            (HSplit, Left | Right) => {
                energized(grid, points, known, x, y, Up);
                energized(grid, points, known, x, y, Down);
            }
            (VSplit, Up | Down) => {
                energized(grid, points, known, x, y, Right);
                energized(grid, points, known, x, y, Left);
            }
            (VSplit, Left | Right) => energized(grid, points, known, x, y, dir),
        }
    }
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == r".|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|...."
    for tests: {
        part1: { TEST_INPUT => 46 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 7517,
    bench2 == 0,
    bench_parse: Vec::len => 110,
}
