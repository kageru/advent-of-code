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
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
    start(grid, 0, 0, Right)
}

fn part2(grid: &Parsed) -> usize {
    (0..grid.len())
        .map(|y| (0, y, Right))
        .chain((0..grid.len()).map(|y| (grid[0].len() - 1, y, Left)))
        .chain((0..grid[0].len()).map(|x| (x, 0, Down)))
        .chain((0..grid[0].len()).map(|x| (x, grid.len() - 1, Up)))
        .map(|(x, y, dir)| start(grid, x, y, dir))
        .max()
        .unwrap()
}

fn start(grid: &Parsed, x: usize, y: usize, dir: Direction) -> usize {
    let mut points = FnvHashSet::default();
    points.insert((x, y));
    let mut known = FnvHashSet::default();
    match turn(dir, grid[y][x]) {
        (d1, Some(d2)) => {
            energized(grid, &mut points, &mut known, x, y, d1);
            energized(grid, &mut points, &mut known, x, y, d2);
        }
        (d1, None) => energized(grid, &mut points, &mut known, x, y, d1),
    }
    points.len()
}

fn turn(dir: Direction, tile: Tile) -> (Direction, Option<Direction>) {
    match (dir, tile) {
        (dir, Empty) => (dir, None),
        (Up, Angle1) => (Right, None),
        (Right, Angle1) => (Up, None),
        (Down, Angle1) => (Left, None),
        (Left, Angle1) => (Down, None),
        (Up, Angle2) => (Left, None),
        (Left, Angle2) => (Up, None),
        (Down, Angle2) => (Right, None),
        (Right, Angle2) => (Down, None),
        (Left | Right, HSplit) => (Up, Some(Down)),
        (dir, HSplit) => (dir, None),
        (Up | Down, VSplit) => (Right, Some(Left)),
        (dir, VSplit) => (dir, None),
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
    if let Some(&tile) = try {
        y = y.checked_sub((dir == Up) as usize)? + (dir == Down) as usize;
        x = x.checked_sub((dir == Left) as usize)? + (dir == Right) as usize;
        grid.get(y)?.get(x)?
    } {
        points.insert((x, y));
        let (d1, d2) = turn(dir, tile);
        energized(grid, points, known, x, y, d1);
        if let Some(d2) = d2 {
            energized(grid, points, known, x, y, d2);
        }
    }
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
        part2: { TEST_INPUT => 51 },
    },
    bench1 == 7517,
    bench2 == 7741,
    bench_parse: Vec::len => 110,
}
