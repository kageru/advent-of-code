#![feature(test, try_blocks)]
extern crate test;
use aoc2023::{boilerplate, common::*, direction::Direction};
use itertools::Itertools;
use std::mem::transmute;

const DAY: usize = 14;
type Parsed = Vec<Vec<Tile>>;

#[allow(dead_code)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(u8)]
enum Tile {
    Round = b'O',
    Cube = b'#',
    Empty = b'.',
}

fn next_index(x: usize, y: usize, direction: Direction) -> Option<(usize, usize)> {
    match direction {
        Direction::Up => Some((x, y.checked_sub(1)?)),
        Direction::Down => Some((x, y + 1)),
        Direction::Right => Some((x + 1, y)),
        Direction::Left => Some((x.checked_sub(1)?, y)),
    }
}

fn parse_input(raw: &str) -> Parsed {
    raw.lines().map(|l| unsafe { transmute::<&str, &[Tile]>(l) }.to_vec()).collect()
}

fn tilt(grid: &mut Parsed, direction: Direction) {
    for y in 0..grid.len() {
        for x in 0..(grid[y].len()) {
            if grid[y][x] == Tile::Round {
                grid[y][x] = Tile::Empty;
                let (mut x, mut y) = (x, y);
                // While the next position is valid, move 1 step
                while let Some(idx) = try {
                    let (next_x, next_y) = next_index(x, y, direction)?;
                    (*grid.get(next_y)?.get(next_x)? == Tile::Empty).then_some((next_x, next_y))?
                } {
                    (x, y) = idx;
                }
                grid[y][x] = Tile::Round;
            }
        }
    }
}

fn weight(grid: &Parsed) -> usize {
    grid.iter().rev().zip(1..).map(|(row, weight)| row.iter().filter(|&&t| t == Tile::Round).count() * weight).sum()
}

fn part1(parsed: &Parsed) -> usize {
    let mut grid = parsed.clone();
    tilt(&mut grid, Direction::Up);
    weight(&grid)
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "\
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."
    for tests: {
        part1: { TEST_INPUT => 136 },
        part2: { TEST_INPUT => 64 },
    },
    bench1 == 110407,
    bench2 == 0,
    bench_parse: Vec::len => 100,
}
