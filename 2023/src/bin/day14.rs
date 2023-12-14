#![feature(test, try_blocks)]
extern crate test;
use aoc2023::{boilerplate, common::*};
use fnv::{FnvHashMap, FnvHasher};
use std::{hash::Hasher, mem::transmute};

const DAY: usize = 14;
type Parsed = Vec<Vec<Tile>>;

#[allow(dead_code)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(u8)]
enum Tile {
    Round = b'O',
    Cube = b'#',
    Empty = b'.',
}

fn parse_input(raw: &str) -> Parsed {
    raw.lines().map(|l| unsafe { transmute::<&str, &[Tile]>(l) }.to_vec()).collect()
}

fn tilt_north(grid: &mut Parsed) {
    for y in 0..grid.len() {
        for x in 0..grid[y].len() {
            if grid[y][x] == Tile::Round {
                let mut i = 0;
                while let Some(Tile::Empty) = try { grid.get(y.checked_sub(i + 1)?)?[x] } {
                    i += 1;
                }
                grid[y][x] = Tile::Empty;
                grid[y - i][x] = Tile::Round;
            }
        }
    }
}

fn tilt_west(grid: &mut Parsed) {
    for y in 0..grid.len() {
        for x in 0..grid[y].len() {
            if grid[y][x] == Tile::Round {
                let mut i = 0;
                while let Some(Tile::Empty) = try { grid[y][x.checked_sub(i + 1)?] } {
                    i += 1;
                }
                grid[y][x] = Tile::Empty;
                grid[y][x - i] = Tile::Round;
            }
        }
    }
}

fn tilt_south(grid: &mut Parsed) {
    for y in (0..grid.len()).rev() {
        for x in 0..(grid[y].len()) {
            if grid[y][x] == Tile::Round {
                let mut i = 0;
                while let Some(Tile::Empty) = try { grid.get(y + i + 1)?[x] } {
                    i += 1;
                }
                grid[y][x] = Tile::Empty;
                grid[y + i][x] = Tile::Round;
            }
        }
    }
}

fn tilt_east(grid: &mut Parsed) {
    for y in 0..grid.len() {
        for x in (0..grid[y].len()).rev() {
            if grid[y][x] == Tile::Round {
                let mut i = 0;
                while let Some(Tile::Empty) = try { grid[y].get(x + i + 1)? } {
                    i += 1;
                }
                grid[y][x] = Tile::Empty;
                grid[y][x + i] = Tile::Round;
            }
        }
    }
}

fn weight(grid: &Parsed) -> usize {
    grid.iter().rev().zip(1..).map(|(row, weight)| row.iter().filter(|&&t| t == Tile::Round).count() * weight).sum()
}

fn part1(parsed: &Parsed) -> usize {
    let mut grid = parsed.clone();
    tilt_north(&mut grid);
    weight(&grid)
}

fn part2(parsed: &Parsed) -> usize {
    let mut grid = parsed.clone();
    let mut weights = FnvHashMap::default();
    let mut n = 0;
    let cycle = loop {
        let mut hasher = FnvHasher::default();
        for l in grid.iter() {
            hasher.write(unsafe { transmute(l.as_slice()) });
        }
        let hash = hasher.finish();
        if let Some((_, old_n)) = weights.insert(hash, (weight(&grid), n)) {
            break n - old_n;
        }
        n += 1;

        tilt_north(&mut grid);
        tilt_west(&mut grid);
        tilt_south(&mut grid);
        tilt_east(&mut grid);
    };
    const REPETITIONS: usize = 1000000000;
    let steps = (REPETITIONS - n) % cycle;
    weights.into_iter().find_map(|(_, (w, n2))| (n2 == n - cycle + steps).then_some(w)).unwrap()
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
    bench2 == 87273,
    bench_parse: Vec::len => 100,
}
