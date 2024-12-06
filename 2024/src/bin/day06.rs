#![feature(test, iter_map_windows)]
extern crate test;
use std::iter::successors;

use aoc2024::{
    boilerplate,
    common::*,
    direction::Direction,
    grid::{Grid, VecGrid},
    position::Pos,
};
use fnv::FnvHashSet;
use itertools::Itertools;

const DAY: usize = 6;
type P = Pos<usize, 2>;
type Parsed = (VecGrid<Tile>, P);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
enum Tile {
    Empty = b'.',
    Obstacle = b'#',
    Visited = b'X',
    Guard = b'^',
}

fn parse_input(raw: &str) -> Parsed {
    let grid = VecGrid::transmute_from_lines(raw);
    let start = grid.indices().find(|&p| grid[p] == Tile::Guard).expect("No guard on map");
    (grid, start)
}

fn walk(grid: &VecGrid<Tile>, &start: &P, start_dir: Direction) -> impl Iterator<Item = (Direction, P)> {
    successors(Some((start_dir, start)), |(dir, p)| {
        let next = p.checked_add(*dir)?;
        match grid.get(&next)? {
            Tile::Obstacle => Some((*dir + 1, *p)),
            _ => Some((*dir, next)),
        }
    })
}

fn part1((unmodified_grid, start): &Parsed) -> usize {
    let mut grid = unmodified_grid.to_owned();
    for (_, pos) in walk(unmodified_grid, start, Direction::Up) {
        grid[pos] = Tile::Visited;
    }
    grid.iter().filter(|&&t| t == Tile::Visited).count()
}

fn has_loop(grid: &VecGrid<Tile>, pos: &P, d: Direction, visited: &mut FnvHashSet<usize>) -> usize {
    for (d, p) in walk(grid, pos, d) {
        // Manually “hashing” into a single integer: 50% speedup.
        let key = (d as usize) * 1_000_000 + p[0] * 1000 + p[1];
        if !visited.insert(key) {
            return 1;
        }
    }
    0
}

fn part2((unmodified_grid, start): &Parsed) -> usize {
    let mut grid = unmodified_grid.to_owned();
    // Re-using the allocation each loop: 70% speedup.
    let mut visited = FnvHashSet::default();
    walk(unmodified_grid, start, Direction::Up)
        .unique_by(|(_, p)| *p)
        .map_windows(|&[(d1, p1), (_, p2)]| {
            grid[p2] = Tile::Obstacle;
            let has_loop = has_loop(&grid, &p1, d1, &mut visited);
            grid[p2] = Tile::Empty;
            visited.clear();
            has_loop
        })
        .sum()
}

boilerplate! {
    TEST_INPUT == "\
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."
    for tests: {
        part1: { TEST_INPUT => 41 },
        part2: { TEST_INPUT => 6 },
    },
    bench1 == 4454,
    bench2 == 1503,
    bench_parse: |(_, p): &Parsed| *p => Pos([84, 42]),
}
