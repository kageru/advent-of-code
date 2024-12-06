#![feature(test)]
extern crate test;
use aoc2024::{
    boilerplate,
    common::*,
    direction::Direction,
    grid::{Grid, VecGrid},
    position::Pos,
};
use fnv::FnvHashSet;

const DAY: usize = 6;
type P = Pos<usize, 2>;
type Parsed = (VecGrid<Tile>, P);

#[derive(Clone, Copy, PartialEq, Eq)]
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

fn try_move(Pos([y, x]): P, dir: Direction) -> Option<P> {
    Some(match dir {
        Direction::Up => Pos([y + 1, x]),
        Direction::Down => Pos([y.checked_sub(1)?, x]),
        Direction::Right => Pos([y, x + 1]),
        Direction::Left => Pos([y, x.checked_sub(1)?]),
    })
}

fn part1((grid, start): &Parsed) -> usize {
    let mut grid = grid.to_owned();
    let mut dir = Direction::Up;
    let mut pos = *start;
    grid[pos] = Tile::Visited;
    while let Some(next) = try_move(pos, dir) {
        match grid.get_mut(&next) {
            None => break,
            Some(e @ Tile::Empty) => {
                *e = Tile::Visited;
                pos = next;
            }
            Some(Tile::Obstacle) => dir.turn(1),
            _ => pos = next,
        }
    }
    grid.iter().filter(|&&t| t == Tile::Visited).count()
}

fn has_loop(mut grid: VecGrid<Tile>, mut pos: P) -> usize {
    let mut visited = FnvHashSet::default();
    let mut dir = Direction::Up;
    while let Some(next) = try_move(pos, dir) {
        match grid.get_mut(&next) {
            None => break,
            Some(Tile::Empty) => pos = next,
            Some(Tile::Obstacle) => dir.turn(1),
            _ => pos = next,
        }
        if !visited.insert((dir, pos)) {
            return 1;
        }
    }
    0
}

fn part2((grid, start): &Parsed) -> usize {
    grid.indices()
        .filter(|&p| grid[p] == Tile::Empty)
        .map(|p| {
            let mut grid = grid.clone();
            *grid.get_mut(&p).unwrap() = Tile::Obstacle;
            has_loop(grid, *start)
        })
        .sum()
}

boilerplate! {
    TEST_INPUT == "....#.....
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
    bench_parse: |(_, p): &Parsed| *p => Pos([84usize, 42]),
}
