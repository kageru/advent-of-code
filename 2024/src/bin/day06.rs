#![feature(test)]
extern crate test;
use aoc2024::{
    boilerplate,
    common::*,
    direction::Direction,
    grid::{Grid, VecGrid},
    position::Pos,
};

const DAY: usize = 6;
type Parsed = (VecGrid<Tile>, Pos<usize, 2>);

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
#[allow(dead_code)] // not actually dead, we transmute into it
enum Tile {
    Empty = b'.',
    Obstacle = b'#',
    Visited = b'X',
    Guard = b'^',
}

fn parse_input(raw: &str) -> Parsed {
    let grid = VecGrid { fields: raw.lines().rev().map(|l| unsafe { std::mem::transmute::<_, &[Tile]>(l.as_bytes()).to_vec() }).collect() };
    let start = (0..grid.len())
        .flat_map(|y| (0..grid.fields[0].len()).map(move |x| Pos([y, x])))
        .find(|p| grid.get(p) == Some(&Tile::Guard))
        .expect("No guard on map");
    (grid, start)
}

fn mov(Pos([y, x]): Pos<usize, 2>, dir: Direction) -> Option<Pos<usize, 2>> {
    Some(match dir {
        Direction::Up => Pos([y + 1, x]),
        Direction::Down => Pos([y.checked_sub(1)?, x]),
        Direction::Right => Pos([y, x + 1]),
        Direction::Left => Pos([y, x.checked_sub(1)?]),
    })
}

fn part1((grid, start): &Parsed) -> usize {
    let mut grid = grid.to_owned();
    *grid.get_mut(start).unwrap() = Tile::Visited;
    let mut dir = Direction::Up;
    let mut pos = *start;
    while let Some(next) = mov(pos, dir) {
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
    grid.fields.iter().flatten().filter(|&&t| t == Tile::Visited).count()
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
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
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 4454,
    bench2 == 0,
    bench_parse: |(_, p): &Parsed| *p => Pos([84usize, 42]),
}
