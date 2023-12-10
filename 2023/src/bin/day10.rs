#![feature(test)]
extern crate test;
use std::mem::transmute;

use aoc2023::{
    boilerplate,
    common::*,
    direction::{Direction, ALL_DIRECTIONS},
    position::{Position2D, PositionND},
};
use itertools::Itertools;

const DAY: usize = 10;

#[allow(dead_code)]
#[derive(Debug, Copy, Clone, PartialEq)]
#[repr(u8)]
enum Pipe {
    Vertical = b'|',
    Horizontal = b'-',
    TopRight = b'L',
    TopLeft = b'J',
    BottomRight = b'F',
    BottomLeft = b'7',
    None = b'.',
    Start = b'S',
}

impl Pipe {
    fn openings(self) -> [Direction; 2] {
        match self {
            Pipe::Vertical => [Direction::Up, Direction::Down],
            Pipe::Horizontal => [Direction::Left, Direction::Right],
            Pipe::TopRight => [Direction::Up, Direction::Right],
            Pipe::TopLeft => [Direction::Up, Direction::Left],
            Pipe::BottomRight => [Direction::Down, Direction::Right],
            Pipe::BottomLeft => [Direction::Down, Direction::Left],
            Pipe::None | Pipe::Start => unimplemented!(),
        }
    }
}

type Parsed<'a> = (Pos, Vec<&'a [Pipe]>);
type Pos = Position2D<usize>;

fn parse_input(raw: &str) -> Parsed {
    let grid = raw.lines().rev().map(|l| unsafe { transmute::<&str, &[Pipe]>(l) }).collect_vec();
    let start = grid
        .iter()
        .enumerate()
        .find_map(|(y, line)| line.iter().enumerate().find_map(|(x, p)| (*p == Pipe::Start).then_some(PositionND([x, y]))))
        .unwrap();
    (start, grid)
}

fn step(PositionND([x, y]): Pos, dir: Direction) -> Pos {
    match dir {
        Direction::Up => PositionND([x, y + 1]),
        Direction::Right => PositionND([x + 1, y]),
        Direction::Left => PositionND([x - 1, y]),
        Direction::Down => PositionND([x, y - 1]),
    }
}

fn part1((start, grid): &Parsed) -> usize {
    ALL_DIRECTIONS
        .iter()
        .cloned()
        .map(|mut dir| {
            let mut pos = *start;
            let mut steps = 0;
            loop {
                steps += 1;
                pos = step(pos, dir);
                if &pos == start {
                    return steps / 2;
                }
                dir = *grid[pos.0[1]][pos.0[0]].openings().iter().find(|&&o| o != !dir).unwrap();
            }
        })
        .max()
        .unwrap()
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "\
-L|F7
7S-7|
L|7||
-L-J|
L|-JF",
    tests: {
        part1: { TEST_INPUT => 4 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 6923,
    bench2 == 0,
    bench_parse: |p: &Parsed| (p.0, p.1.len()) => (PositionND([114, 117]), 140),
}
