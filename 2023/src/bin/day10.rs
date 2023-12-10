#![feature(test)]
extern crate test;
use fnv::FnvHashSet as HashSet;
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
type Pos = Position2D<isize>;

fn parse_input(raw: &str) -> Parsed {
    let grid = raw.lines().rev().map(|l| unsafe { transmute::<&str, &[Pipe]>(l) }).collect_vec();
    let start = grid
        .iter()
        .zip(0..)
        .find_map(|(line, y)| line.iter().zip(0..).find_map(|(p, x)| (*p == Pipe::Start).then_some(PositionND([x, y]))))
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
        .filter_map(|mut dir| {
            let mut pos = *start;
            let mut steps = 0;
            loop {
                steps += 1;
                pos = step(pos, dir);
                if &pos == start {
                    return Some(steps / 2);
                }
                dir = *grid.get(pos.0[1] as usize)?.get(pos.0[0] as usize)?.openings().iter().find(|&&o| o != !dir).unwrap();
            }
        })
        .max()
        .unwrap()
}

fn part2((start, grid): &Parsed) -> usize {
    let mut corners = Vec::new();
    let mut dir = Direction::Down; // I got this from my part 1 solution.
    let mut pos = *start;
    let mut points = HashSet::default();
    loop {
        pos = step(pos, dir);
        let pipe = grid[pos.0[1] as usize][pos.0[0] as usize];
        points.insert(pos);
        if matches!(pipe, Pipe::Start | Pipe::TopLeft | Pipe::TopRight | Pipe::BottomLeft | Pipe::BottomRight) {
            corners.push(pos);
        }
        if &pos == start {
            break;
        }
        dir = *pipe.openings().iter().find(|&&o| o != !dir).unwrap();
    }
    let ylen = grid.len() as isize;
    let xlen = grid[0].len() as isize;
    (0..ylen)
        .flat_map(|y| (0..xlen).map(move |x| PositionND([x, y])))
        .filter(|p| !points.contains(p))
        .filter(|p| is_inside(p, &corners))
        .count()
}

fn is_inside(p: &Pos, polygon: &Vec<Pos>) -> bool {
    let mut counter = 0;
    let mut p1 = polygon[0];
    let mut p2;
    for i in 1..=polygon.len() {
        p2 = polygon[i % polygon.len()];
        if p[1] > p1[1].min(p2[1]) {
            if p[1] <= p1[1].max(p2[1]) {
                if p[0] <= p1[0].max(p2[0]) {
                    if p1[1] != p2[1] {
                        let xinters = (p[1] - p1[1]) * (p2[0] - p1[0]) / (p2[1] - p1[1]) + p1[0];
                        if p1[0] == p2[0] || p[0] <= xinters {
                            counter += 1;
                        }
                    }
                }
            }
        }
        p1 = p2;
    }

    println!("Counter for {p:?} was {counter}");
    return counter % 2 != 0;
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
        part2: {
            INPUT_P2 => 4,
            INPUT_P2_2 => 10,
        },
    },
    bench1 == 6923,
    bench2 == 529,
    bench_parse: |p: &Parsed| (p.0, p.1.len()) => (PositionND([114, 117]), 140),
}

#[cfg(test)]
const INPUT_P2: &str = "\
..........
.S------7.
.|F----7|.
.||....||.
.||....||.
.|L-7F-J|.
.|..||..|.
.L--JL--J.
..........";
#[cfg(test)]
const INPUT_P2_2: &str = "\
FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJIF7FJ-
L---JF-JLJIIIIFJLJJ7
|F|F-JF---7IIIL7L|7|
|FFJF7L7F-JF7IIL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L";
