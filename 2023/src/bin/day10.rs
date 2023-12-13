#![feature(test)]
extern crate test;
use aoc2023::{
    boilerplate,
    common::*,
    direction::{Direction, ALL_DIRECTIONS},
    position::{Position2D, PositionND},
};
use fnv::FnvHashSet as HashSet;
use itertools::{Itertools, MinMaxResult};
use std::mem::transmute;

const DAY: usize = 10;

type Parsed<'a> = (Pos, Vec<&'a [Pipe]>);
type Pos = Position2D<isize>;

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
        use self::{Direction::*, Pipe::*};
        match self {
            Vertical => [Up, Down],
            Horizontal => [Left, Right],
            TopRight => [Up, Right],
            TopLeft => [Up, Left],
            BottomRight => [Down, Right],
            BottomLeft => [Down, Left],
            None | Start => unimplemented!(),
        }
    }
}

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
    let MinMaxResult::MinMax(&ymin, &ymax) = corners.iter().map(|PositionND([y, _])| y).minmax() else { unreachable!() };
    let MinMaxResult::MinMax(&xmin, &xmax) = corners.iter().map(|PositionND([_, x])| x).minmax() else { unreachable!() };
    (ymin..=ymax)
        .flat_map(|y| (xmin..=xmax).map(move |x| PositionND([x, y])))
        .filter(|p| !points.contains(p))
        .filter(|p| is_inside(p, &corners))
        .count()
}

// A reimplementation of https://www.eecs.umich.edu/courses/eecs380/HANDOUTS/PROJ2/InsidePoly.html
fn is_inside(p: &Pos, polygon: &[Pos]) -> bool {
    // Zip with next and wrap for the last element
    polygon
        .iter()
        .zip(polygon.iter().cycle().skip(1))
        .filter(|(p1, p2)| p[1] > p1[1].min(p2[1]) && p[1] <= p1[1].max(p2[1]) && p[0] <= p1[0].max(p2[0]) && p1[1] != p2[1])
        .filter(|(p1, p2)| p1[0] == p2[0] || p[0] <= (p[1] - p1[1]) * (p2[0] - p1[0]) / (p2[1] - p1[1]) + p1[0])
        .count()
        & 1
        == 1
}

boilerplate! {
    TEST_INPUT == "\
-L|F7
7S-7|
L|7||
-L-J|
L|-JF"
    for tests: {
        part1: { TEST_INPUT => 4 },
        part2: {
            INPUT_P2 => 4,
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
