#![feature(test)]
extern crate test;
use aoc2023::{
    boilerplate,
    common::*,
    direction::Direction::{self, *},
    position::{Position2D, PositionND},
};
use fnv::FnvHashSet as HashSet;
use itertools::{Itertools, MinMaxResult};

const DAY: usize = 18;
type I = isize;
type Pos = Position2D<I>;
type Parsed = Vec<(Direction, I, u32)>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines()
        .map(|line| line.split(' ').collect_tuple().unwrap())
        .map(|(d, l, c)| {
            let dir = match d {
                "R" => Right,
                "D" => Down,
                "U" => Up,
                "L" => Left,
                _ => unreachable!(),
            };
            (dir, parse_num(l), u32::from_str_radix(&c[2..8], 16).unwrap())
        })
        .collect()
}

fn part1(instructions: &Parsed) -> usize {
    let mut edges = Vec::new();
    let mut points = HashSet::default();
    let mut pos = PositionND([0, 0]);
    points.insert(pos);
    edges.push(pos);
    for &(dir, len, _) in instructions {
        let movement = PositionND(match dir {
            Up => [1, 0],
            Down => [-1, 0],
            Right => [0, 1],
            Left => [0, -1],
        });
        for _ in 0..len {
            pos += movement;
            points.insert(pos);
        }
        edges.push(pos);
    }
    let MinMaxResult::MinMax(xmin, xmax) = edges.iter().map(|p| p[0]).minmax() else { unreachable!() };
    let MinMaxResult::MinMax(ymin, ymax) = edges.iter().map(|p| p[1]).minmax() else { unreachable!() };
    let mut count = 0;
    for x in xmin..=xmax {
        for y in ymin..=ymax {
            let p = PositionND([x, y]);
            if points.contains(&p) || is_inside(&p, &edges) {
                count += 1;
            }
        }
    }
    count
}

// copied from day 10
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

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "\
R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)"
    for tests: {
        part1: { TEST_INPUT => 62 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 35991,
    bench2 == 0,
    bench_parse: Vec::len => 602,
}
