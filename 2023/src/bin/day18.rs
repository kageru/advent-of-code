#![feature(test)]
extern crate test;
use aoc2023::{
    boilerplate,
    common::*,
    direction::Direction::{self, *},
    position::{Position2D, PositionND},
};
use itertools::Itertools;

const DAY: usize = 18;
type I = isize;
type Pos = Position2D<I>;
type Parsed = Vec<((Direction, I), (Direction, I))>;

fn parse_dir(c: u8) -> Direction {
    match c {
        b'R' | b'0' => Right,
        b'D' | b'1' => Down,
        b'L' | b'2' => Left,
        b'U' | b'3' => Up,
        _ => unreachable!(),
    }
}

fn parse_input(raw: &str) -> Parsed {
    raw.lines()
        .map(|line| line.split(' ').collect_tuple().unwrap())
        .map(|(d, l, c)| {
            ((parse_dir(d.as_bytes()[0]), parse_num(l)), (parse_dir(c.as_bytes()[7]), I::from_str_radix(&c[2..7], 16).unwrap()))
        })
        .collect()
}

fn part1(instructions: &Parsed) -> isize {
    solve(instructions.iter().map(|(a, _)| a))
}

fn part2(instructions: &Parsed) -> isize {
    solve(instructions.iter().map(|(_, b)| b))
}

fn solve<'a>(instructions: impl Iterator<Item = &'a (Direction, I)> + Clone) -> isize {
    let n_points: I = instructions.clone().map(|(_, len)| len).sum();
    let points: Vec<_> = instructions
        .scan(PositionND([0, 0]), |pos, &(dir, len)| {
            let movement = PositionND(match dir {
                Up => [len, 0],
                Down => [-len, 0],
                Right => [0, len],
                Left => [0, -len],
            });
            *pos += movement;
            Some(*pos)
        })
        .collect();
    (2 * area(&points) - n_points + 2) / 2 + n_points
}

fn area(polygon: &[Pos]) -> isize {
    polygon.iter().zip(polygon.iter().cycle().skip(1)).map(|(p1, p2)| p1[0] * p2[1] - p1[1] * p2[0]).sum::<I>().abs() >> 1
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
        part2: { TEST_INPUT => 952408144115 },
    },
    unittests: {
        area: {
            &[
                PositionND([0, 0]),
                PositionND([0, 2]),
                PositionND([2, 2]),
                PositionND([2, 0]),
            ] => 4
        }
    },
    bench1 == 35991,
    bench2 == 54058824661845,
    bench_parse: Vec::len => 602,
}
