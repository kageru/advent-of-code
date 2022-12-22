#![feature(test)]
extern crate test;
use std::fmt;

use aoc2022::{
    boilerplate,
    common::*,
    grid::{Direction, Grid, HashGrid, PositionND},
};
use itertools::Itertools;

const DAY: usize = 22;
type Parsed = (HashGrid<Field, 2>, Vec<(i64, Turn)>);

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
enum Field {
    Empty = b'.' as isize,
    Wall = b'#' as isize,
    #[default]
    Missing = b' ' as isize,
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", *self as u8 as char)
    }
}

#[derive(Copy, Clone, Debug)]
enum Turn {
    Left,
    Right,
    None,
}

fn parse_input(raw: &str) -> Parsed {
    let (maze, moves) = raw.split_once("\n\n").unwrap();
    let grid = maze
        .lines()
        .zip(1..)
        .flat_map(|(line, x)| {
            line.bytes().zip(1..).filter(|(b, _)| *b != b' ').map(move |(b, y)| {
                let f = match b {
                    b'.' => Field::Empty,
                    b'#' => Field::Wall,
                    _ => unreachable!(),
                };
                (PositionND::from([x, y]), f)
            })
        })
        .collect();
    let moves = moves
        .split_inclusive(['L', 'R'])
        .map(|s| s.split_at(s.len() - 1))
        .map(|(n, d)| {
            let d = match d {
                "L" => Turn::Left,
                "R" => Turn::Right,
                _ => Turn::None,
            };
            (parse_num(n), d)
        })
        .collect_vec();
    debug_assert_eq!(moves.iter().filter(|(_, d)| matches!(d, Turn::None)).count(), 1, "Only the last entry should have no turn");
    (grid, moves)
}
fn part1((grid, instructions): &Parsed) -> i64 {
    let mut pos = *grid.fields.keys().filter(|p| p.0[0] == 1).min_by_key(|p| p.0[1]).unwrap();
    let mut dir = Direction::Right;
    for &(steps, turn) in instructions {
        for _ in 0..steps {
            let next = PositionND::from(match dir {
                Direction::Up => [-1, 0],
                Direction::Down => [1, 0],
                Direction::Left => [0, -1],
                Direction::Right => [0, 1],
            }) + pos;
            pos = match grid.get(&next) {
                Some(Field::Wall) => break,
                Some(_) => next,
                None => {
                    let (&new_pos, &wall) = match dir {
                        Direction::Up => grid.fields.iter().filter(|(p, _)| p.0[1] == pos.0[1]).max_by_key(|(p, _)| p.0[0]),
                        Direction::Down => grid.fields.iter().filter(|(p, _)| p.0[1] == pos.0[1]).min_by_key(|(p, _)| p.0[0]),
                        Direction::Left => grid.fields.iter().filter(|(p, _)| p.0[0] == pos.0[0]).max_by_key(|(p, _)| p.0[1]),
                        Direction::Right => grid.fields.iter().filter(|(p, _)| p.0[0] == pos.0[0]).min_by_key(|(p, _)| p.0[1]),
                    }
                    .unwrap();
                    if wall == Field::Wall {
                        break;
                    } else {
                        new_pos
                    }
                }
            }
        }
        dir = match (dir, turn) {
            (_, Turn::None) => break,
            (d, Turn::Right) => d + 1,
            (d, Turn::Left) => d + -1,
        };
    }
    println!("{pos:?}, {dir:?}");
    pos.0[0] * 1000 + pos.0[1] * 4 + (dir as i64)
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5
",
    tests: {
        part1: { TEST_INPUT => 6032 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 123046,
    bench2 == 0,
    bench_parse: |(g, i): &Parsed| g.len() + i.len() => 17001,
}
