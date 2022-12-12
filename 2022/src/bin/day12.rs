#![feature(test)]
extern crate test;
use aoc2022::{
    boilerplate,
    common::*,
    grid::{Grid, Position2D, VecGrid},
};

const DAY: usize = 12;
type Parsed = (Position2D, Position2D, VecGrid<u8>);

fn find_and_modify(grid: &mut Vec<Vec<u8>>, needle: u8, replacement: u8) -> (usize, usize) {
    grid.iter_mut()
        .enumerate()
        .find_map(|(x, line)| {
            line.iter_mut()
                .enumerate()
                .find_map(|(y, b)| {
                    (b == &needle).then(|| {
                        *b = replacement;
                        y
                    })
                })
                .map(|y| (x, y))
        })
        .unwrap()
}

fn parse_input(raw: &str) -> Parsed {
    let mut grid = VecGrid::from_bytes_2d(raw, |b| b);
    let start = find_and_modify(&mut grid.fields, b'S', b'a');
    let start = Position2D::from([start.0, start.1]);
    let end = find_and_modify(&mut grid.fields, b'E', b'z');
    let end = Position2D::from([end.0, end.1]);
    (start, end, grid)
}

fn part1((start, end, grid): &Parsed) -> usize {
    let mut distances = VecGrid { fields: vec![vec![usize::MAX; grid.fields[0].len()]; grid.len()] };
    distances[*start] = 0;
    0
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi",
    tests: {
        part1: { TEST_INPUT => 31 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 0,
    bench2 == 0,
    bench_parse: |&((sx, sy), (ex, ey), _)| ((sx, sy), (ex, ey)) => ((20, 0), (20, 145)),
}
