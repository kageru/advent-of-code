#![feature(test, try_blocks)]
extern crate test;
use std::ops::RangeInclusive;

use aoc2023::{
    boilerplate,
    common::*,
    position::{Position2D, PositionND},
};
use itertools::Itertools;

const DAY: usize = 3;
type I = i32;
type Grid<'a> = Vec<&'a [u8]>;
type Parsed<'a> = (Grid<'a>, Vec<Vec<(usize, RangeInclusive<usize>)>>);

fn parse_input(raw: &str) -> Parsed {
    let grid = raw.lines().map(|l| l.as_bytes()).collect_vec();
    let number_positions = grid
        .iter()
        .enumerate()
        .map(|(x, l)| {
            l.iter()
                .enumerate()
                .filter_map(move |(y, c)| c.is_ascii_digit().then_some((x, y..=y)))
                .coalesce(
                    |(x1, y1), (x2, y2)| {
                        if y1.end() + 1 == *y2.start() {
                            Ok((x1, *y1.start()..=*y2.end()))
                        } else {
                            Err(((x1, y1), (x2, y2)))
                        }
                    },
                )
                .collect_vec()
        })
        .collect_vec();
    (grid, number_positions)
}

fn parse_at(grid: &Grid, (x, ys): (usize, RangeInclusive<usize>)) -> usize {
    String::from_utf8_lossy(&grid[x][ys]).parse::<usize>().unwrap()
}

fn part1((grid, number_positions): &Parsed) -> usize {
    number_positions
        .iter()
        .flatten()
        .cloned()
        .filter_map(|(x, ys)| {
            let start = PositionND([x as I, *ys.start() as I]);
            let end = PositionND([x as I, *ys.end() as I]);
            start
                .neighbors()
                .into_iter()
                .chain(end.neighbors())
                .any(|PositionND([x, y])| {
                    !matches!(grid.get(x as usize).and_then(|ys| ys.get(y as usize)), Some(b'0'..=b'9' | b'.') | None)
                })
                .then(|| parse_at(grid, (x, ys)))
        })
        .sum()
}

fn part_of(PositionND([_, y]): Position2D<I>, (_, ys): &(usize, RangeInclusive<usize>)) -> bool {
    ys.contains(&(y as usize))
}

fn part2((grid, number_positions): &Parsed) -> usize {
    grid.iter()
        .enumerate()
        .flat_map(|(x, ys)| ys.iter().enumerate().filter_map(move |(y, &b)| (b == b'*').then_some(PositionND([x as I, y as I]))))
        .filter_map(|p| {
            let neighbors = p.neighbors();
            number_positions[(p.0[0].dec() as usize)..=(p.0[0].inc() as usize)]
                .iter()
                .flatten()
                .filter_map(|np| neighbors.iter().find_map(|&n| part_of(n, np).then(|| parse_at(grid, np.clone()))))
                .collect_tuple()
        })
        .map(|(a, b)| a * b)
        .sum()
}

boilerplate! {
    TEST_INPUT == "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..",
    tests: {
        part1: { TEST_INPUT => 4361 },
        part2: { TEST_INPUT => 467835 },
    },
    bench1 == 536202,
    bench2 == 78272573,
    bench_parse: |(g, ns): &Parsed| (g.len(), ns.len()) => (140, 140),
}
