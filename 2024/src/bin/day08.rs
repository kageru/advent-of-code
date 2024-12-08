#![feature(test)]
extern crate test;
use std::{
    iter::successors,
    ops::{Add, Sub},
};

use aoc2024::{
    boilerplate,
    common::*,
    grid::{Grid, VecGrid},
    position::Pos,
};
use itertools::Itertools as _;

const DAY: usize = 8;
type P = Pos<usize, 2>;
const FREQUENCIES: usize = b'z' as usize + 1;
type Parsed = (VecGrid<u8>, Vec<Vec<P>>);

fn parse_input(raw: &str) -> Parsed {
    let grid: VecGrid<u8> = VecGrid::from_bytes_2d(raw, |b| b);
    let mut antennas = [const { Vec::new() }; FREQUENCIES];
    for p in grid.indices().filter(|&p| grid[p] != b'.') {
        antennas[grid[p] as usize].push(p);
    }
    let mut antennas = antennas.to_vec();
    antennas.retain(|ps| ps.len() > 1);
    (grid, antennas)
}

fn try_usizes((x, y): (i64, i64)) -> Option<(usize, usize)> {
    (x >= 0 && y >= 0).then_some((x as _, y as _))
}

fn expand_pairs_with<F: Fn((i64, i64, i64, i64)) -> I, I: IntoIterator<Item = P>>(antennas: &[Vec<P>], f: F) -> usize {
    antennas
        .iter()
        .flat_map(|ps| {
            ps.iter().map(|&Pos([x, y])| (x as i64, y as i64)).tuple_combinations().map(|((x1, y1), (x2, y2))| (x1, y1, x1 - x2, y1 - y2))
        })
        .flat_map(f)
        .sorted()
        .dedup()
        .count()
}

fn part1((grid, antennas): &Parsed) -> usize {
    expand_pairs_with(antennas, |(x, y, xdiff, ydiff)| {
        [(x + xdiff, y + ydiff), (x - 2 * xdiff, y - 2 * ydiff)]
            .into_iter()
            .flat_map(try_usizes)
            .map(|(x, y)| Pos([x, y]))
            .filter(|p| grid.get(p).is_some())
    })
}

fn follow_to_edge<F: Fn(i64, i64) -> i64>(x: i64, y: i64, xdiff: i64, ydiff: i64, grid: &VecGrid<u8>, f: F) -> impl Iterator<Item = P> {
    successors(Some((x as _, y as _)), move |&(x, y)| try_usizes((f(x as i64, xdiff), f(y as i64, ydiff))))
        .map(|(x, y)| Pos([x, y]))
        .take_while(|p| grid.get(p).is_some())
}

fn part2((grid, antennas): &Parsed) -> usize {
    expand_pairs_with(antennas, |(x, y, xdiff, ydiff)| {
        follow_to_edge(x, y, xdiff, ydiff, grid, Add::add).chain(follow_to_edge(x, y, xdiff, ydiff, grid, Sub::sub))
    })
}

boilerplate! {
    TEST_INPUT == "\
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"
    for tests: {
        part1: { TEST_INPUT => 14 },
        part2: { TEST_INPUT => 34 },
    },
    bench1 == 413,
    bench2 == 1417,
    bench_parse: |(grid, _): &Parsed| grid.len() => 50,
}
