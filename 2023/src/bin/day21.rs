#![feature(test, try_blocks)]
extern crate test;
use aoc2023::{boilerplate, common::*};
use fnv::FnvHashSet as Set;
use std::mem::transmute;

const DAY: usize = 21;
type Garden<'a> = Vec<&'a [Tile]>;
type Parsed<'a> = (Garden<'a>, (usize, usize));

#[repr(u8)]
#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
enum Tile {
    Empty = b'.',
    Rock = b'#',
    Start = b'S',
}

fn parse_input(raw: &str) -> Parsed {
    let garden: Garden = raw.lines().map(|l| unsafe { transmute(l) }).collect();
    let start = garden
        .iter()
        .enumerate()
        .find_map(|(y, row)| row.iter().enumerate().find_map(|(x, t)| (*t == Tile::Start).then_some((x, y))))
        .unwrap();
    (garden, start)
}

fn step(garden: &Garden, x: usize, y: usize, steps_left: usize, plots: &mut Set<(usize, usize)>, cache: &mut Set<(usize, usize, usize)>) {
    if steps_left == 0 {
        plots.insert((x, y));
        return;
    }
    if cache.contains(&(x, y, steps_left)) {
        return;
    }
    [try { (x.checked_sub(1)?, y) }, Some((x + 1, y)), try { (x, y.checked_sub(1)?) }, Some((x, y + 1))]
        .into_iter()
        .flatten()
        .filter(|&(x, y)| matches!(try { garden.get(y)?.get(x)? }, Some(Tile::Start | Tile::Empty)))
        .for_each(|(x, y)| step(garden, x, y, steps_left - 1, plots, cache));
    cache.insert((x, y, steps_left));
}

fn part1((garden, (x, y)): &Parsed, steps: usize) -> usize {
    let mut cache = Set::default();
    let mut visited = Set::default();
    step(garden, *x, *y, steps, &mut visited, &mut cache);
    visited.len()
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "\
...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
..........."
    for tests: {
        part1: { TEST_INPUT, 6 => 16 },
        part2: { TEST_INPUT => 0 },
    },
    bench1(64) == 3646,
    bench2 == 0,
    bench_parse: |(garden, (x, y)): &Parsed| (garden.len(), *x, *y) => (131, 65, 65),
}
