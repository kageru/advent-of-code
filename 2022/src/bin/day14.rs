#![feature(test, array_windows)]
extern crate test;
use aoc2022::{boilerplate, common::*};
use std::iter::repeat;

const DAY: usize = 14;
const X_OFFSET: usize = 300;
const SAND_SOURCE: (usize, usize) = (500 - X_OFFSET, 0);
const CAVE_HEIGHT: usize = 200;
const CAVE_WIDTH: usize = CAVE_HEIGHT * 2;
type Cave = [[bool; CAVE_HEIGHT]; CAVE_WIDTH];
type Parsed = (Cave, usize);

fn parse_input(raw: &str) -> Parsed {
    let mut cave = [[false; CAVE_HEIGHT]; CAVE_WIDTH];
    let mut max_y = 0;
    let mut segments: Vec<(usize, usize)> = Vec::with_capacity(10);
    for line in raw.lines() {
        segments
            .extend(line.split(" -> ").map(|s| s.split_once(',').unwrap()).map(|(x, y)| (parse_num::<usize>(x) - X_OFFSET, parse_num(y))));
        for &[(x1, y1), (x2, y2)] in segments.array_windows() {
            for (x, y) in ((x1.min(x2))..=(x1.max(x2))).flat_map(|x| repeat(x).zip((y1.min(y2))..=(y1.max(y2)))) {
                cave[x][y] = true;
            }
        }
        max_y = max_y.max(segments.drain(..).map(|(_, y)| y).max().unwrap());
    }
    (cave, max_y)
}

fn simulate((x, y): (usize, usize), cave: &Cave) -> Option<(usize, usize)> {
    if y >= CAVE_HEIGHT - 1 {
        None
    } else if !cave[x][y + 1] {
        simulate((x, y + 1), cave)
    } else if !cave[x - 1][y + 1] {
        simulate((x - 1, y + 1), cave)
    } else if !cave[x + 1][y + 1] {
        simulate((x + 1, y + 1), cave)
    } else {
        Some((x, y))
    }
}

// abusing scan here to simulate a stateful for-loop with a counter
fn part1((cave, _): &Parsed) -> usize {
    repeat(()).scan(cave.to_owned(), |cave, _| simulate(SAND_SOURCE, &cave).map(|(x, y)| cave[x][y] = true)).count()
}

fn part2((cave, max_y): &Parsed) -> usize {
    let mut cave = cave.to_owned();
    cave.iter_mut().for_each(|row| row[max_y + 2] = true);
    1 + repeat(())
        .scan(cave, |cave, _| simulate(SAND_SOURCE, &cave).map(|(x, y)| cave[x][y] = true).filter(|_| !cave[SAND_SOURCE.0][SAND_SOURCE.1]))
        .count()
}

boilerplate! {
    TEST_INPUT == "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9",
    tests: {
        part1: { TEST_INPUT => 24 },
        part2: { TEST_INPUT => 93 },
    },
    bench1 == 674,
    bench2 == 24958,
    bench_parse: |(_, y): &Parsed| *y => 161,
}
