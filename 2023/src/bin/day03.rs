#![feature(test, try_blocks)]
extern crate test;
use aoc2023::{
    boilerplate,
    common::*,
    grid::{Position2D, PositionND},
};
use itertools::Itertools;

const DAY: usize = 3;
type Parsed<'a> = Vec<&'a [u8]>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines().map(|l| l.as_bytes()).collect()
}

fn part1(parsed: &Parsed) -> usize {
    let number_positions = parsed
        .iter()
        .enumerate()
        .flat_map(|(x, l)| l.iter().enumerate().filter_map(move |(y, c)| matches!(c, b'0'..=b'9').then_some((x, y..=y))))
        .coalesce(
            |(x1, y1), (x2, y2)| if y1.end() + 1 == *y2.start() { Ok((x1, *y1.start()..=*y2.end())) } else { Err(((x1, y1), (x2, y2))) },
        )
        .collect_vec();
    let mut sum = 0usize;
    for (x, ys) in number_positions {
        let start = Position2D::from([x, *ys.start()]);
        let end = Position2D::from([x, *ys.end()]);
        if start
            .neighbors()
            .into_iter()
            .chain(end.neighbors())
            .any(|PositionND([x, y])| !matches!(parsed.get(x as usize).and_then(|ys| ys.get(y as usize)), Some(b'0'..=b'9' | b'.') | None))
        {
            sum += String::from_utf8_lossy(&parsed[x][ys]).parse::<usize>().unwrap();
        }
    }
    sum
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
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
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 536202,
    bench2 == 0,
    bench_parse: Vec::len => 0,
}
