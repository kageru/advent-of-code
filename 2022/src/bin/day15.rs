#![feature(test, iter_array_chunks)]
extern crate test;

use aoc2022::{boilerplate, common::*};
use itertools::Itertools;

const DAY: usize = 15;
type Parsed = Vec<((i64, i64), u64)>;

fn parse_input(raw: &str) -> Parsed {
    raw.replace(", y=", ",")
        .replace(": closest beacon is at x=", ",")
        .lines()
        .flat_map(|line| line[12..].split(','))
        .map(|s| s.parse().unwrap())
        .array_chunks()
        .map(|[x1, y1, x2, y2]| ((x1, y1), manhattan((x1, y1), (x2, y2))))
        .collect()
}

fn manhattan((x1, y1): (i64, i64), (x2, y2): (i64, i64)) -> u64 {
    x1.abs_diff(x2) + y1.abs_diff(y2)
}

fn part1(parsed: &Parsed, y: i64) -> i64 {
    let (&min_x, &max_x) = parsed.iter().map(|((x1, _), _)| x1).minmax().into_option().unwrap();
    let mut x = min_x - *parsed.iter().map(|(_, d)| d).max().unwrap() as i64;
    let mut c = -1;
    while x <= max_x {
        match parsed.iter().find(|(p, d)| manhattan(*p, (x, y)) <= *d) {
            Some(&((px, py), d)) => {
                let new_x = px + d as i64 - (py.abs_diff(y) as i64) + 1;
                c += new_x - x;
                x = new_x;
            }
            None => {
                match parsed.iter().filter(|&&((x1, y1), d)| x1 > x && y1.abs_diff(y) < d).min_by_key(|&&(p, d)| manhattan(p, (x, y)) - d) {
                    Some(&((px, py), d)) => x = px - (d as i64 - (py.abs_diff(y) as i64)),
                    None => break,
                }
            }
        };
    }
    c
}

fn part2(parsed: &Parsed, bounds: i64) -> i64 {
    for x in 0..=bounds {
        let mut y = 0;
        while y <= bounds {
            match parsed.iter().find(|(p, d)| manhattan(*p, (x, y)) <= *d) {
                Some(&((px, py), d)) => y = py + d as i64 - (px.abs_diff(x) as i64) + 1,
                None => return x * 4_000_000 + y,
            };
        }
    }
    unreachable!("Did not find an eligible spot")
}

boilerplate! {
    TEST_INPUT == "\
Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3",
    tests: {
        part1: { TEST_INPUT, 10 => 26 },
        part2: { TEST_INPUT, 20 => 56000011 },
    },
    unittests: {
        manhattan: {
            (8,7), (2,10) => 9,
            (8,7), (-1,7) => 9_,
        }
    },
    bench1(2_000_000) == 4827924,
    bench2(4_000_000) == 12977110973564,
    bench_parse: Vec::len => 25,
}
