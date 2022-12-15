#![feature(test, result_option_inspect)]
extern crate test;

use aoc2022::{boilerplate, common::*};
use itertools::Itertools;

const DAY: usize = 15;
type Parsed = Vec<((i64, i64), (i64, i64), u64)>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines()
        .map(|line| {
            <[&str; 4]>::try_from(line[12..].replace(", y=", ",").replace(": closest beacon is at x=", ",").split(',').collect_vec())
                .unwrap()
                .map(|s| s.parse().unwrap())
        })
        .map(|[x1, y1, x2, y2]| ((x1, y1), (x2, y2)))
        .map(|(t1, t2)| (t1, t2, manhattan(t1, t2)))
        .collect()
}

fn manhattan((x1, y1): (i64, i64), (x2, y2): (i64, i64)) -> u64 {
    x1.abs_diff(x2) + y1.abs_diff(y2)
}

fn part1(parsed: &Parsed, y: i64) -> usize {
    let max_distance = *parsed.iter().map(|(_, _, d)| d).max().unwrap();
    let (&min_x, &max_x) = parsed.iter().map(|((x1, _), _, _)| x1).minmax().into_option().unwrap();

    let mut c = 0;
    let mut nearest = None;
    for x in (min_x - max_distance as i64)..=(max_x + max_distance as i64) {
        match nearest {
            None => {
                nearest = parsed.iter().find(|(p, _, d)| manhattan(*p, (x, y)) <= *d);
                if nearest.is_some() {
                    c += 1;
                }
            }
            Some(&(p, _, d)) => {
                if manhattan(p, (x, y)) <= d {
                    c += 1;
                } else {
                    nearest = parsed.iter().min_by_key(|(p, _, _)| manhattan(*p, (x, y))).filter(|(p, _, d)| manhattan(*p, (x, y)) <= *d);
                    if nearest.is_some() {
                        c += 1;
                    }
                }
            }
        }
    }
    c
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
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
        part2: { TEST_INPUT => 0 },
    },
    unittests: {
        manhattan: {
            (8,7), (2,10) => 9,
            (8,7), (-1,7) => 9_,
        }
    },
    bench1(2_000_000) == 0, // 4827922 too low
    bench2 == 0,
    bench_parse: Vec::len => 25,
}
