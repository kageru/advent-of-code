#![feature(test)]
extern crate test;
use aoc2023::{boilerplate, common::*};
use itertools::Itertools;
use tuple_map::TupleMap2;

const DAY: usize = 6;
type I = usize;
type Parsed = (Vec<(I, I)>, (f64, f64));

fn parse_input(raw: &str) -> Parsed {
    let lines = raw.lines().collect_tuple::<(_, _)>().unwrap();
    let (time, distance) = lines.map(|l| l.split_ascii_whitespace().skip(1).map(parse_num).collect_vec());
    let part1 = time.into_iter().zip(distance).collect();
    let part2 = lines.map(|l| l.after(":").replace(' ', "").parse().unwrap());
    (part1, part2)
}

fn part1((races, _): &Parsed) -> usize {
    races.iter().cloned().map(|(time, distance)| (1..time).filter(|i| i * (time - i) > distance).count()).product()
}

fn part2((_, (time, distance)): &Parsed) -> usize {
    let x1 = time / 2.0 + (time * time / 4.0 - distance).sqrt();
    let x2 = time / 2.0 - (time * time / 4.0 - distance).sqrt();
    (x1.floor() - x2.ceil()) as usize + 1
}

boilerplate! {
    TEST_INPUT == "\
Time:      7  15   30
Distance:  9  40  200"
    for tests: {
        part1: { TEST_INPUT => 288 },
        part2: { TEST_INPUT => 71503 },
    },
    bench1 == 131376,
    bench2 == 34123437,
    bench_parse: |(v, _): &Parsed| v.len() => 4,
}
