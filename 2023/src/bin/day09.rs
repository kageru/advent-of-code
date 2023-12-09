#![feature(test)]
extern crate test;
use aoc2023::{boilerplate, common::*};
use itertools::Itertools;

const DAY: usize = 9;
type I = i64;
type Parsed = Vec<Vec<I>>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines().map(|l| parse_nums_separator(l, ' ')).collect()
}

fn build_levels(levels: &mut Parsed) {
    loop {
        let previous = levels.last().unwrap();
        let level = previous.iter().zip(previous.iter().skip(1)).map(|(a, b)| b - a).collect_vec();
        levels.push(level);
        if levels.last().unwrap().iter().all(|n| n == &0) {
            break;
        }
    }
}

fn part1(lines: &Parsed) -> I {
    lines
        .iter()
        .map(|line| {
            let mut levels = vec![line.to_owned()];
            build_levels(&mut levels);
            for i in (1..levels.len()).rev() {
                let new = levels[i].last().unwrap_or(&0) + levels[i - 1].last().unwrap();
                levels[i - 1].push(new);
            }
            *levels[0].last().unwrap()
        })
        .sum()
}

fn part2(lines: &Parsed) -> I {
    lines
        .iter()
        .map(|line| {
            let mut levels = vec![line.to_owned()];
            build_levels(&mut levels);
            for i in (1..levels.len()).rev() {
                let new = levels[i - 1].first().unwrap_or(&0) - levels[i][0];
                levels[i - 1].insert(0, new);
            }
            levels[0][0]
        })
        .sum()
}

boilerplate! {
    TEST_INPUT == "\
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45",
    tests: {
        part1: { TEST_INPUT => 114 },
        part2: { TEST_INPUT => 2 },
    },
    bench1 == 2038472161,
    bench2 == 1091,
    bench_parse: Vec::len => 200,
}
