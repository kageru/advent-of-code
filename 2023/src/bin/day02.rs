#![feature(test)]
extern crate test;
use aoc2023::{boilerplate, common::*};

const DAY: usize = 2;
type Parsed = Vec<Vec<Pull>>;

#[derive(Debug)]
struct Pull {
    red:   usize,
    green: usize,
    blue:  usize,
}

fn parse_input(raw: &str) -> Parsed {
    raw.lines()
        .map(|l| {
            l.after(": ")
                .split("; ")
                .map(|p| p.split(", ").map(|b| b.split_once(' ').unwrap()).collect())
                .map(|ps: Vec<_>| Pull { red: find_color(&ps, "red"), green: find_color(&ps, "green"), blue: find_color(&ps, "blue") })
                .collect()
        })
        .collect()
}

fn find_color(ps: &[(&str, &str)], color: &str) -> usize {
    ps.iter().find_map(|(n, c)| (*c == color).then(|| parse_num(n))).unwrap_or(0)
}

fn part1(parsed: &Parsed) -> usize {
    parsed.iter().zip(1..).filter_map(|(ps, n)| ps.iter().all(|p| p.red <= 12 && p.green <= 13 && p.blue <= 14).then_some(n)).sum()
}

fn part2(parsed: &Parsed) -> usize {
    parsed
        .iter()
        .map(|ps| {
            ps.iter().max_by_key(|p| p.red).unwrap().red
                * ps.iter().max_by_key(|p| p.green).unwrap().green
                * ps.iter().max_by_key(|p| p.blue).unwrap().blue
        })
        .sum()
}

boilerplate! {
    TEST_INPUT == "\
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green",
    tests: {
        part1: { TEST_INPUT => 8 },
        part2: { TEST_INPUT => 2286 },
    },
    bench1 == 2207,
    bench2 == 62241,
    bench_parse: Vec::len => 100,
}
