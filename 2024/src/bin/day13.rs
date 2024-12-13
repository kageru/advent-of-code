#![feature(test)]
extern crate test;
use aoc2024::{boilerplate, common::*};
use itertools::Itertools;
use tuple_map::TupleMap2;

const DAY: usize = 13;
type I = isize;
type Parsed = Vec<(I, I, I, I, I, I)>;

const X_COST: I = 3;
const Y_COST: I = 1;

fn parse_button(line: &str) -> (I, I) {
    line["Button _: X+".len()..].split_once(", Y+").unwrap().map(parse_num)
}

fn parse_input(raw: &str) -> Parsed {
    raw.split("\n\n")
        .map(|block| {
            let (a_line, b_line, prize_line) = block.lines().collect_tuple().unwrap();
            let (a1, a2) = parse_button(a_line);
            let (b1, b2) = parse_button(b_line);
            let (c1, c2) = prize_line.trim_start_matches("Prize: X=").split_once(", Y=").unwrap().map(parse_num);
            (a1, a2, b1, b2, c1, c2)
        })
        .collect()
}

fn int_div(x: I, y: I) -> Option<I> {
    let r = x.checked_div(y)?;
    (r * y == x).then_some(r)
}

fn solve<const OFFSET: I>(parsed: &Parsed) -> I {
    parsed
        .iter()
        .filter_map(|&(a1, a2, b1, b2, mut c1, mut c2)| {
            c1 += OFFSET;
            c2 += OFFSET;
            // look mom, I can implement Wikipedia algorithms without even renaming the variables.
            let div = a1 * b2 - b1 * a2;
            Some(int_div(c1 * b2 - b1 * c2, div)? * X_COST + int_div(a1 * c2 - c1 * a2, div)? * Y_COST)
        })
        .sum()
}

fn part1(parsed: &Parsed) -> I {
    solve::<0>(parsed)
}

fn part2(parsed: &Parsed) -> I {
    solve::<10000000000000>(parsed)
}

boilerplate! {
    TEST_INPUT == "\
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279"
    for tests: {
        part1: { TEST_INPUT => 480 },
        part2: { TEST_INPUT => 875318608908 },
    },
    bench1 == 37680,
    bench2 == 87550094242995,
    bench_parse: Vec::len => 320,
}
