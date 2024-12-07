#![feature(test)]
extern crate test;
use aoc2024::{boilerplate, common::*};
use std::ops::{Add, Mul};

const DAY: usize = 7;
type I = usize;
type Parsed = Vec<(I, Vec<I>)>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines().filter_map(|l| l.split_once(": ")).map(|(r, ns)| (parse_num(r), parse_nums_separator(ns, ' '))).collect()
}

const OPS: [fn(I, I) -> I; 3] = [Add::add, Mul::mul, concat];

fn concat(a: I, b: I) -> I {
    (a * 10usize.pow(b.ilog10() + 1)) + b
}

fn can_be_solved_inner<F: Fn(I, I) -> I, const NUM_OPS: usize>(target: I, sum: I, ns: &[I], op: F) -> bool {
    match ns {
        [n] => op(sum, *n) == target,
        [n, ns @ ..] => OPS.iter().take(NUM_OPS).any(|&f| can_be_solved_inner::<_, NUM_OPS>(target, op(sum, *n), ns, f)),
        _ => unreachable!(),
    }
}

fn can_be_solved<const N: usize>(parsed: &Parsed) -> I {
    parsed.iter().filter_map(|(r, ns)| OPS.iter().take(N).any(|&f| can_be_solved_inner::<_, N>(*r, ns[0], &ns[1..], f)).then_some(*r)).sum()
}

fn part1(parsed: &Parsed) -> I {
    can_be_solved::<2>(parsed)
}

fn part2(parsed: &Parsed) -> I {
    can_be_solved::<3>(parsed)
}

boilerplate! {
    TEST_INPUT == "\
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"
    for tests: {
        part1: { TEST_INPUT => 3749 },
        part2: {
            TEST_INPUT => 11387,
            "156: 15 6" => 156,
            "3267: 81 40 27" => 3267,
        },
    },
    unittests: {
        concat: { 12, 34 => 1234 },
        concat: { 15, 6 => 156 },
        concat: { 1234, 5678 => 12345678 },
    },
    bench1 == 7885693428401,
    bench2 == 348360680516005,
    bench_parse: Vec::len => 850,
}
