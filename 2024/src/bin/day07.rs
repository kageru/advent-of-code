#![feature(test)]
extern crate test;
use aoc2024::{boilerplate, common::*};

const DAY: usize = 07;
type I = i64;
type Parsed = Vec<(I, Vec<I>)>;

enum Op {
    Add,
    Mul,
    Concat,
}

fn parse_input(raw: &str) -> Parsed {
    raw.lines().filter_map(|l| l.split_once(": ")).map(|(r, ns)| (parse_num(r), parse_nums_separator(ns, ' '))).collect()
}

fn concat(a: I, b: I) -> I {
    let log = a.ilog10() + 1;
    a * log as I + b
}

fn can_be_solved(target: I, sum: I, ns: &[I], op: Op) -> bool {
    match (ns, op) {
        ([n], Op::Add) => sum + n == target,
        ([n], Op::Mul) => sum * n == target,
        ([n, ns @ ..], Op::Add) => can_be_solved(target, sum + n, ns, Op::Add) || can_be_solved(target, sum + n, ns, Op::Mul),
        ([n, ns @ ..], Op::Mul) => can_be_solved(target, sum * n, ns, Op::Add) || can_be_solved(target, sum * n, ns, Op::Mul),
        _ => unreachable!(),
    }
}

fn part1(parsed: &Parsed) -> I {
    parsed
        .iter()
        .filter_map(|(r, ns)| (can_be_solved(*r, ns[0], &ns[1..], Op::Add) || can_be_solved(*r, ns[0], &ns[1..], Op::Mul)).then_some(*r))
        .sum()
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "190: 10 19
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
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 7885693428401,
    bench2 == 0,
    bench_parse: Vec::len => 850,
}
