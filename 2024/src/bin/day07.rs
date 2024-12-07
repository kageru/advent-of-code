#![feature(test)]
extern crate test;
use aoc2024::{boilerplate, common::*};

const DAY: usize = 7;
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
    let log = b.ilog10() + 1;
    (a * 10i64.pow(log) as I) + b
}

fn can_be_solved_p1(target: I, sum: I, ns: &[I]) -> bool {
    can_be_solved::<false>(target, sum, ns, Op::Add) || can_be_solved::<false>(target, sum, ns, Op::Mul)
}

fn can_be_solved_p2(target: I, sum: I, ns: &[I]) -> bool {
    can_be_solved::<true>(target, sum, ns, Op::Add)
        || can_be_solved::<true>(target, sum, ns, Op::Mul)
        || can_be_solved::<true>(target, sum, ns, Op::Concat)
}

fn can_be_solved<const P2: bool>(target: I, sum: I, ns: &[I], op: Op) -> bool {
    match (ns, op) {
        ([n], Op::Add) => sum + n == target,
        ([n], Op::Mul) => sum * n == target,
        ([n], Op::Concat) => concat(sum, *n) == target,
        ([n, ns @ ..], Op::Add) => {
            if P2 {
                can_be_solved_p2(target, sum + n, ns)
            } else {
                can_be_solved_p1(target, sum + n, ns)
            }
        }
        ([n, ns @ ..], Op::Mul) => {
            if P2 {
                can_be_solved_p2(target, sum * n, ns)
            } else {
                can_be_solved_p1(target, sum * n, ns)
            }
        }
        ([n, ns @ ..], Op::Concat) => {
            if P2 {
                can_be_solved_p2(target, concat(sum, *n), ns)
            } else {
                can_be_solved_p1(target, concat(sum, *n), ns)
            }
        }
        _ => unreachable!(),
    }
}

fn part1(parsed: &Parsed) -> I {
    parsed.iter().filter_map(|(r, ns)| (can_be_solved_p1(*r, ns[0], &ns[1..])).then_some(*r)).sum()
}

fn part2(parsed: &Parsed) -> I {
    parsed.iter().filter_map(|(r, ns)| (can_be_solved_p2(*r, ns[0], &ns[1..])).then_some(*r)).sum()
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
