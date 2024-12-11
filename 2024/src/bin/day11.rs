#![feature(test)]
extern crate test;
use aoc2024::{boilerplate, common::*};
use fnv::FnvHashMap as HashMap;

const DAY: usize = 11;
type I = usize;
type Parsed = Vec<I>;

fn parse_input(raw: &str) -> Parsed {
    parse_nums_separator(raw, ' ')
}

fn split(n: I, log: u32) -> (I, I) {
    let exp = 10usize.pow(log / 2 + 1);
    let lhs = n / exp;
    (lhs, n - lhs * exp)
}

/// Using our own keying function instead of HashMap<(I, I), I> is about 30% faster.
#[inline(always)]
fn key(stone: I, iterations: I) -> I {
    iterations * 1_000_000_000_000 + stone
}

fn expand(stone: I, iterations: I, cache: &mut HashMap<I, I>) -> I {
    if iterations == 0 {
        return 1;
    }
    if let Some(&n) = cache.get(&key(stone, iterations)) {
        return n;
    }
    let res = match stone.checked_ilog10() {
        None => expand(1, iterations - 1, cache),
        Some(log) if log & 1 == 1 => {
            let (a, b) = split(stone, log);
            expand(a, iterations - 1, cache) + expand(b, iterations - 1, cache)
        }
        Some(_) => expand(stone * 2024, iterations - 1, cache),
    };
    cache.insert(key(stone, iterations), res);
    res
}

fn solve(parsed: &Parsed, iterations: I) -> I {
    let mut cache = HashMap::default();
    parsed.iter().map(|&stone| expand(stone, iterations, &mut cache)).sum()
}

fn part1(parsed: &Parsed) -> I {
    solve(parsed, 25)
}

fn part2(parsed: &Parsed) -> I {
    solve(parsed, 75)
}

boilerplate! {
    TEST_INPUT == "125 17"
    for tests: {
        part1: { TEST_INPUT => 55312 },
        part2: { TEST_INPUT => 65601038650482 },
    },
    bench1 == 222461,
    bench2 == 264350935776416,
    bench_parse: Vec::len => 8,
}
