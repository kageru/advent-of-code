#![feature(test)]
extern crate test;
use aoc2024::{boilerplate, common::*};
use fnv::FnvHashMap;

const DAY: usize = 19;
type Parsed<'a> = (Vec<&'a str>, Vec<&'a str>);

fn parse_input(raw: &str) -> Parsed<'_> {
    let (raw_towels, raw_patterns) = raw.split_once("\n\n").unwrap();
    (raw_towels.split(", ").collect(), raw_patterns.lines().collect())
}

fn solvable(pattern: &str, towels: &[&str]) -> bool {
    match pattern {
        "" => true,
        _ => towels.iter().filter_map(|t| pattern.strip_prefix(t)).any(|p| solvable(p, towels)),
    }
}

fn solutions<'a>(pattern: &'a str, towels: &[&str], cache: &mut FnvHashMap<&'a str, usize>) -> usize {
    if pattern.is_empty() {
        return 1;
    }
    if let Some(&cached) = cache.get(pattern) {
        return cached;
    }
    let res = towels.iter().filter_map(|t| pattern.strip_prefix(t)).map(|p| solutions(p, towels, cache)).sum();
    cache.insert(pattern, res);
    res
}

fn part1((towels, patterns): &Parsed) -> usize {
    patterns.iter().filter(|p| solvable(p, towels)).count()
}

fn part2((towels, patterns): &Parsed) -> usize {
    let mut cache = FnvHashMap::default();
    patterns.iter().map(|p| solutions(p, towels, &mut cache)).sum()
}

boilerplate! {
    TEST_INPUT == "\
r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb"
    for tests: {
        part1: { TEST_INPUT => 6 },
        part2: { TEST_INPUT => 16 },
    },
    bench1 == 374,
    bench2 == 1100663950563322,
    bench_parse: |(a, b): &Parsed| (a.len(), b.len()) => (447, 400),
}
