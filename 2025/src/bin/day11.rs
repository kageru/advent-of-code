#![feature(test)]
extern crate test;
use aoc2025::{boilerplate, common::*};
use fnv::FnvHashMap as Map;

const DAY: usize = 11;
type Parsed<'a> = Map<&'a str, Vec<&'a str>>;

fn parse_input(raw: &str) -> Parsed<'_> {
    raw.lines().filter_map(|l| l.split_once(": ")).map(|(k, v)| (k, v.split(' ').collect())).collect()
}

fn part1(parsed: &Parsed) -> usize {
    count_paths(parsed, "you")
}

fn count_paths(mappings: &Parsed, position: &str) -> usize {
    if position == "out" {
        return 1;
    }
    mappings[position].iter().map(|p| count_paths(mappings, p)).sum()
}

fn part2(parsed: &Parsed) -> usize {
    let mut cache = Map::default();
    count_paths_2(parsed, "svr", false, false, &mut cache)
}

fn count_paths_2<'a>(
    mappings: &Parsed<'a>,
    position: &'a str,
    dac: bool,
    fft: bool,
    cache: &mut Map<(&'a str, bool, bool), usize>,
) -> usize {
    if position == "out" {
        return (dac && fft) as _;
    }
    let key = (position, dac, fft);
    if let Some(&cached) = cache.get(&key) {
        return cached;
    }
    let n = mappings[position].iter().map(|&p| count_paths_2(mappings, p, dac || p == "dac", fft || p == "fft", cache)).sum();
    cache.insert(key, n);
    n
}

boilerplate! {
    TEST_INPUT == "\
aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out"
    for tests: {
        part1: { TEST_INPUT => 5 },
        part2: { "\
svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out" => 2 },
    },
    bench1 == 683,
    bench2 == 533996779677200,
    bench_parse: Map::len => 589,
}
