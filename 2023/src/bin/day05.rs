#![feature(test)]
extern crate test;
use std::ops::Range;

use aoc2023::{boilerplate, common::*};
use itertools::Itertools;

const DAY: usize = 5;
type I = i64;
type Mapping = Vec<(Range<I>, I)>;
type Parsed = (Vec<I>, Vec<Mapping>);

fn parse_input(raw: &str) -> Parsed {
    let mut groups = raw.split("\n\n");
    let seeds = groups.next().unwrap().trim_start_matches("seeds: ").split(' ').map(parse_num).collect();
    let ranges = groups
        .map(|g| {
            g.lines()
                .skip(1)
                .map(|l| l.split(' ').map(parse_num).collect_tuple().unwrap())
                .map(|(dst, src, len)| (src..src + len, dst - src))
                .collect()
        })
        .collect();
    (seeds, ranges)
}

fn resolve(start: I, mappings: &Vec<Mapping>) -> I {
    mappings.iter().fold(start, |i, map| map.iter().find_map(|(range, offset)| range.contains(&i).then_some(i + offset)).unwrap_or(i))
}

fn part1((seeds, mappings): &Parsed) -> I {
    seeds.iter().map(|&s| resolve(s, mappings)).min().unwrap()
}

fn part2(parsed: &Parsed) -> I {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4",
    tests: {
        part1: { TEST_INPUT => 35 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 462648396,
    bench2 == 0,
    bench_parse: |(v1, v2): &Parsed| (v1.len(), v2.len()) => (20, 7),
}
