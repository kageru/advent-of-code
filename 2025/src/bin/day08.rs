#![feature(test)]
extern crate test;
use aoc2025::{boilerplate, common::*};
use fnv::FnvHashSet;
use itertools::Itertools;

const DAY: usize = 8;
type I = i64;
type Point = (I, I, I);
type Parsed = Vec<Point>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines().map(|l| l.split(',').map(parse_num).collect_tuple().unwrap()).collect()
}

fn part1(parsed: &Parsed) -> usize {
    let pairs = parsed.iter().tuple_combinations().sorted_by_cached_key(|(p1, p2)| distance(p1, p2)).collect_vec();
    let mut circuits = Vec::<FnvHashSet<Point>>::new();
    let limit = if parsed.len() < 100 { 10 } else { 1000 }; // meh
    for (p1, p2) in pairs.into_iter().take(limit) {
        match &mut circuits.iter_mut().filter(|c| c.contains(p1) || c.contains(p2)).collect_vec()[..] {
            [] => {
                let mut c = FnvHashSet::default();
                c.insert(*p1);
                c.insert(*p2);
                circuits.push(c);
            }
            [c] => {
                c.insert(*p1);
                c.insert(*p2);
            }
            [c1, c2] => {
                c1.extend(c2.drain());
                circuits.retain(|c| !c.is_empty());
            }
            _ => unreachable!(),
        }
    }
    circuits.iter().map(|c| c.len()).sorted().rev().take(3).product()
}

fn distance((x1, y1, z1): &Point, (x2, y2, z2): &Point) -> I {
    (x1 - x2).pow(2) + (y1 - y2).pow(2) + (z1 - z2).pow(2)
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "\
162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689"
    for tests: {
        part1: { TEST_INPUT => 40 },
        part2: { TEST_INPUT => 25272 },
    },
    bench1 == 46398,
    bench2 == 0,
    bench_parse: Vec::len => 1000,
}
