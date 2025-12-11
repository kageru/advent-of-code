#![allow(incomplete_features)]
#![feature(test, generic_const_exprs)]
extern crate test;
use aoc2025::{boilerplate, common::*};
use fnv::FnvHashMap;
use itertools::Itertools;
use std::ops::RangeInclusive;

const DAY: usize = 9;
type I = usize;
type Point = (I, I);
type Parsed = Vec<Point>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines().filter_map(|l| l.split_once(',')).map(|(x, y)| (parse_num(x), parse_num(y))).collect()
}

fn area((x1, y1): &Point, (x2, y2): &Point) -> I {
    (x1.abs_diff(*x2) + 1) * (y1.abs_diff(*y2) + 1)
}

fn sort_by_size(parsed: &Parsed) -> Vec<(&Point, &Point)> {
    parsed.iter().tuple_combinations().sorted_unstable_by_key(|(p1, p2)| area(p1, p2)).rev().collect()
}

fn part1(parsed: &Parsed) -> I {
    sort_by_size(parsed).first().map(|(p1, p2)| area(p1, p2)).unwrap()
}

#[derive(Debug)]
struct Edge(I, RangeInclusive<I>);

fn range<T: Ord>(x: T, y: T) -> RangeInclusive<T> {
    if x > y { y..=x } else { x..=y }
}

fn check_sampled<const SAMPLING: usize>(
    (x1, y1): Point,
    (x2, y2): Point,
    vertical: &Vec<Edge>,
    horizontal: &Vec<Edge>,
    cache: &mut FnvHashMap<Point, bool>,
) -> bool {
    if SAMPLING == 1 {
        println!("Checking potential candidate ({x1}, {y1}) ({x2}, {y2}) at full resolution");
    }
    range(x1, x2)
        .filter(|x| x.is_multiple_of(SAMPLING))
        .flat_map(|x| range(y1, y2).filter(|y| y.is_multiple_of(SAMPLING)).map(move |y| (x, y)))
        .all(|p| {
            // there’s no use caching at very high resolutions, and there’s also not enough RAM even if we wanted to.
            if SAMPLING > 4 {
                *cache.entry(p).or_insert_with(|| point_in_rectangle(p, &vertical, &horizontal))
            } else {
                point_in_rectangle(p, &vertical, &horizontal)
            }
        })
}

fn part2(parsed: &Parsed) -> I {
    let mut cache = FnvHashMap::default();
    let (vertical, horizontal): (Vec<_>, Vec<_>) = parsed.iter().circular_tuple_windows().partition(|((x1, _), (x2, _))| x1 == x2);
    let vertical = vertical.into_iter().map(|(&(x, y1), &(_, y2))| Edge(x, range(y1, y2))).sorted_by_key(|e| e.0).collect();
    let horizontal = horizontal.into_iter().map(|(&(x1, y), &(x2, _))| Edge(y, range(x1, x2))).sorted_by_key(|e| *e.1.end()).collect();
    let rectangles = sort_by_size(parsed);
    let (p1, p2) = rectangles
        .into_iter()
        .find(|&(&p1, &p2)| {
            // check progressively higher resolutions to quickly narrow down the selection.
            // The final check still takes a few minutes to do.
            // The sampling is all consts and powers of 2, to make the is_multiple_of as cheap as possible.
            check_sampled::<1024>(p1, p2, &vertical, &horizontal, &mut cache)
                && check_sampled::<32>(p1, p2, &vertical, &horizontal, &mut cache)
                && check_sampled::<8>(p1, p2, &vertical, &horizontal, &mut cache)
                && check_sampled::<4>(p1, p2, &vertical, &horizontal, &mut cache)
                && check_sampled::<1>(p1, p2, &vertical, &horizontal, &mut cache)
        })
        .unwrap();
    area(p1, p2)
}

fn point_in_rectangle((x, y): Point, vertical_edges: &Vec<Edge>, horizontal_edges: &Vec<Edge>) -> bool {
    let mut intersections = 0usize;
    for e in vertical_edges.iter().take_while(|e| e.0 <= x).filter(|e| e.1.contains(&y)) {
        if e.0 == x {
            return true;
        }
        intersections += 1;
    }
    intersections += horizontal_edges.iter().take_while(|e| *e.1.end() < x).filter(|e| e.0 == y).count();
    !intersections.is_multiple_of(2)
}

boilerplate! {
    TEST_INPUT == "\
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3"
    for tests: {
        part1: { TEST_INPUT => 50 },
        part2: { TEST_INPUT => 24 },
    },
    bench1 == 4750092396,
    bench2 == 1468516555,
    bench_parse: Vec::len => 496,
}
