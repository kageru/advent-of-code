#![feature(test)]
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

fn part2(parsed: &Parsed) -> I {
    const SAMPLING: usize = 1024;
    let mut cache = FnvHashMap::default();
    let (vertical, horizontal): (Vec<_>, Vec<_>) = parsed.iter().circular_tuple_windows().partition(|((x1, _), (x2, _))| x1 == x2);
    let vertical = vertical.into_iter().map(|(&(x, y1), &(_, y2))| Edge(x, range(y1, y2))).sorted_by_key(|e| e.0).collect();
    let horizontal = horizontal.into_iter().map(|(&(x1, y), &(x2, _))| Edge(y, range(x1, x2))).sorted_by_key(|e| *e.1.end()).collect();
    let rectangles = sort_by_size(parsed);
    let n = rectangles.len();
    let (_, (p1, p2)) = rectangles
        .into_iter()
        .enumerate()
        .find(|&(i, (&(x1, y1), &(x2, y2)))| {
            println!("Checking combination {i}/{n} ({x1}, {y1}) to ({x2}, {y2}). Cache size is {}", cache.len());
            range(x1, x2)
                .filter(|x| x.is_multiple_of(SAMPLING))
                .flat_map(|x| range(y1, y2).filter(|y| y.is_multiple_of(SAMPLING)).map(move |y| (x, y)))
                .all(|p| *cache.entry(p).or_insert_with(|| point_in_rectangle(p, &vertical, &horizontal)))
                && {
                    println!("Checking {i} more thoroughly");
                    range(x1, x2)
                        .filter(|x| x.is_multiple_of(32))
                        .flat_map(|x| range(y1, y2).filter(|y| y.is_multiple_of(32)).map(move |y| (x, y)))
                        .all(|p| *cache.entry(p).or_insert_with(|| point_in_rectangle(p, &vertical, &horizontal)))
                }
                && {
                    println!("Checking {i} even more thoroughly");
                    range(x1, x2)
                        .filter(|x| x.is_multiple_of(8))
                        .flat_map(|x| range(y1, y2).filter(|y| y.is_multiple_of(8)).map(move |y| (x, y)))
                        .all(|p| *cache.entry(p).or_insert_with(|| point_in_rectangle(p, &vertical, &horizontal)))
                }
                && {
                    println!("Checking {i} even more more thoroughly");
                    range(x1, x2)
                        .filter(|x| x.is_multiple_of(4))
                        .flat_map(|x| range(y1, y2).filter(|y| y.is_multiple_of(4)).map(move |y| (x, y)))
                        .all(|p| point_in_rectangle(p, &vertical, &horizontal))
                }
                && {
                    println!("Actually checking combination {i}/{n}");
                    range(x1, x2).flat_map(|x| range(y1, y2).map(move |y| (x, y))).all(|p| point_in_rectangle(p, &vertical, &horizontal))
                }
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
