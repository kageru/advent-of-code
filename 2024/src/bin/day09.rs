#![feature(test)]
extern crate test;
use std::iter;

use aoc2024::{boilerplate, common::*};

const DAY: usize = 9;
type I = usize;
type Parsed = Vec<Option<I>>;

fn parse_input(raw: &str) -> Parsed {
    let len = raw.bytes().map(|b| (b - b'0') as usize).sum();
    let mut disk = Vec::with_capacity(len);
    let mut gap = false;
    let mut id = 0;
    for size in raw.bytes().map(|b| (b - b'0') as usize) {
        if gap {
            disk.extend(iter::repeat_n(None, size));
        } else {
            disk.extend(iter::repeat_n(Some(id), size));
            id += 1;
        }
        gap = !gap;
    }
    disk
}

fn part1(disk: &Parsed) -> usize {
    let mut disk = disk.to_owned();
    let mut from = disk.len() - 1;
    let mut to = 0;
    while from > to {
        match (disk[from], disk[to]) {
            (src @ Some(_), None) => {
                disk[to] = src;
                disk[from] = None;
                from -= 1;
                to += 1;
            }
            (None, _) => from -= 1,
            (_, Some(_)) => to += 1,
        }
    }
    disk.into_iter().flatten().enumerate().map(|(i, id)| i * id).sum()
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "2333133121414131402"
    for tests: {
        part1: { TEST_INPUT => 1928 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 6283170117911,
    bench2 == 0,
    bench_parse: Vec::len => 95023,
}
