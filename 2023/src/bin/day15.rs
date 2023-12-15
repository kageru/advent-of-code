#![feature(test, type_alias_impl_trait)]
extern crate test;
use aoc2023::{boilerplate, common::*};

const DAY: usize = 15;
type I = u32;
type Parsed<'a> = impl Iterator<Item = &'a str> + Clone;

fn parse_input(raw: &str) -> Parsed {
    raw.trim_end_matches('\n').split(',')
}

fn hash(s: &str) -> I {
    s.bytes().fold(0, |acc, b| (acc + b as I) * 17 & 255)
}

fn part1(parsed: &Parsed) -> I {
    parsed.clone().map(hash).inspect(|n| println!("{n}")).sum()
}

fn part2(parsed: &Parsed) -> I {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
    for tests: {
        part1: { TEST_INPUT => 1320 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 510273,
    bench2 == 0,
    bench_parse: |i: &Parsed| i.clone().count() => 4000,
}
