#![feature(test)]
extern crate test;
use std::mem::transmute;

use aoc2023::{boilerplate, common::*};
use itertools::Itertools;

const DAY: usize = 12;
type I = u32;
type Parsed<'a> = Vec<(&'a [Spring], Vec<I>)>;

#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
#[allow(dead_code)]
enum Spring {
    Operational = b'.',
    Broken = b'#',
    Unknown = b'?',
}

fn parse_input(raw: &str) -> Parsed {
    raw.lines()
        .map(|l| l.split_once(' ').unwrap())
        .map(|(springs, counts)| (unsafe { transmute(springs) }, parse_nums_separator(counts, ',')))
        .collect()
}

fn is_legal(springs: &[Spring], expected: &[I]) -> bool {
    let broken = springs
        .iter()
        .map(|s| (s, 1))
        .coalesce(|(s1, n1), (s2, n2)| if s1 == s2 { Ok((s1, n1 + n2)) } else { Err(((s1, n1), (s2, n2))) })
        .filter_map(|(&s, n)| (s == Spring::Broken).then_some(n))
        .collect_vec();
    broken == expected
}

fn fill(springs: &mut Vec<Vec<Spring>>, base: Vec<Spring>, mut unknowns: Vec<usize>) {
    match unknowns.pop() {
        Some(i) => {
            let mut clone = base.clone();
            clone[i] = Spring::Operational;
            fill(springs, clone, unknowns.clone());
            let mut clone = base.clone();
            clone[i] = Spring::Broken;
            fill(springs, clone, unknowns.clone());
        }
        None => springs.push(base),
    }
}

fn part1(lines: &Parsed) -> usize {
    lines
        .iter()
        .map(|(springs, expected)| {
            let unknowns = springs.iter().enumerate().filter_map(|(i, s)| (s == &Spring::Unknown).then_some(i)).collect_vec();
            let mut buffer = Vec::with_capacity(1 << unknowns.len());
            fill(&mut buffer, springs.to_vec(), unknowns);
            buffer.iter().filter(|s| is_legal(s, expected)).count()
        })
        .sum()
}

fn part2(_parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "\
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1",
    tests: {
        part1: { TEST_INPUT => 21 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 7260,
    bench2 == 0,
    bench_parse: Vec::len => 1000,
}
