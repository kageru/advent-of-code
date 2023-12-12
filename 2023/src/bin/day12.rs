#![feature(test)]
extern crate test;
use aoc2023::{boilerplate, common::*};
use fnv::FnvHashMap;
use itertools::Itertools;
use std::mem::transmute;

const DAY: usize = 12;
type I = usize;
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

fn part1(lines: &Parsed) -> usize {
    lines
        .iter()
        .map(|(springs, expected)| {
            let mut cache = FnvHashMap::default();
            valid_combinations_recursive(&springs, &expected, 0, 0, 0, 0, false, &mut cache)
        })
        .sum()
}

fn valid_combinations_recursive(
    springs: &[Spring],
    constraints: &[usize],
    index: usize,
    broken_count: usize,
    current_constraint: usize,
    broken_streak: usize,
    just_completed_streak: bool,
    cache: &mut FnvHashMap<(usize, usize, usize, usize, bool), usize>,
) -> usize {
    if let Some(&cached) = cache.get(&(index, broken_count, current_constraint, broken_streak, just_completed_streak)) {
        return cached;
    }
    if index == springs.len() {
        let valid = broken_count == constraints.iter().sum::<usize>()
            && current_constraint == constraints.len()
            && (broken_streak == 0 || just_completed_streak);
        return valid as usize;
    }

    let valid = match springs[index] {
        Spring::Operational => {
            valid_combinations_recursive(springs, constraints, index + 1, broken_count, current_constraint, 0, false, cache)
        }
        Spring::Broken => {
            if current_constraint < constraints.len()
                && broken_count < constraints.iter().take(current_constraint + 1).sum()
                && !just_completed_streak
            {
                let just_completed_streak = broken_streak + 1 == constraints[current_constraint];
                valid_combinations_recursive(
                    springs,
                    constraints,
                    index + 1,
                    broken_count + 1,
                    current_constraint + just_completed_streak as usize,
                    broken_streak + 1,
                    just_completed_streak,
                    cache,
                )
            } else {
                0
            }
        }
        Spring::Unknown => {
            let operational =
                valid_combinations_recursive(springs, constraints, index + 1, broken_count, current_constraint, 0, false, cache);
            let broken = if current_constraint < constraints.len()
                && broken_count < constraints.iter().take(current_constraint + 1).sum()
                && !just_completed_streak
            {
                let just_completed_streak = broken_streak + 1 == constraints[current_constraint];
                valid_combinations_recursive(
                    springs,
                    constraints,
                    index + 1,
                    broken_count + 1,
                    current_constraint + just_completed_streak as usize,
                    broken_streak + 1,
                    just_completed_streak,
                    cache,
                )
            } else {
                0
            };
            operational + broken
        }
    };
    cache.insert((index, broken_count, current_constraint, broken_streak, just_completed_streak), valid);
    valid
}

fn part2(lines: &Parsed) -> usize {
    lines
        .iter()
        .map(|(springs, expected)| {
            let springs = springs.to_vec();
            let new_spring_length = springs.len() * 5 + 4;
            (
                springs.into_iter().chain(std::iter::once(Spring::Unknown)).cycle().take(new_spring_length).collect_vec(),
                expected.iter().cloned().cycle().take(expected.len() * 5).collect_vec(),
            )
        })
        .map(|(springs, expected)| {
            let mut cache = FnvHashMap::default();
            valid_combinations_recursive(&springs, &expected, 0, 0, 0, 0, false, &mut cache)
        })
        .sum()
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
        part2: { TEST_INPUT => 525152 },
    },
    bench1 == 7260,
    bench2 == 1909291258644,
    bench_parse: Vec::len => 1000,
}
