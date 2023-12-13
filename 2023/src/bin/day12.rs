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
            valid_combinations(&springs, &expected, CallState::default(), &mut cache)
        })
        .sum()
}

#[derive(Debug, PartialEq, Eq, Hash, Default, Clone, Copy)]
struct CallState {
    index:                 usize,
    broken_count:          usize,
    current_constraint:    usize,
    broken_streak:         usize,
    just_completed_streak: bool,
}

impl CallState {
    fn new(index: usize, broken_count: usize, current_constraint: usize, broken_streak: usize, just_completed_streak: bool) -> Self {
        Self { index, broken_count, current_constraint, broken_streak, just_completed_streak }
    }

    fn operational(self) -> Self {
        CallState::new(self.index + 1, self.broken_count, self.current_constraint, 0, false)
    }
}

fn add_broken(springs: &[Spring], constraints: &[usize], state: CallState, cache: &mut FnvHashMap<CallState, usize>) -> Option<usize> {
    (state.current_constraint < constraints.len()
        && state.broken_count < constraints.iter().take(state.current_constraint + 1).sum()
        && !state.just_completed_streak)
        .then(|| {
            let just_completed_streak = state.broken_streak + 1 == constraints[state.current_constraint];
            valid_combinations(
                springs,
                constraints,
                CallState::new(
                    state.index + 1,
                    state.broken_count + 1,
                    state.current_constraint + just_completed_streak as usize,
                    state.broken_streak + 1,
                    just_completed_streak,
                ),
                cache,
            )
        })
}

fn valid_combinations(springs: &[Spring], constraints: &[usize], state: CallState, cache: &mut FnvHashMap<CallState, usize>) -> usize {
    if let Some(&cached) = cache.get(&state) {
        return cached;
    }
    if state.index == springs.len() {
        return (state.broken_count == constraints.iter().sum::<usize>() && state.current_constraint == constraints.len()) as _;
    }

    let valid = match springs[state.index] {
        Spring::Operational => valid_combinations(springs, constraints, state.operational(), cache),
        Spring::Broken => add_broken(springs, constraints, state, cache).unwrap_or(0),
        Spring::Unknown => {
            let operational = valid_combinations(springs, constraints, state.operational(), cache);
            let broken = add_broken(springs, constraints, state, cache).unwrap_or(0);
            operational + broken
        }
    };
    cache.insert(state, valid);
    valid
}

fn part2(lines: &Parsed) -> usize {
    lines
        .iter()
        .map(|(springs, expected)| {
            // this seems cursed. is there a join() for iterators?
            let springs = springs.to_vec();
            let new_spring_length = springs.len() * 5 + 4;
            (
                springs.into_iter().chain(std::iter::once(Spring::Unknown)).cycle().take(new_spring_length).collect_vec(),
                expected.iter().cloned().cycle().take(expected.len() * 5).collect_vec(),
            )
        })
        .map(|(springs, expected)| {
            let mut cache = FnvHashMap::default();
            valid_combinations(&springs, &expected, CallState::default(), &mut cache)
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
?###???????? 3,2,1"
    for tests: {
        part1: { TEST_INPUT => 21 },
        part2: { TEST_INPUT => 525152 },
    },
    bench1 == 7260,
    bench2 == 1909291258644,
    bench_parse: Vec::len => 1000,
}
