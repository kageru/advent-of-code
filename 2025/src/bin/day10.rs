#![feature(test)]
extern crate test;
use aoc2025::{boilerplate, common::*};
use itertools::Itertools;

const DAY: usize = 10;
type Parsed = Vec<(Vec<bool>, Vec<Vec<usize>>, Vec<usize>)>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines()
        .filter_map(|l| {
            let (desired, rest) = l[1..(l.len() - 1)].split_once("] (")?;
            let desired = desired.bytes().map(|b| b == b'#').collect();
            let (switches, joltage) = rest.split_once(") {")?;
            let switches = switches.split(") (").map(|s| parse_nums_separator(s, ',')).collect();
            let joltage = parse_nums_separator(joltage, ',');
            Some((desired, switches, joltage))
        })
        .collect()
}

/// The least significant bit is at index 0.
fn int_to_bits(i: u16) -> Vec<bool> {
    (0..(16 - i.leading_zeros() as usize)).map(|b| (i >> b) & 1 != 0).collect()
}

fn flip(lights: &mut [bool], switch: &[usize]) {
    for &i in switch {
        lights[i] = !lights[i];
    }
}

fn part1(parsed: &Parsed) -> u32 {
    parsed
        .iter()
        .filter_map(|(desired, switches, _)| {
            (0u16..(1 << switches.len())).sorted_unstable_by_key(|i| i.count_ones()).find_map(|i| {
                let mut lights = vec![false; desired.len()];
                let bits = int_to_bits(i);
                for switch in bits.into_iter().zip(switches).filter_map(|(b, s)| b.then_some(s)) {
                    flip(&mut lights, &switch);
                }
                (&lights == desired).then_some(i.count_ones())
            })
        })
        .sum()
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

#[cfg(test)]
mod unittests {
    use super::*;

    #[test]
    fn test_int_to_bits() {
        assert_eq!(int_to_bits(7), &[true, true, true], "7");
        assert_eq!(int_to_bits(8), &[false, false, false, true], "8");
    }
}

boilerplate! {
    TEST_INPUT == "\
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
    for tests: {
        part1: { TEST_INPUT => 7 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 455,
    bench2 == 0,
    bench_parse: Vec::len => 167,
}
