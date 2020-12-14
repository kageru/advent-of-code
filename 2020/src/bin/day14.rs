#![feature(test, str_split_once, destructuring_assignment, bool_to_option)]
extern crate test;
use itertools::Itertools;
use std::collections::{HashMap, HashSet};

use aoc2020::common::*;

enum Command<'a> {
    BitMask(&'a str),
    MemSet { addr: usize, val: usize },
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum BitState {
    One,
    Zero,
    Floating,
}

/*
impl PartialEq<BitState> for BitState {
    fn eq(&self, other: &BitState) -> bool {
        match (self, other) {
            (BitState::One, BitState::One) => true,
            (BitState::Zero, BitState::Zero) => true,
            (BitState::Floating, _) => true,
            (_, BitState::Floating) => true,
            _ => false,
        }
    }
}
*/

type Parsed<'a> = Vec<Command<'a>>;

fn read_input() -> String {
    read_file(14)
}

fn parse_input<'a>(raw: &'a str) -> Parsed<'a> {
    raw.lines()
        .map(|l| {
            if let Some(l) = l.strip_prefix("mask = ") {
                Command::BitMask(l)
            } else if let Some((addr, val)) = l.strip_prefix("mem[").and_then(|l| l.split_once("] = ")) {
                Command::MemSet {
                    addr: addr.parse().unwrap(),
                    val:  val.parse().unwrap(),
                }
            } else {
                unreachable!()
            }
        })
        .collect()
}

fn calc_bitmask<F: FnMut(&(usize, u8)) -> bool>(b: &str, f: F) -> usize {
    b.bytes().rev().enumerate().filter(f).fold(0, |acc, (n, _)| acc | 1 << n)
}

fn calc_bitmask_p2(b: &str) -> Vec<BitState> {
    b.bytes()
        .map(|b| match b {
            b'0' => BitState::Zero,
            b'1' => BitState::One,
            b'X' => BitState::Floating,
            _ => unreachable!(),
        })
        .collect()
}

fn calc_bitmasks_p1(b: &str) -> (usize, usize) {
    (calc_bitmask(b, |(_, b)| *b == b'1'), calc_bitmask(b, |(_, b)| *b != b'0'))
}

fn part1<'a>(parsed: &Parsed<'a>) -> usize {
    let (mut ones, mut zeros) = (0, 0);
    let mut mem = HashMap::new();
    for command in parsed {
        match command {
            Command::BitMask(bm) => (ones, zeros) = calc_bitmasks_p1(bm),
            Command::MemSet { addr, val } => {
                mem.insert(addr, (val & zeros) | ones);
            }
        }
    }
    mem.values().sum()
}

// TODO: clean this all up or just delete it straight away because whatever I was trying to do here
// doesn’t work for the real input, and I’m pretty sure it can’t, but I also don’t know what to do.
fn part2<'a>(parsed: &Parsed<'a>) -> u128 {
    // let mut ones = 0;
    let mut floating = vec![];
    // let mut mem = HashMap::new();
    let mut floating_mem = vec![];
    for command in parsed {
        match command {
            Command::BitMask(bm) => {
                // ones = calc_bitmasks_p1(bm).0;
                floating = calc_bitmask_p2(bm);
            }
            Command::MemSet { addr, val } => {
                let addr = format!("{:036b}", addr)
                    .bytes()
                    .zip(floating.iter())
                    .map(|(b, f)| match (b, f) {
                        (_, BitState::Floating) => BitState::Floating,
                        (b'1', _) | (_, BitState::One) => BitState::One,
                        (b'0', _) => BitState::Zero,
                        _ => unreachable!(),
                    })
                    .collect_vec();
                // mem.insert(addr, val);
                floating_mem.push((addr, val));
                // floating.entry((addr, xs)).or_insert_with(Vec::new).push(val);
            }
        }
    }
    let mut sum = 0u128;
    for i in 0..floating_mem.len() {
        // for (addr, val) in floating_mem.iter().rev() {
        let val = *floating_mem[i].1;
        let volatiles = floating_mem[i..]
            .iter()
            .map(|(addr, _)| {
                addr.iter()
                    .enumerate()
                    .filter_map(|(n, &a)| (a == BitState::Floating).then_some(n))
                    .collect::<HashSet<_>>()
            })
            .collect_vec();
        let mut first = volatiles[0].clone();
        for r in volatiles.iter().skip(1) {
            for e in r {
                first.remove(e);
            }
        }
        // if first.len() != 0 {
            sum += val as u128 * (1 << first.len()) as u128;
        // }
    }
    sum
}

fn main() {
    let raw = read_input();
    let input = parse_input(&raw);
    println!("Part 1: {}", part1(&input));
    // 40413886813 is too low
    println!("Part 2: {}", part2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2020::*;
    use paste::paste;
    use test::black_box;

    const TEST_INPUT: &str = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0";

    const INPUT_PART_2: &str = "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1";

    #[test]
    fn test_part2() {
        let parsed = parse_input(INPUT_PART_2);
        assert_eq!(part2(&parsed), 208);
    }

    test!(part1() == 165);
    bench!(part1() == 18630548206046);
    //bench!(part2() == 0);
    bench_input!(len == 577);
}
