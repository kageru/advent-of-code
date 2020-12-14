#![feature(test, str_split_once, destructuring_assignment, bool_to_option)]
extern crate test;
use std::collections::HashMap;

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

fn states_to_number(bits: &Vec<BitState>) -> usize {
    bits.iter()
        .rev()
        .enumerate()
        .filter(|(_, &b)| b == BitState::One)
        .fold(0, |acc, (n, _)| acc | 1 << n)
}

fn get_variations(input: Vec<BitState>) -> Vec<usize> {
    match input.iter().position(|&b| b == BitState::Floating) {
        Some(i) => {
            let mut ret = vec![];
            for &new in &[BitState::One, BitState::Zero] {
                let mut changed = input.clone();
                changed[i] = new;
                ret.append(&mut get_variations(changed));
            }
            ret
        }
        None => vec![states_to_number(&input)],
    }
}

fn part2<'a>(parsed: &Parsed<'a>) -> usize {
    let mut mask = vec![];
    parsed
        .iter()
        .map(|cmd| match cmd {
            Command::BitMask(bm) => {
                mask = calc_bitmask_p2(bm);
                (vec![], 0)
            }
            Command::MemSet { addr, val } => {
                let masked = format!("{:036b}", addr)
                    .bytes()
                    .zip(mask.iter())
                    .map(|b| match b {
                        (_, BitState::Floating) => BitState::Floating,
                        (_, BitState::One) => BitState::One,
                        (n, BitState::Zero) => {
                            if n == b'0' {
                                BitState::Zero
                            } else {
                                BitState::One
                            }
                        }
                    })
                    .collect();
                (get_variations(masked), *val)
            }
        })
        .flat_map(|(addrs, val)| addrs.into_iter().map(move |a| (a, val)))
        .collect::<HashMap<_, _>>()
        .values()
        .sum()
}

fn main() {
    let raw = read_input();
    let input = parse_input(&raw);
    println!("Part 1: {}", part1(&input));
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

    #[test]
    fn test_states_to_number() {
        let states = vec![BitState::One, BitState::Zero, BitState::One, BitState::Zero];
        assert_eq!(states_to_number(&states), 10);
    }

    test!(part1() == 165);
    bench!(part1() == 18630548206046);
    bench!(part2() == 4254673508445);
    bench_input!(len == 577);
}
