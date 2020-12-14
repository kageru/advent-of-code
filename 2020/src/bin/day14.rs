#![feature(test, str_split_once, destructuring_assignment)]
extern crate test;
use std::collections::HashMap;

use aoc2020::common::*;

enum Command<'a> {
    BitMask(&'a str),
    MemSet { addr: usize, val: usize },
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

fn calc_bitmasks(b: &str) -> (usize, usize) {
    (calc_bitmask(b, |(_, b)| *b == b'1'), calc_bitmask(b, |(_, b)| *b != b'0'))
}

fn part1<'a>(parsed: &Parsed<'a>) -> usize {
    let (mut ones, mut zeros) = (0, 0);
    let mut mem = HashMap::new();
    for command in parsed {
        match command {
            Command::BitMask(bm) => (ones, zeros) = calc_bitmasks(bm),
            Command::MemSet { addr, val } => {
                mem.insert(addr, (val & zeros) | ones);
            }
        }
    }
    mem.values().sum()
}

fn part2<'a>(parsed: &Parsed<'a>) -> usize {
    unimplemented!()
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

    test!(part1() == 165);
    //test!(part2() == 0);
    bench!(part1() == 18630548206046);
    //bench!(part2() == 0);
    //bench_input!(len == 0);
}
