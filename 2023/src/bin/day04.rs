#![feature(test, try_blocks)]
extern crate test;
use aoc2023::{boilerplate, common::*};

const DAY: usize = 4;
type I = usize;
type Parsed = Vec<I>;

// Parsing with this gives me 50ns for the entire parsing step and then a few hundred ns each for parts 1 and 2
// If only there was a way to use include!() inside a macro.
#[allow(unused)]
macro_rules! parse {
    ($(Card $_:literal: $($winning:literal )* | $($mine:literal )*)*) => {
        [$(([$($winning,)*], [$($mine,)*]),)*]
    };
}

fn parse_input(raw: &str) -> Parsed {
    raw.lines()
        .map(|l| {
            let (w, m) = l.after(": ").split_once(" | ").unwrap();
            let w: Vec<u32> = w.split_whitespace().map(parse_num).collect();
            m.split_whitespace().filter(|n| w.contains(&parse_num(n))).count()
        })
        .collect()
}

fn part1(parsed: &Parsed) -> I {
    parsed.iter().filter_map(|n| Some(2usize.pow(n.checked_sub(1)? as u32))).sum()
}

fn part2(parsed: &Parsed) -> I {
    let mut cards = vec![1; parsed.len()];
    for (i, card) in parsed.iter().enumerate() {
        for j in 1..=*card {
            cards[i + j] += cards[i];
        }
    }
    cards.iter().sum()
}

boilerplate! {
    TEST_INPUT == "\
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11",
    tests: {
        part1: { TEST_INPUT => 13 },
        part2: { TEST_INPUT => 30 },
    },
    bench1 == 20407,
    bench2 == 23806951,
    bench_parse: Vec::len => 192,
}
