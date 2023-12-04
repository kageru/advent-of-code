#![feature(test, try_blocks)]
extern crate test;
use aoc2023::{boilerplate, common::*};

const DAY: usize = 4;
type I = u32;
type Parsed = Vec<(Vec<I>, Vec<I>)>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines()
        .filter_map(|l| {
            let (w, m) = l.after(": ").split_once(" | ")?;
            Some((w.split_whitespace().map(parse_num).collect(), m.split_whitespace().map(parse_num).collect()))
        })
        .collect()
}

fn winning_numbers((winning, mine): &(Vec<I>, Vec<I>)) -> usize {
    mine.iter().filter(|n| winning.contains(n)).count()
}

const BASE: I = 2;
fn score(n: usize) -> Option<I> {
    Some(BASE.pow(n.checked_sub(1)? as u32))
}

fn part1(parsed: &Parsed) -> I {
    parsed.iter().map(winning_numbers).filter_map(score).sum()
}

fn part2(parsed: &Parsed) -> I {
    let mut cards = vec![1; parsed.len()];
    for (i, card) in parsed.iter().enumerate() {
        let _: Option<()> = try {
            for j in 1..=winning_numbers(card) {
                *cards.get_mut(i + j)? += cards[i];
            }
        };
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
