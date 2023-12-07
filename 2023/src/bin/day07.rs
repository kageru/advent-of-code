#![feature(test)]
extern crate test;
use aoc2023::{boilerplate, common::*};
use itertools::Itertools;

const DAY: usize = 7;
type I = usize;
type Parsed = Vec<([I; 5], I, I)>;

const CARDS: [u8; 13] = [b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'T', b'J', b'Q', b'K', b'A'];

fn parse_input(raw: &str) -> Parsed {
    raw.lines()
        .map(|l| l.split_once(' ').unwrap())
        .map(|(hand, points)| {
            (<[_; 5]>::try_from(hand.as_bytes()).unwrap().map(|b| CARDS.iter().position(|&c| c == b).unwrap()), parse_num(points))
        })
        .map(|(hand, points)| (hand, points, hand.iter().fold(0, |acc, n| (acc << 4) + n)))
        .collect()
}

fn part1(hands: &Parsed) -> usize {
    let mut rated_hands = hands
        .iter()
        .cloned()
        .map(|(mut hand, points, tiebreak)| {
            hand.sort();
            let paired = hand
                .into_iter()
                .map(|c| (c, 1))
                .coalesce(|(c1, n1), (c2, n2)| if c1 == c2 { Ok((c1, n1 + n2)) } else { Err(((c1, n1), (c2, n2))) })
                .map(|(_, n)| n)
                .collect_vec();
            let value = match paired.as_slice() {
                [_] => (1 << 30) + tiebreak,
                [4, 1] | [1, 4] => (1 << 29) + tiebreak,
                [3, 2] | [2, 3] => (1 << 28) + tiebreak,
                [3, 1, 1] | [1, 3, 1] | [1, 1, 3] => (1 << 27) + tiebreak,
                // can only be 2 pair
                [_, _, _] => (1 << 26) + tiebreak,
                [_, _, _, _] => (1 << 25) + tiebreak,
                _ => (1 << 24) + tiebreak,
            };
            (value, points)
        })
        .collect_vec();
    rated_hands.sort_unstable_by_key(|(v, _)| *v);
    rated_hands.into_iter().zip(1..).map(|((_, points), position)| points * position).sum()
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "\
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483",
    tests: {
        part1: { TEST_INPUT => 6440 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 248812215,
    bench2 == 0,
    bench_parse: Vec::len => 1000,
}
