#![feature(test)]
extern crate test;

use aoc2023::{boilerplate, common::*};
use itertools::Itertools;

const DAY: usize = 7;
type I = usize;
type Hand = [u8; 5];
type Parsed = Vec<(Hand, I)>;

const CARDS: [u8; 14] = [b' ', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'T', b'J', b'Q', b'K', b'A'];

#[derive(Debug, PartialEq)]
enum Quality {
    AllEqual = 1 << 30,
    Quad = 1 << 29,
    FullHouse = 1 << 28,
    Triple = 1 << 27,
    TwoPair = 1 << 26,
    Pair = 1 << 25,
    None = 1 << 24,
}

impl Quality {
    // How adding 1 joker raises the quality of a hand.
    fn upgrade(&self) -> Self {
        match self {
            Quality::AllEqual | Quality::Quad => Quality::AllEqual,
            Quality::Triple | Quality::FullHouse => Quality::Quad,
            Quality::TwoPair => Quality::FullHouse,
            Quality::Pair => Quality::Triple,
            Quality::None => Quality::Pair,
        }
    }
}

fn parse_input(raw: &str) -> Parsed {
    raw.lines()
        .map(|l| l.split_once(' ').unwrap())
        .map(|(hand, points)| (<[_; 5]>::try_from(hand.as_bytes()).unwrap(), parse_num(points)))
        .collect()
}

fn rate_hand(mut hand: Hand) -> Quality {
    hand.sort();
    let equals = hand
        .into_iter()
        .map(|c| (c, 1))
        .coalesce(|(c1, n1), (c2, n2)| if c1 == c2 { Ok((c1, n1 + n2)) } else { Err(((c1, n1), (c2, n2))) })
        .map(|(_, n)| n)
        .collect_vec();
    match equals.as_slice() {
        [_] => Quality::AllEqual,
        [4, 1] | [1, 4] => Quality::Quad,
        [3, 2] | [2, 3] => Quality::FullHouse,
        [3, 1, 1] | [1, 3, 1] | [1, 1, 3] => Quality::Triple,
        [_, _, _] => Quality::TwoPair,
        [_, _, _, _] => Quality::Pair,
        _ => Quality::None,
    }
}

fn tiebreak(hand: &Hand) -> usize {
    hand.iter().map(|b| CARDS.iter().position(|c| c == b).unwrap_or(0)).fold(0, |acc, n| (acc << 4) + n)
}

fn part1(hands: &Parsed) -> usize {
    let mut rated_hands = hands.iter().cloned().map(|(hand, points)| (tiebreak(&hand) + rate_hand(hand) as usize, points)).collect_vec();
    rated_hands.sort_unstable_by_key(|(v, _)| *v);
    rated_hands.into_iter().zip(1..).map(|((_, points), position)| points * position).sum()
}

fn part2(hands: &Parsed) -> usize {
    let mut rated_hands = hands
        .iter()
        .cloned()
        .map(|(mut hand, points)| {
            // Count how many jokers there are and insert nonsense data for them so they can’t produce any pairs.
            let jokers = hand.iter_mut().filter(|c| c == &&b'J').zip(0..).map(|(c, i)| *c = i).count();
            let tiebreak = tiebreak(&hand);
            let rating = (0..jokers).fold(rate_hand(hand), |rating, _| rating.upgrade());
            (rating as usize + tiebreak, points)
        })
        .collect_vec();
    rated_hands.sort_unstable_by_key(|(v, _)| *v);
    rated_hands.into_iter().zip(1..).map(|((_, points), position)| points * position).sum()
}

boilerplate! {
    TEST_INPUT == "\
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"
    for tests: {
        part1: { TEST_INPUT => 6440 },
        part2: { TEST_INPUT => 5905 },
    },
    bench1 == 248812215,
    bench2 == 250057090,
    bench_parse: Vec::len => 1000,
}

#[cfg(test)]
mod bench {
    use test::black_box;

    use super::*;

    #[bench]
    fn bench_tiebreak(b: &mut test::Bencher) {
        b.iter(|| assert_eq!(tiebreak(black_box(&[b'3', b'2', b'T', b'3', b'K'])), 137516))
    }

    #[bench]
    fn bench_rate_hand(b: &mut test::Bencher) {
        b.iter(|| assert_eq!(rate_hand(black_box([b'3', b'2', b'T', b'3', b'K'])), Quality::Pair))
    }
}
