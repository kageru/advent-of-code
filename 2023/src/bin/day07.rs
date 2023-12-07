#![feature(test)]
extern crate test;

use aoc2023::{boilerplate, common::*};
use itertools::Itertools;

const DAY: usize = 7;
type I = usize;
type Hand = [u8; 5];
type Parsed = Vec<(Hand, I)>;

const CARDS: [u8; 13] = [b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'T', b'J', b'Q', b'K', b'A'];
const CARDS_P2: [u8; 13] = [b'J', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'T', b'Q', b'K', b'A'];

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
    let paired = hand
        .into_iter()
        .map(|c| (c, 1))
        .coalesce(|(c1, n1), (c2, n2)| if c1 == c2 { Ok((c1, n1 + n2)) } else { Err(((c1, n1), (c2, n2))) })
        .map(|(_, n)| n)
        .collect_vec();
    match paired.as_slice() {
        [_] => Quality::AllEqual,
        [4, 1] | [1, 4] => Quality::Quad,
        [3, 2] | [2, 3] => Quality::FullHouse,
        a @ [_, _, _] if a.contains(&3) => Quality::Triple,
        [_, _, _] => Quality::TwoPair,
        [_, _, _, _] => Quality::Pair,
        _ => Quality::None,
    }
}

fn tiebreaker(hand: &Hand, values: &[u8; 13]) -> usize {
    let tiebreak_hand = hand.map(|b| values.iter().position(|&c| c == b).unwrap());
    tiebreak_hand.iter().fold(0, |acc, n| (acc << 4) + n)
}

fn part1(hands: &Parsed) -> usize {
    let mut rated_hands = hands
        .iter()
        .cloned()
        .map(|(hand, points)| {
            let tiebreak = tiebreaker(&hand, &CARDS);
            (rate_hand(hand) as usize + tiebreak, points)
        })
        .collect_vec();
    rated_hands.sort_unstable_by_key(|(v, _)| *v);
    rated_hands.into_iter().zip(1..).map(|((_, points), position)| points * position).sum()
}

fn part2(hands: &Parsed) -> usize {
    let mut rated_hands = hands
        .iter()
        .cloned()
        .map(|(mut hand, points)| {
            let tiebreak = tiebreaker(&hand, &CARDS_P2);
            // Count how many jokers there are and insert nonsense data for them so they can’t produce any pairs.
            let mut jokers = 0;
            for c in hand.iter_mut().filter(|c| c == &&b'J') {
                *c = jokers;
                jokers += 1;
            }
            let mut rating = rate_hand(hand);
            for _ in 0..jokers {
                rating = rating.upgrade();
            }
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
QQQJA 483",
    tests: {
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
        b.iter(|| assert_eq!(tiebreaker(black_box(&[b'3', b'2', b'T', b'3', b'K']), &CARDS), 67611))
    }

    #[bench]
    fn bench_rate_hand(b: &mut test::Bencher) {
        b.iter(|| assert_eq!(rate_hand(black_box([b'3', b'2', b'T', b'3', b'K'])), Quality::Pair))
    }
}
