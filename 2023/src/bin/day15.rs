#![feature(test, type_alias_impl_trait)]
extern crate test;
use aoc2023::{boilerplate, common::*};

const DAY: usize = 15;
type I = usize;
type Parsed<'a> = impl Iterator<Item = &'a str> + Clone;

fn parse_input(raw: &str) -> Parsed {
    raw.trim_end_matches('\n').split(',')
}

fn hash(s: &[u8]) -> I {
    s.iter().fold(0, |acc, &b| ((acc + b as I) * 17) & 255)
}

fn part1(parsed: &Parsed) -> I {
    parsed.clone().map(|s| hash(s.as_bytes())).sum()
}

fn part2(parsed: &Parsed) -> I {
    let mut boxes: Vec<Vec<(&[u8], u8)>> = vec![vec![]; 256];
    for s in parsed.clone() {
        match s.as_bytes() {
            [label @ .., b'-'] => {
                let hash = hash(label);
                if let Some(p) = boxes[hash].iter().position(|(l, _)| l == &label) {
                    boxes[hash].remove(p);
                }
            }
            [label @ .., b'=', focal_strength] => {
                let hash = hash(label);
                match boxes[hash].iter().position(|(l, _)| l == &label) {
                    Some(p) => boxes[hash][p] = (&label, focal_strength - b'0'),
                    None => boxes[hash].push((&label, focal_strength - b'0')),
                }
            }
            _ => unreachable!(),
        }
    }
    boxes.into_iter().zip(1..).flat_map(|(b, i)| b.into_iter().zip(1..).map(move |((_, f), j)| i * j * f as I)).sum()
}

boilerplate! {
    TEST_INPUT == "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
    for tests: {
        part1: { TEST_INPUT => 1320 },
        part2: { TEST_INPUT => 145 },
    },
    bench1 == 510273,
    bench2 == 212449,
    bench_parse: |i: &Parsed| i.clone().count() => 4000,
}
