#![feature(test, try_blocks, hash_drain_filter)]
extern crate test;
use aoc2022::{boilerplate, common::*};
use fnv::FnvHashMap as HashMap;

const DAY: usize = 21;
type Parsed<'a> = HashMap<&'a [u8], Monkey<'a>>;

#[derive(Clone, Copy)]
enum Monkey<'a> {
    Number(isize),
    Add(&'a [u8], &'a [u8]),
    Sub(&'a [u8], &'a [u8]),
    Mul(&'a [u8], &'a [u8]),
    Div(&'a [u8], &'a [u8]),
}

fn parse_input(raw: &str) -> Parsed {
    raw.lines()
        .map(|line| {
            let key = &line.as_bytes()[0..4];
            let value = line[6..].as_bytes();

            if value[0].is_ascii_digit() {
                return (key, Monkey::Number(parse_num(&line[6..])));
            }
            match value[5] {
                b'+' => (key, Monkey::Add(&value[0..4], &value[7..])),
                b'-' => (key, Monkey::Sub(&value[0..4], &value[7..])),
                b'*' => (key, Monkey::Mul(&value[0..4], &value[7..])),
                b'/' => (key, Monkey::Div(&value[0..4], &value[7..])),
                _ => unreachable!(),
            }
        })
        .collect()
}

fn part1(parsed: &Parsed) -> isize {
    let mut missing = parsed.clone();
    let mut resolved = HashMap::<&[u8], isize>::default();
    // parsed.iter().filter_map(|(&k, m)| if let Monkey::Number(n) = m { Some((k, *n)) } else { None }).collect();
    while !missing.is_empty() {
        missing
            .drain_filter(|&k, v| {
                let o: Option<()> = try {
                    resolved.insert(
                        k,
                        match v {
                            Monkey::Number(n) => *n,
                            Monkey::Add(a, b) => resolved.get(a)? + resolved.get(b)?,
                            Monkey::Sub(a, b) => resolved.get(a)? - resolved.get(b)?,
                            Monkey::Mul(a, b) => resolved.get(a)? * resolved.get(b)?,
                            Monkey::Div(a, b) => resolved.get(a)? / resolved.get(b)?,
                        },
                    );
                };
                o.is_some()
            })
            .last();
    }
    let root: &[u8] = b"root";
    resolved[root]
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "\
root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32",
    tests: {
        part1: { TEST_INPUT => 152 },
        part2: { TEST_INPUT => 301 },
    },
    bench1 == 66174565793494,
    bench2 == 0,
    bench_parse: HashMap::len => 1901,
}
