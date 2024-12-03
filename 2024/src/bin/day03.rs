#![feature(test, never_type)]
extern crate test;

use aoc2024::{boilerplate, common::*};
use nom::{
    IResult,
    branch::alt,
    bytes::complete::{tag, take},
    character::complete::digit1,
    combinator::map_res,
    multi::many1,
    sequence::{delimited, preceded, separated_pair},
};

const DAY: usize = 03;
type I = i32;
type Parsed = Vec<(I, I)>;

fn parse_input(raw: &str) -> Parsed {
    parse_command(raw).expect("Failed to parse").1
}

fn parse_command(input: &str) -> IResult<&str, Parsed> {
    many1(alt((
        preceded(
            tag("mul"),
            delimited(tag("("), separated_pair(map_res(digit1, str::parse::<I>), tag(","), map_res(digit1, str::parse::<I>)), tag(")")),
        ),
        map_res(take(1u32), |_| Ok::<_, !>((0, 0))),
    )))(input)
}

fn part1(parsed: &Parsed) -> I {
    parsed.iter().map(|(a, b)| a * b).sum()
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
    for tests: {
        part1: { TEST_INPUT => 161 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 187825547,
    bench2 == 0,
    bench_parse: Vec::len => 11596,
}
