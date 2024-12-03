#![feature(test)]
extern crate test;

use aoc2024::{boilerplate, common::*};
use nom::{
    IResult,
    branch::alt,
    bytes::complete::{tag, take},
    character::complete::digit1,
    combinator::{map, map_res},
    multi::many1,
    sequence::{delimited, separated_pair},
};

const DAY: usize = 3;
type I = i32;
type Parsed = Vec<Instruction>;

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
enum Instruction {
    Mul((I, I)),
    Noop,
    Do,
    Dont,
}

fn parse_input(raw: &str) -> Parsed {
    let mut instructions = parse_command(raw).expect("Failed to parse").1;
    instructions.retain(|i| i != &Instruction::Noop);
    instructions
}

fn number(input: &str) -> IResult<&str, I> {
    map_res(digit1, str::parse)(input)
}

fn parse_command(input: &str) -> IResult<&str, Parsed> {
    many1(alt((
        map(delimited(tag("mul("), separated_pair(number, tag(","), number), tag(")")), Instruction::Mul),
        map(tag("do()"), |_| Instruction::Do),
        map(tag("don't()"), |_| Instruction::Dont),
        map(take(1u32), |_| Instruction::Noop),
    )))(input)
}

fn part1(parsed: &Parsed) -> I {
    parsed.iter().map(|i| if let Instruction::Mul((a, b)) = i { a * b } else { 0 }).sum()
}

fn part2(parsed: &Parsed) -> I {
    parsed
        .iter()
        .scan(1, |enabled, &i| {
            if let Instruction::Mul((a, b)) = i {
                Some(*enabled * a * b)
            } else {
                *enabled = (i == Instruction::Do) as I;
                Some(0)
            }
        })
        .sum()
}

boilerplate! {
    TEST_INPUT == "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
    for tests: {
        part1: { TEST_INPUT => 161 },
        part2: { TEST_INPUT => 48 },
    },
    bench1 == 187825547,
    bench2 == 85508223,
    bench_parse: Vec::len => 782,
}
