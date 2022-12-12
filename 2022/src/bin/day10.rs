#![feature(test)]
extern crate test;
use aoc2022::{boilerplate, common::*};
use itertools::Itertools;

const DAY: usize = 10;
type Parsed = Vec<Instruction>;

#[derive(Debug)]
enum Instruction {
    Add(i32),
    Noop,
}

fn parse_input(raw: &str) -> Parsed {
    raw.lines()
        .flat_map(|line| {
            if line == "noop" {
                vec![Instruction::Noop]
            } else {
                vec![Instruction::Noop, Instruction::Add(line[5..].parse().unwrap())]
            }
        })
        .collect()
}

fn part1(parsed: &Parsed) -> i32 {
    const INSPECTION_CYCLES: &[i32] = &[20, 60, 100, 140, 180, 220];
    parsed
        .iter()
        .zip(1..)
        .scan(1, |x, (ins, cycle)| {
            let ret = Some((cycle, *x * cycle));
            if let Instruction::Add(n) = ins {
                *x += n
            }
            ret
        })
        .filter_map(|(c, x)| INSPECTION_CYCLES.contains(&c).then_some(x))
        .sum()
}

fn part2(parsed: &Parsed) -> String {
    parsed
        .iter()
        .zip((0..=39).cycle())
        .scan(1, |x, (ins, cycle)| {
            let ret = if *x - 1 <= cycle && cycle <= *x + 1 { '#' } else { '.' };
            if let Instruction::Add(n) = ins {
                *x += n
            }
            Some(ret)
        })
        .chunks(40)
        .into_iter()
        .map(|c| c.collect::<String>())
        .join("\n")
}

boilerplate! {
    TEST_INPUT == "\
addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop",
    tests: {
        part1: { TEST_INPUT => 13140 },
        part2: { TEST_INPUT => P2_TEST_OUTPUT },
    },
    bench1 == 11780,
    bench2 == "###..####.#..#.#....###...##..#..#..##..
#..#....#.#..#.#....#..#.#..#.#..#.#..#.
#..#...#..#..#.#....###..#..#.#..#.#..#.
###...#...#..#.#....#..#.####.#..#.####.
#....#....#..#.#....#..#.#..#.#..#.#..#.
#....####..##..####.###..#..#..##..#..#.",
    bench_parse: Vec::len => 240,
}

#[cfg(test)]
const P2_TEST_OUTPUT: &str = "\
##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....";
