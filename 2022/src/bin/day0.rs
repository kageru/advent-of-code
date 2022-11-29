#![feature(test)]
extern crate test;
use aoc2022::{boilerplate, common::*};

const DAY: usize = 0;
type Parsed = Vec<usize>;

boilerplate! {
    TEST_INPUT == "1721
979
366
299
675
1456",
    tests: {
        part1: {
            TEST_INPUT => 514579,
            "1234\n786" => 969924,
        },
        part2: { TEST_INPUT => 241861950 },
    },
    bench1 == 731731,
    bench2 == 116115990,
    parse: Vec::len => 200,
}

/// Naive solution for a previous day 1,
/// just to test my test setup for this year.

fn parse_input(raw: &str) -> Parsed {
    parse_nums(raw)
}

fn part1(parsed: &Parsed) -> usize {
    for x in parsed {
        for y in parsed {
            if x + y == 2020 {
                return x * y;
            }
        }
    }
    unreachable!()
}

fn part2(parsed: &Parsed) -> usize {
    for x in parsed {
        for y in parsed {
            for z in parsed {
                if x + y + z == 2020 {
                    return x * y * z;
                }
            }
        }
    }
    unreachable!()
}
