#![feature(test)]
extern crate test;
use aoc2024::{boilerplate, common::*};

const DAY: usize = 4;
type Parsed = Vec<Vec<char>>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines().map(|l| l.chars().collect()).collect()
}

fn is_xmas(mut slice: Vec<&char>) -> bool {
    let forwards = slice == [&'X', &'M', &'A', &'S'];
    slice.reverse();
    let backwards = slice == [&'X', &'M', &'A', &'S'];
    forwards || backwards
}

fn part1(grid: &Parsed) -> usize {
    let mut count = 0;
    for x in 0..grid.len() {
        for y in 0..grid[0].len() {
            // From each position, try
            //
            //    /
            //   /
            //  /
            // X---
            // |\
            // | \
            // |  \
            //
            // while skipping all that are out of bounds
            count += [
                (0..=3).map(|n| grid.get(x + n)?.get(y.checked_sub(n)?)).collect::<Option<Vec<_>>>(),
                (0..=3).map(|n| grid.get(x)?.get(y + n)).collect(),
                (0..=3).map(|n| grid.get(x + n)?.get(y)).collect(),
                (0..=3).map(|n| grid.get(x + n)?.get(y + n)).collect(),
            ]
            .into_iter()
            .flatten()
            // map takes ownership (which we want) while filter doesn’t
            .map(|s| is_xmas(s))
            .filter(|&b| b)
            .count();
        }
    }
    count
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"
    for tests: {
        part1: { TEST_INPUT => 18 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 2547,
    bench2 == 0,
    bench_parse: Vec::len => 140,
}
