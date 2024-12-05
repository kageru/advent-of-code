#![feature(test)]
extern crate test;
use aoc2024::{boilerplate, common::*};

const DAY: usize = 4;
type Parsed = Vec<Vec<u8>>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines().map(|l| l.bytes().collect()).collect()
}

fn is_xmas(slice: &[u8; 4]) -> bool {
    slice == b"XMAS" || slice == b"SAMX"
}

fn xmas_slice<F: Fn(usize) -> Option<u8>>(f: F) -> [u8; 4] {
    [0usize, 1, 2, 3].map(|n| f(n).unwrap_or(0))
}

fn part1(grid: &Parsed) -> usize {
    (0..grid.len())
        .flat_map(|x| (0..grid[0].len()).map(move |y| (x, y)))
        .filter(|&(x, y)| grid[x][y] == b'X' || grid[x][y] == b'S')
        .map(|(x, y)| {
            // From each position X, try
            //       /
            //      /
            //     /
            //    X---
            //    |\
            //    | \
            //    |  \
            [
                xmas_slice(|n| grid.get(x + n)?.get(y.checked_sub(n)?).copied()),
                xmas_slice(|n| grid.get(x)?.get(y + n).copied()),
                xmas_slice(|n| grid.get(x + n)?.get(y).copied()),
                xmas_slice(|n| grid.get(x + n)?.get(y + n).copied()),
            ]
            .into_iter()
            .filter(is_xmas)
            .count()
        })
        .sum()
}

fn is_x_mas(grid: &Parsed, (x, y): (usize, usize)) -> bool {
    grid[x][y] == b'A' && grid[x - 1][y - 1] + grid[x + 1][y + 1] == b'M' + b'S' && grid[x + 1][y - 1] + grid[x - 1][y + 1] == b'M' + b'S'
}

fn part2(grid: &Parsed) -> usize {
    (1..grid.len() - 1).flat_map(|x| (1..grid[0].len() - 1).map(move |y| (x, y))).filter(|&p| is_x_mas(grid, p)).count()
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
        part2: { TEST_INPUT => 9 },
    },
    bench1 == 2547,
    bench2 == 1939,
    bench_parse: Vec::len => 140,
}
