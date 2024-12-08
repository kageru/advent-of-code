#![feature(test, try_blocks)]
extern crate test;
use aoc2024::{
    boilerplate,
    common::*,
    grid::{Grid, VecGrid},
    position::Pos,
};
use itertools::Itertools as _;

const DAY: usize = 08;
type P = Pos<usize, 2>;
const FREQUENCIES: usize = b'z' as usize + 1;
type Parsed = (VecGrid<u8>, [Vec<P>; FREQUENCIES]);

fn parse_input(raw: &str) -> Parsed {
    let grid: VecGrid<u8> = VecGrid::from_bytes_2d(raw, |b| b);
    let mut antennas = [const { Vec::new() }; FREQUENCIES];
    for p in grid.indices() {
        antennas[grid[p] as usize].push(p);
    }
    antennas[b'.' as usize].clear();
    (grid, antennas)
}

fn usize_if_positive(n: i64) -> Option<usize> {
    (n >= 0).then_some(n as usize)
}

fn part1((grid, antennas): &Parsed) -> usize {
    antennas
        .iter()
        .filter(|ps| ps.len() > 1)
        .flat_map(|ps| {
            ps.iter().tuple_combinations().flat_map::<[Option<P>; 2], _>(|(&Pos([x1, y1]), &Pos([x2, y2]))| {
                let (x1, x2, y1, y2) = (x1 as i64, x2 as i64, y1 as i64, y2 as i64);
                let xdiff = x1 - x2;
                let ydiff = y1 - y2;
                [
                    try { Pos([usize_if_positive(x1 + xdiff)?, usize_if_positive(y1 + ydiff)?]) },
                    try { Pos([usize_if_positive(x1 - 2 * xdiff)?, usize_if_positive(y1 - 2 * ydiff)?]) },
                ]
            })
        })
        .flatten()
        .filter(|p| grid.get(p).is_some())
        .sorted()
        .dedup()
        .count()
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"
    for tests: {
        part1: { TEST_INPUT => 14 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 413,
    bench2 == 0,
    bench_parse: |(grid, _): &Parsed| grid.len() => 50,
}
