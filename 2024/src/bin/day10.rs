#![feature(test)]
extern crate test;
use aoc2024::{
    boilerplate,
    common::*,
    grid::{Grid, VecGrid},
    position::Pos2D,
};

const DAY: usize = 10;
type P = Pos2D<usize>;
type Parsed = VecGrid<u8>;

fn parse_input(raw: &str) -> Parsed {
    VecGrid::from_bytes_2d(raw, |b| b - b'0')
}

fn valid_trails(map: &Parsed, pos: P, heads: &mut Vec<P>) {
    let here = map[pos];
    if here == 9 {
        heads.push(pos);
        return;
    }
    for p in pos.neighbors_no_diagonals_checked().into_iter() {
        match map.get(&p) {
            Some(&height) if height == here + 1 => valid_trails(map, p, heads),
            _ => (),
        }
    }
}

fn part1(map: &Parsed) -> usize {
    map.indices()
        .filter_map(|p| {
            (map[p] == 0).then(|| {
                let mut heads = Vec::new();
                valid_trails(map, p, &mut heads);
                heads.sort();
                heads.dedup();
                heads.len()
            })
        })
        .sum()
}

fn part2(map: &Parsed) -> usize {
    map.indices()
        .filter_map(|p| {
            (map[p] == 0).then(|| {
                let mut heads = Vec::new();
                valid_trails(map, p, &mut heads);
                heads.len()
            })
        })
        .sum()
}

boilerplate! {
    TEST_INPUT == "\
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732"
    for tests: {
        part1: { TEST_INPUT => 36 },
        part2: { TEST_INPUT => 81 },
    },
    bench1 == 709,
    bench2 == 1326,
    bench_parse: Grid::len => 54,
}
