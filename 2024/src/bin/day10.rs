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
    for p in pos.manhattan_neighbors_checked().into_iter().filter(|p| map.get(p).is_some_and(|&height| height == here + 1)) {
        valid_trails(map, p, heads)
    }
}

fn solve<const UNIQUE: bool>(map: &Parsed) -> usize {
    map.indices()
        .filter(|&p| map[p] == 0)
        .map(|p| {
            let mut heads = Vec::new();
            valid_trails(map, p, &mut heads);
            if UNIQUE {
                heads.sort();
                heads.dedup();
            }
            heads.len()
        })
        .sum()
}

fn part1(map: &Parsed) -> usize {
    solve::<true>(map)
}

fn part2(map: &Parsed) -> usize {
    solve::<false>(map)
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
