#![feature(test)]
extern crate test;
use aoc2024::{
    boilerplate,
    common::*,
    grid::{Grid, VecGrid},
    position::Pos,
};
use itertools::Itertools;
use pathfinding::prelude::bfs;

const DAY: usize = 20;
type P = Pos<usize, 2>;
type Parsed = VecGrid<u8>;

fn parse_input(raw: &str) -> Parsed {
    VecGrid::transmute_from_lines(raw)
}

fn find_shortest(grid: &Parsed, start: &P) -> Vec<P> {
    bfs(start, |p| p.manhattan_neighbors().into_iter().filter(|&p| grid[p] != b'#'), |&p| grid[p] == b'E').unwrap()
}

fn solve(grid: &Parsed, min_saved: usize, max_distance: usize) -> usize {
    let start = grid.indices().find(|&p| grid[p] == b'S').expect("No start point");
    find_shortest(grid, &start)
        .into_iter()
        .enumerate()
        .tuple_combinations()
        .filter(|((dist, pos), (dist2, pos2))| {
            let manhattan = pos.manhattan_distance(pos2);
            manhattan <= max_distance && dist.abs_diff(*dist2) - manhattan >= min_saved
        })
        .count()
}

fn part1(grid: &Parsed, min_saved: usize) -> usize {
    solve(grid, min_saved, 2)
}

fn part2(grid: &Parsed, min_saved: usize) -> usize {
    solve(grid, min_saved, 20)
}

boilerplate! {
    TEST_INPUT == "\
###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############"
    for tests: {
        part1: { TEST_INPUT, 1 => 44 },
        part2: { TEST_INPUT, 50 => 285 },
    },
    bench1(100) == 1409,
    bench2(100) == 1012821,
    bench_parse: Grid::len => 141,
}
