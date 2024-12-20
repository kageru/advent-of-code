#![feature(test)]
extern crate test;
use aoc2024::{
    boilerplate,
    common::*,
    grid::{Grid, VecGrid},
    position::Pos,
};
use pathfinding::prelude::astar;

const DAY: usize = 20;
type P = Pos<usize, 2>;
type Parsed = VecGrid<u8>;

fn parse_input(raw: &str) -> Parsed {
    VecGrid::transmute_from_lines(raw)
}

fn find_shortest(grid: &Parsed, start: &P, end: &P) -> usize {
    astar(
        start,
        |p| p.manhattan_neighbors().into_iter().filter(|&p| grid[p] != b'#').map(|p| (p, 1)),
        |Pos([x, y])| (x.abs_diff(end[0] + y.abs_diff(end[1]))) / 3,
        |p| p == end,
    )
    .unwrap()
    .1
}

fn part1(grid: &Parsed, min_saved: usize) -> usize {
    let start = grid.indices().find(|&p| grid[p] == b'S').expect("No start point");
    let end = grid.indices().find(|&p| grid[p] == b'E').expect("No start point");
    let shortest = find_shortest(grid, &start, &end);
    let mut grid = grid.clone();
    let ylimit = grid.len() - 1;
    let xlimit = grid.0[0].len() - 1;
    (1..ylimit)
        .flat_map(|y| (1..xlimit).map(move |x| Pos([y, x])))
        .filter(|&p| {
            let t = grid[p];
            if t != b'#' {
                return false;
            }
            grid[p] = b'.';
            let cheated_shortest = find_shortest(&grid, &start, &end);
            grid[p] = t;
            let saved = dbg!(shortest - cheated_shortest);
            saved >= min_saved
        })
        .count()
}

fn part2(grid: &Parsed, min_saved: usize) -> usize {
    unimplemented!()
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
    bench2(100) == 0,
    bench_parse: Grid::len => 0,
}
