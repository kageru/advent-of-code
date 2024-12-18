#![feature(test, impl_trait_in_fn_trait_return)]
extern crate test;
use aoc2024::{
    boilerplate,
    common::*,
    direction::Direction,
    grid::{Grid, VecGrid},
    position::Pos,
};
use itertools::Itertools;
use pathfinding::prelude::{astar, astar_bag_collect};

const DAY: usize = 16;
type P = Pos<usize, 2>;
type Parsed = (VecGrid<u8>, P);

fn parse_input(raw: &str) -> Parsed {
    let grid = VecGrid::transmute_from_lines(raw);
    let start = grid.indices().find(|&p| grid[p] == b'S').unwrap();
    (grid, start)
}

fn successors(grid: &VecGrid<u8>, pos: P, dir: Direction) -> impl IntoIterator<Item = ((P, Direction), usize)> {
    [(grid[pos + dir] != b'#').then(|| ((pos + dir, dir), 1)), Some(((pos, dir + 1), 1000)), Some(((pos, dir + 3), 1000))]
        .into_iter()
        .flatten()
}

fn part1((grid, start): &Parsed) -> usize {
    astar(&(*start, Direction::Right), |&(pos, dir)| successors(grid, pos, dir), |_| 1, |&(p, _)| grid[p] == b'E').unwrap().1
}

fn part2((grid, start): &Parsed) -> usize {
    astar_bag_collect(&(*start, Direction::Right), |&(pos, dir)| successors(grid, pos, dir), |_| 1, |&(p, _)| grid[p] == b'E')
        .unwrap()
        .0
        .into_iter()
        .flatten()
        .map(|(p, _)| p)
        .unique()
        .count()
}

boilerplate! {
    TEST_INPUT == "\
###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############"
    for tests: {
        part1: { TEST_INPUT => 7036 },
        part2: { TEST_INPUT => 45 },
    },
    bench1 == 79404,
    bench2 == 451,
    bench_parse: |(grid, start): &Parsed| (grid.len(), *start) => (141, Pos([1, 1])),
}
