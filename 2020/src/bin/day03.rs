#![feature(iter_map_while)]
#![feature(test)]
extern crate test;
use aoc2020::common::*;
use itertools::Itertools;
use std::iter;

#[derive(Debug, PartialEq, Copy, Clone)]
enum Tile {
    Free,
    Tree,
}

type Forest = Vec<Vec<Tile>>;

const STEP_RIGHT: [usize; 5] = [1, 3, 5, 7, 1];
const STEP_DOWN: [usize; 5] = [1, 1, 1, 1, 2];
const TREE: u8 = b'#';
const FREE: u8 = b'.';

impl From<u8> for Tile {
    #[inline]
    fn from(b: u8) -> Self {
        match b {
            FREE => Tile::Free,
            TREE => Tile::Tree,
            _ => unreachable!(),
        }
    }
}

fn read_input() -> String {
    read_file(3)
}

fn parse_input(raw: &str) -> Forest {
    raw.lines().map(|l| l.bytes().map_into().collect()).collect()
}

fn count_all_paths(forest: &Forest) -> usize {
    STEP_RIGHT
        .iter()
        .zip(STEP_DOWN.iter())
        .map(|(&r, &d)| count_trees(forest, r, d))
        .product()
}

fn count_trees(forest: &Forest, step_right: usize, step_down: usize) -> usize {
    iter::successors(Some((0, 0)), |(y, x)| {
        Some((
            y + step_down,
            Some(x + step_right)
                .filter(|&it| it < forest[0].len())
                .unwrap_or_else(|| (x + step_right) - forest[0].len()),
        ))
    })
    .map_while(|(y, x)| forest.get(y).map(|r| r[x]))
    .filter(|&t| t == Tile::Tree)
    .count()
}

fn main() {
    let forest = parse_input(&read_input());
    let p1 = count_trees(&forest, STEP_RIGHT[1], STEP_DOWN[1]);
    println!("Part 1: {}", p1);
    let p2 = count_all_paths(&forest);
    println!("Part 2: {}", p2);
}

#[allow(unused)]
mod tests {
    use super::*;
    use aoc2020::*;
    use paste::paste;
    use test::{self, black_box};

    const TEST_INPUT: &str = "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#";

    fn count_trees_imperative(forest: &Forest, step_right: usize, step_down: usize) -> usize {
        let mut x = 0;
        let mut y = 0;
        let mut trees = 0;
        let width = forest[0].len();
        while y < forest.len() {
            trees += (forest[y][x] == Tile::Tree) as usize;
            y += step_down;
            x += step_right;
            // branch-free version of if x >= width { x -= width }
            x -= width * ((x >= width) as usize);
        }
        trees
    }


    test!(count_trees(STEP_RIGHT[1], STEP_DOWN[1]) == 7);
    test!(count_trees_imperative(STEP_RIGHT[1], STEP_DOWN[1]) == 7);
    test!(count_all_paths() == 336);

    bench!(count_all_paths() == 4723283400);
    bench!(count_trees(STEP_RIGHT[1], STEP_DOWN[1]) == 187);
    bench!(count_trees_imperative(STEP_RIGHT[1], STEP_DOWN[1]) == 187);
    bench_input!(len == 323);
}
