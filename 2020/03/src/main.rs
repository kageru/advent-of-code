#![feature(iter_map_while)]
#![feature(test)]
extern crate test;
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

impl From<char> for Tile {
    fn from(c: char) -> Self {
        match c {
            '.' => Tile::Free,
            '#' => Tile::Tree,
            _ => unreachable!(),
        }
    }
}

fn read_input() -> String {
    std::fs::read_to_string("input").unwrap()
}

fn parse_input(raw: &str) -> Forest {
    raw.lines().map(|l| l.chars().map_into().collect()).collect()
}

fn count_all_paths(forest: &Forest) -> usize {
    STEP_RIGHT
        .iter()
        .zip(STEP_DOWN.iter())
        .map(|(&r, &d)| count_trees(forest, r, d))
        .product()
}

fn count_trees(forest: &Forest, step_right: usize, step_down: usize) -> usize {
    iter::successors(Some((0, 0)), |(y, x)| Some((y + step_down, (x + step_right) % forest[0].len())))
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
    use test::{self, black_box};

    const INPUT: &str = "..##.......
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
            x = (x + step_right) % width;
        }
        return trees;
    }

    #[test]
    fn part1_test_functional() {
        let forest = parse_input(INPUT);
        assert_eq!(count_trees(&forest, STEP_RIGHT[1], STEP_DOWN[1]), 7);
    }

    #[test]
    fn part1_test_imperative() {
        let forest = parse_input(INPUT);
        assert_eq!(count_trees_imperative(&forest, STEP_RIGHT[1], STEP_DOWN[1]), 7);
    }

    #[test]
    fn part2_test() {
        let forest = parse_input(INPUT);
        assert_eq!(count_all_paths(&forest), 336);
    }

    #[bench]
    fn bench_input_parsing(b: &mut test::Bencher) {
        let raw = read_input();
        b.iter(|| {
            let forest = parse_input(black_box(&raw));
            assert_eq!(forest.len(), 323);
        });
    }

    #[bench]
    fn bench_part_1_functional(b: &mut test::Bencher) {
        let forest = parse_input(&read_input());
        b.iter(|| {
            assert_eq!(count_trees(&forest, STEP_RIGHT[1], STEP_DOWN[1]), 187);
        })
    }

    #[bench]
    fn bench_part_1_imperative(b: &mut test::Bencher) {
        let forest = parse_input(&read_input());
        b.iter(|| {
            assert_eq!(count_trees_imperative(&forest, STEP_RIGHT[1], STEP_DOWN[1]), 187);
        })
    }

    #[bench]
    fn bench_part_2(b: &mut test::Bencher) {
        let forest = parse_input(&read_input());
        b.iter(|| {
            let p2 = count_all_paths(&forest);
        })
    }
}
