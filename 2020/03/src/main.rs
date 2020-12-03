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

mod tests {
    use super::*;
    use test;

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

    #[test]
    fn part1_test() {
        let forest = parse_input(INPUT);
        assert_eq!(count_trees(&forest, STEP_RIGHT[1], STEP_DOWN[1]), 7);
    }

    #[test]
    fn part2_test() {
        let forest = parse_input(INPUT);
        assert_eq!(count_all_paths(&forest), 336);
    }
}
