#![feature(derive_default_enum)]
#![feature(test)]
extern crate test;
use aoc2021::{
    common::*,
    grid::{get_boundaries, Boundaries, Grid, HashGrid, Position2D},
};
use itertools::Itertools;
use std::fmt;

#[derive(Debug, PartialEq, Default, Clone, Copy)]
enum Pixel {
    Bright,
    #[default]
    Dark,
}

#[rustfmt::skip]
impl fmt::Display for Pixel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self { Pixel::Bright => '#', Pixel::Dark => '.' })
    }
}

impl From<u8> for Pixel {
    fn from(b: u8) -> Self {
        match b {
            b'#' => Pixel::Bright,
            b'.' => Pixel::Dark,
            _ => unreachable!(),
        }
    }
}

const DAY: usize = 20;
type Parsed = ([Pixel; 512], HashGrid<Pixel, 2>);

fn parse_input(raw: &str) -> Parsed {
    let (enhance, image) = raw.split_once("\n\n").unwrap();
    // type inference on these methods is absolute magic
    (enhance.bytes().map_into().collect_vec().try_into().unwrap(), HashGrid::from_bytes_2d(image, Pixel::from))
}

fn neighbors_plus_self(p: Position2D) -> Vec<Position2D> {
    (-1..=1)
        .rev() // my grid has (0,0) at top left which doesn’t seem to match the task
        .flat_map(|x| (-1..=1).rev().map(move |y| Position2D::from([y, x])))
        .map(|offset| p + offset)
        .collect()
}

fn step(grid: HashGrid<Pixel, 2>, lookup: &[Pixel; 512], iteration: usize) -> HashGrid<Pixel, 2> {
    let Boundaries { x_min, x_max, y_min, y_max } = get_boundaries(&grid.fields.keys().collect_vec());
    (x_min - 1..=x_max + 1)
        .flat_map(|x| (y_min - 1..=y_max + 1).map(move |y| Position2D::from([x, y])))
        .map(|p| (p, lookup[lookup_index(p, &grid, iteration)]))
        .collect()
}

const OUTSIDE: [Pixel; 2] = [Pixel::Dark, Pixel::Bright];

fn lookup_index(p: Position2D, grid: &HashGrid<Pixel, 2>, iteration: usize) -> usize {
    let idx = neighbors_plus_self(p)
        .into_iter()
        .rev()
        .map(|p| grid.get(&p).unwrap_or(&OUTSIDE[iteration & 1]) == &Pixel::Bright)
        .fold(0, |acc, n| (acc << 1) | n as usize);
    idx
}

fn step_times(grid: &HashGrid<Pixel, 2>, lookup: &[Pixel; 512], iterations: usize) -> usize {
    let mut grid = grid.to_owned();
    for i in 0..iterations {
        grid = step(grid, lookup, i);
    }
    grid.fields.values().filter(|&&p| p == Pixel::Bright).count()
}

fn part1((lookup, grid): &Parsed) -> usize {
    step_times(grid, lookup, 2)
}

fn part2((lookup, grid): &Parsed) -> usize {
    step_times(grid, lookup, 50)
}

fn main() {
    let input = parse_input(&read_file(DAY));
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2021::*;

    const TEST_INPUT: &str = "#.#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#...

#..#.
#....
##..#
..#..
..###";

    #[test]
    fn lookup_index_test() {
        let grid = HashGrid::from_bytes_2d("...\n#..\n.#.", Pixel::from);
        let p = Position2D::from([1, 1]);
        let idx = lookup_index(p, &grid, 0);
        assert_eq!(idx, 34);
    }

    test!(part1() == 24);
    bench!(part1() == 5503);
    bench!(part2() == 19156);
    bench_input!(|(_, g): &Parsed| g.len() => 10_000);
}
