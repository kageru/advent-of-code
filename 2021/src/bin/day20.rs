#![feature(derive_default_enum)]
#![feature(test)]
extern crate test;
use std::fmt;

use aoc2021::{
    common::*,
    grid::{draw_ascii, get_boundaries, Boundaries, Grid, HashGrid, Position2D},
};
use itertools::Itertools;

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

fn part1((lookup, grid): &Parsed) -> usize {
    let mut grid = grid.to_owned();
    for _ in 0..2 {
        // println!("{}\n{}\n\n", grid.len(), draw_ascii(&grid.fields));
        grid = step(grid, lookup);
    }
    // println!("{}\n{}\n\n", grid.len(), draw_ascii(&grid.fields));
    grid.fields.values().filter(|&&p| p == Pixel::Bright).count()
}

fn step(grid: HashGrid<Pixel, 2>, lookup: &[Pixel; 512]) -> HashGrid<Pixel, 2> {
    let Boundaries { x_min, x_max, y_min, y_max } = get_boundaries(&grid.fields.keys().collect_vec());
    println!("{x_min}, {x_max}, {y_min}, {y_max}");
    (x_min - 1..=x_max + 1)
        .flat_map(|x| (y_min - 1..=y_max + 1).map(move |y| Position2D::from([x, y])))
        .map(|p| (p, lookup[lookup_index(p, &grid)]))
        .collect()
}

fn lookup_index(p: Position2D, grid: &HashGrid<Pixel, 2>) -> usize {
    let idx =
        neighbors_plus_self(p).into_iter().rev().map(|p| grid.get(&p) == Some(&Pixel::Bright)).fold(0, |acc, n| (acc << 1) | n as usize);
    idx
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
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

    const TEST_INPUT: &str = "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###";

    #[test]
    fn lookup_index_test() {
        let grid = HashGrid::from_bytes_2d("...\n#..\n.#.", Pixel::from);
        let p = Position2D::from([1, 1]);
        let idx = lookup_index(p, &grid);
        assert_eq!(idx, 34);
    }

    test!(part1() == 35);
    test!(part2() == 0);
    // bench!(part1() == 5573); // too high
    bench!(part2() == 0);
    bench_input!(|(_, g): &Parsed| g.len() => 10_000);
}
