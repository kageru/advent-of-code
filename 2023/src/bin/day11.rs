#![feature(test)]
extern crate test;
use aoc2023::{boilerplate, common::*};
use itertools::Itertools;

const DAY: usize = 11;
type I = usize;
type Parsed = (Vec<I>, Vec<I>, Vec<(I, I)>);

fn parse_input(raw: &str) -> Parsed {
    let galaxies = raw
        .lines()
        .enumerate()
        .flat_map(|(x, l)| l.bytes().enumerate().filter_map(move |(y, b)| (b == b'#').then_some((x, y))))
        .collect_vec();
    let &xmax = galaxies.iter().map(|(x, _)| x).max().unwrap();
    let &ymax = galaxies.iter().map(|(_, y)| y).max().unwrap();
    let mut xs = vec![true; xmax + 1];
    let mut ys = vec![true; ymax + 1];
    for &(x, y) in galaxies.iter() {
        xs[x] = false;
        ys[y] = false;
    }
    (
        xs.into_iter().enumerate().filter_map(|(x, b)| b.then_some(x)).collect(),
        ys.into_iter().enumerate().filter_map(|(y, b)| b.then_some(y)).collect(),
        galaxies,
    )
}

fn distance<const FACTOR: usize>(x1: &I, x2: &I, empty: &[I]) -> I {
    let smaller_x = x1.min(x2);
    let bigger_x = x1.max(x2);
    empty.iter().filter(|x| (smaller_x..=bigger_x).contains(x)).count() * FACTOR + bigger_x - smaller_x
}

fn solve<const FACTOR: usize>((empty_x, empty_y, galaxies): &Parsed) -> usize {
    galaxies
        .iter()
        .tuple_combinations()
        .map(|((x1, y1), (x2, y2))| distance::<FACTOR>(x1, x2, empty_x) + distance::<FACTOR>(y1, y2, empty_y))
        .sum()
}

fn part1(parsed: &Parsed) -> usize {
    solve::<1>(parsed)
}

fn part2(parsed: &Parsed) -> usize {
    solve::<999_999>(parsed)
}

boilerplate! {
    TEST_INPUT == "\
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....",
    tests: {
        part1: { TEST_INPUT => 374 },
    },
    bench1 == 9623138,
    bench2 == 726820169514,
    bench_parse: |(a, b, c): &Parsed| (a.len(), b.len(), c.len()) => (9, 11, 432),
}
