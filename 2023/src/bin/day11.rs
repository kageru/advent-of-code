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
    let max = raw.lines().next().unwrap().len();
    let mut xs = vec![true; max];
    let mut ys = vec![true; max];
    for &(x, y) in galaxies.iter() {
        xs[x] = false;
        ys[y] = false;
    }
    (sliding_count(xs), sliding_count(ys), galaxies)
}

fn sliding_count(bools: Vec<bool>) -> Vec<I> {
    bools
        .into_iter()
        .scan(0, |prev, b| {
            *prev += b as I;
            Some(*prev)
        })
        .collect()
}

fn solve<const FACTOR: usize>((x_offset, y_offset, galaxies): &Parsed) -> usize {
    galaxies
        .iter()
        .map(|&(x, y)| (x + x_offset[x] * FACTOR, y + y_offset[y] * FACTOR))
        .tuple_combinations()
        .map(|((x1, y1), (x2, y2))| x1.abs_diff(x2) + y1.abs_diff(y2))
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
    bench_parse: |(a, b, c): &Parsed| (a.len(), b.len(), c.len()) => (140, 140, 432),
}
