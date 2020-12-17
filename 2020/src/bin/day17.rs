#![feature(test)]
extern crate test;
use aoc2020::{
    common::*, grid::{cell::Cell, *}
};

fn read_input() -> String {
    read_file(17)
}

fn parse_input<P: Position, F: FnMut((usize, usize)) -> P + Copy>(raw: &str, mut pos_gen: F) -> Grid<P, Cell> {
    raw.lines()
        .enumerate()
        .flat_map(move |(y, l)| l.bytes().enumerate().map(move |(x, b)| (pos_gen((x, y)), b.into())))
        .filter(|(_, c)| c == &Cell::Alive)
        .collect()
}

fn count_live_neighbors<P: Position>(p: &P, grid: &Grid<P, Cell>) -> usize {
    p.neighbors().iter().filter(|&n| grid.get(n) == Cell::Alive).count()
}

fn make_step<P: Position>(input: Grid<P, Cell>) -> Grid<P, Cell> {
    let readonly = input.clone();
    input
        .fields
        .keys()
        .flat_map(|p| p.neighbors())
        .map(|pos| {
            let cell = readonly.get(&pos);
            let new = match (&cell, count_live_neighbors(&pos, &readonly)) {
                (Cell::Alive, 2..=3) => Cell::Alive,
                (Cell::Dead, 3) => Cell::Alive,
                _ => Cell::Dead,
            };
            (pos, new)
        })
        .filter(|(_, c)| c == &Cell::Alive)
        .collect()
}

fn solve<P: Position>(parsed: &Grid<P, Cell>, steps: usize) -> usize {
    let mut clone = parsed.clone();
    for _ in 0..steps {
        clone = make_step(clone);
    }
    clone.fields.into_iter().filter(|(_, c)| c == &Cell::Alive).count()
}

fn main() {
    let raw = read_input();
    let input = parse_input(&raw, |(x, y)| Position3D::from((x, y, 0)));
    println!("Part 1: {}", solve(&input, 6));
    let input = parse_input(&raw, |(x, y)| Position4D::from((x, y, 0, 0)));
    println!("Part 2: {}", solve(&input, 6));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2020::*;
    use test::black_box;

    const TEST_INPUT: &str = ".#.
..#
###";

    #[test]
    fn test_3d() {
        let input = parse_input(TEST_INPUT, |(x, y)| Position3D::from((x, y, 0)));
        assert_eq!(solve(&input, 6), 112);
    }

    #[test]
    fn test_4d() {
        let input = parse_input(TEST_INPUT, |(x, y)| Position4D::from((x, y, 0, 0)));
        assert_eq!(solve(&input, 6), 848);
    }

    #[bench]
    fn bench_3d_parse(b: &mut test::Bencher) {
        let raw = read_input();
        b.iter(|| assert_eq!(parse_input(black_box(&raw), |(x, y)| Position3D::from((x, y, 0))).fields.len(), 43));
    }

    #[bench]
    #[rustfmt::skip]
    fn bench_4d_parse(b: &mut test::Bencher) {
        let raw = read_input();
        b.iter(|| assert_eq!(parse_input(black_box(&raw), |(x, y)| Position4D::from((x, y, 0, 0))).fields.len(), 43));
    }

    #[bench]
    fn bench_3d(b: &mut test::Bencher) {
        let input = parse_input(&read_input(), |(x, y)| Position3D::from((x, y, 0)));
        b.iter(|| assert_eq!(solve(&input, 6), 348));
    }

    #[bench]
    fn bench_4d(b: &mut test::Bencher) {
        let input = parse_input(&read_input(), |(x, y)| Position4D::from((x, y, 0, 0)));
        b.iter(|| assert_eq!(solve(&input, 6), 2236));
    }
}
