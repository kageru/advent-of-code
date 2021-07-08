#![allow(incomplete_features)]
#![feature(test, const_generics, const_evaluatable_checked)]
extern crate test;
use aoc2020::{
    common::*, grid::{self, cell::Cell, *}
};
use itertools::Itertools;

fn read_input() -> String {
    read_file(17)
}

fn parse_input<const DIMS: usize>(raw: &str) -> Grid<DIMS, Cell> {
    raw.lines()
        .enumerate()
        .flat_map(move |(y, l)| {
            l.bytes()
                .enumerate()
                .map(move |(x, b)| (PositionND::<DIMS>::from_padded(&[x as i64, y as i64]), b.into()))
        })
        .filter(|(_, c)| c == &Cell::Alive)
        .collect()
}

fn count_live_neighbors<const D: usize>(p: &PositionND<D>, grid: &Grid<D, Cell>) -> usize
where [(); grid::num_neighbors(D)]: Sized {
    IntoIterator::into_iter(p.neighbors())
        .filter(|n| grid.get(n) == Cell::Alive)
        .count()
}

fn make_step<const D: usize>(input: Grid<D, Cell>) -> Grid<D, Cell>
where [(); grid::num_neighbors(D)]: Sized {
    let readonly = input.clone();
    input
        .fields
        .keys()
        .flat_map(|p| p.neighbors())
        .unique()
        .map(|pos| (pos, next_state(&pos, &readonly)))
        .filter(|(_, c)| c == &Cell::Alive)
        .collect()
}

fn next_state<const D: usize>(pos: &PositionND<D>, grid: &Grid<D, Cell>) -> Cell
where [(); grid::num_neighbors(D)]: Sized {
    let cell = grid.get(pos);
    match (&cell, count_live_neighbors::<D>(pos, grid)) {
        (Cell::Alive, 2..=3) => Cell::Alive,
        (Cell::Dead, 3) => Cell::Alive,
        _ => Cell::Dead,
    }
}

fn solve<const D: usize>(parsed: &Grid<D, Cell>, steps: usize) -> usize
where [(); grid::num_neighbors(D)]: Sized {
    let mut clone = parsed.clone();
    for _ in 0..steps {
        clone = make_step(clone);
    }
    clone.fields.into_iter().filter(|(_, c)| c == &Cell::Alive).count()
}

fn main() {
    let raw = read_input();
    let input = parse_input::<3>(&raw);
    println!("Part 1: {}", solve(&input, 6));
    let input = parse_input::<4>(&raw);
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
    fn test_make_step() {
        let input = parse_input::<3>(TEST_INPUT);
        let changed = make_step(input.clone());
        let expected = Grid {
            fields: IntoIterator::into_iter([
                (PositionND { points: [1, 3, 0] }, Cell::Alive),
                (PositionND { points: [0, 1, 0] }, Cell::Alive),
                (PositionND { points: [2, 2, 0] }, Cell::Alive),
                (PositionND { points: [2, 2, 1] }, Cell::Alive),
                (PositionND { points: [0, 1, 1] }, Cell::Alive),
                (PositionND { points: [2, 1, 0] }, Cell::Alive),
                (PositionND { points: [1, 3, -1] }, Cell::Alive),
                (PositionND { points: [0, 1, -1] }, Cell::Alive),
                (PositionND { points: [1, 2, 0] }, Cell::Alive),
                (PositionND { points: [1, 3, 1] }, Cell::Alive),
                (PositionND { points: [2, 2, -1] }, Cell::Alive),
            ])
            .collect(),
        };
        assert_eq!(changed, expected);
    }

    #[test]
    fn test_count_live_neighbors() {
        let input = parse_input::<2>(TEST_INPUT);
        let one_one = PositionND { points: [1, 1] };
        let live = count_live_neighbors(&one_one, &input);
        assert_eq!(live, 5);
    }

    #[test]
    fn test_next_state() {
        let input = parse_input::<2>(TEST_INPUT);
        let one_one = PositionND { points: [1, 1] };
        assert_eq!(next_state(&one_one, &input), Cell::Dead);
        let one_three = PositionND { points: [1, 3] };
        assert_eq!(next_state(&one_three, &input), Cell::Alive);
    }

    #[test]
    fn test_3d() {
        let input = parse_input::<3>(TEST_INPUT);
        assert_eq!(solve(&input, 1), 11);
        assert_eq!(solve(&input, 2), 21);
        assert_eq!(solve(&input, 6), 112);
    }

    #[test]
    fn test_4d() {
        let input = parse_input::<4>(TEST_INPUT);
        assert_eq!(solve(&input, 6), 848);
    }

    #[bench]
    fn bench_3d_parse(b: &mut test::Bencher) {
        let raw = read_input();
        b.iter(|| assert_eq!(parse_input::<3>(black_box(&raw)).fields.len(), 43));
    }

    #[bench]
    #[rustfmt::skip]
    fn bench_4d_parse(b: &mut test::Bencher) {
        let raw = read_input();
        b.iter(|| assert_eq!(parse_input::<4>(black_box(&raw)).fields.len(), 43));
    }

    #[bench]
    fn bench_3d(b: &mut test::Bencher) {
        let input = parse_input::<3>(&read_input());
        b.iter(|| assert_eq!(solve(&input, 6), 348));
    }

    #[bench]
    fn bench_4d(b: &mut test::Bencher) {
        let input = parse_input::<4>(&read_input());
        b.iter(|| assert_eq!(solve(&input, 6), 2236));
    }
}
