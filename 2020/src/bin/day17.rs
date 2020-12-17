#![feature(test)]
extern crate test;
use aoc2020::{common::*, grid::*};

type Parsed = Grid<Position3D, Cell>;

fn read_input() -> String {
    read_file(17)
}

fn parse_input(raw: &str) -> Parsed {
    // TODO: implement FromIterator for Grid
    Grid {
        fields: raw
            .lines()
            .enumerate()
            .flat_map(move |(y, l)| l.bytes().enumerate().map(move |(x, b)| ((x, y, 0).into(), b.into())))
            .collect(),
    }
}

fn count_live_neighbors(p: &Position3D, grid: &Parsed) -> usize {
    p.neighbors().iter().filter(|&n| grid.get(*n) == Cell::Alive).count()
}

fn make_step(mut input: Parsed) -> Parsed {
    let readonly = input.clone();
    Grid {
        fields: input
            .fields
            .keys()
            .flat_map(|p| p.neighbors())
            .map(|pos| {
                let cell = readonly.get(pos);
                let new = match (&cell, count_live_neighbors(&pos, &readonly)) {
                    (Cell::Alive, 2..=3) => Cell::Alive,
                    (Cell::Dead, 3) => Cell::Alive,
                    _ =>  Cell::Dead,
                };
                (pos, new)
            })
            .collect(),
    }
}

fn part1(parsed: &Parsed) -> usize {
    let mut clone = parsed.clone();
    for _ in 0..6 {
        clone = make_step(clone);
    }
    clone.fields.into_iter().filter(|(_, c)| c == &Cell::Alive).count()
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

fn main() {
    let input = parse_input(&read_input());
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2020::*;
    use paste::paste;
    use test::black_box;

    const TEST_INPUT: &str = ".#.
..#
###";

    test!(part1() == 112);
    //test!(part2() == 0);
    bench!(part1() == 348);
    //bench!(part2() == 0);
    // bench_input!(fields == 0);
}
