#![feature(test)]
extern crate test;

use std::time::Duration;

use aoc2021::{
    common::*,
    grid::{Grid, PositionND},
};

const DAY: usize = 11;
const ROUNDS: usize = 100;
type Parsed = Grid<u8, 2>;

fn parse_input(raw: &str) -> Parsed {
    Grid::<u8, 2>::from_bytes_2d(raw, |b: u8| b - b'0')
}

fn part1(parsed: &Parsed) -> usize {
    let mut grid = parsed.to_owned();
    let mut flashed = Vec::new();
    let mut flashes = 0;
    for _ in 0..ROUNDS {
        for (p, energy) in grid.fields.iter_mut() {
            *energy += 1;
            if energy == &10 {
                flashed.push(*p);
            }
        }
        for p in flashed.clone() {
            flash(&mut grid, p, &mut flashed);
        }
        flashes += flashed.len();
        for p in flashed.drain(..) {
            *grid.fields.get_mut(&p).unwrap() = 0;
        }
    }
    flashes
}

fn flash(grid: &mut Parsed, position: PositionND<2>, flashed: &mut Vec<PositionND<2>>) {
    for n in position.neighbors() {
        if let Some(p) = grid.fields.get_mut(&n) {
            *p += 1;
            if p == &10 {
                flashed.push(n);
                flash(grid, n, flashed);
            }
        }
    }
}

fn part2(parsed: &Parsed) -> usize {
    let mut grid = parsed.to_owned();
    let mut flashed = Vec::new();
    for i in 1.. {
        for (p, energy) in grid.fields.iter_mut() {
            *energy += 1;
            if energy == &10 {
                flashed.push(*p);
            }
        }
        for p in flashed.clone() {
            flash(&mut grid, p, &mut flashed);
        }
        if flashed.len() == parsed.len() {
            return i;
        }
        for p in flashed.drain(..) {
            *grid.fields.get_mut(&p).unwrap() = 0;
        }
    }
    unreachable!()
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

    const TEST_INPUT: &str = "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526";

    test!(part1() == 1656);
    test!(part2() == 195);
    bench!(part1() == 1741);
    bench!(part2() == 440);
    bench_input!(Grid::len => 100);
}
