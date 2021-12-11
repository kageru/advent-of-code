#![feature(bool_to_option)]
#![feature(test)]
extern crate test;
use aoc2021::{
    common::*,
    grid::{Grid, HashGrid, PositionND},
};

const DAY: usize = 11;
const ROUNDS: usize = 100;
type Parsed = HashGrid<u8, 2>;

fn parse_input(raw: &str) -> Parsed {
    HashGrid::<u8, 2>::from_bytes_2d(raw, |b: u8| b - b'0')
}

fn part1(parsed: &Parsed) -> usize {
    let mut grid = parsed.to_owned();
    (0..ROUNDS).map(|_| make_step(&mut grid)).sum()
}

fn part2(parsed: &Parsed) -> usize {
    let mut grid = parsed.to_owned();
    (0..).position(|_| make_step(&mut grid) == parsed.len()).unwrap() + 1
}

fn make_step(grid: &mut Parsed) -> usize {
    let mut flashed: Vec<_> = grid
        .fields
        .iter_mut()
        .filter_map(|(p, energy)| {
            *energy += 1;
            (*energy == 10).then_some(*p)
        })
        .collect();
    for p in flashed.clone() {
        flash(grid, &p, &mut flashed);
    }
    for p in &flashed {
        *grid.fields.get_mut(p).unwrap() = 0;
    }
    flashed.len()
}

fn flash(grid: &mut Parsed, position: &PositionND<2>, flashed: &mut Vec<PositionND<2>>) {
    for n in position.neighbors() {
        if let Some(p) = grid.fields.get_mut(&n) {
            *p += 1;
            if p == &10 {
                flashed.push(n);
                flash(grid, &n, flashed);
            }
        }
    }
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
    bench_input!(HashGrid::len => 100);
}
