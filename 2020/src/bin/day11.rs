#![feature(test)]
extern crate test;
use aoc2020::{common::*, grid::*};
use std::{
    collections::HashMap, fmt::{self, Display, Formatter}
};

#[derive(Debug, PartialEq, Clone, Copy)]
enum Tile {
    Floor,
    Empty,
    Occupied,
}

impl Display for Tile {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Tile::Floor => ".",
            Tile::Empty => "L",
            Tile::Occupied => "#",
        })
    }
}

impl From<char> for Tile {
    fn from(c: char) -> Self {
        match c {
            '.' => Tile::Floor,
            'L' => Tile::Empty,
            '#' => Tile::Occupied,
            _ => unreachable!(),
        }
    }
}

impl Default for &Tile {
    fn default() -> Self {
        &Tile::Floor
    }
}

impl Default for Tile {
    fn default() -> Self {
        Tile::Floor
    }
}

type Parsed = HashMap<Position2D, Tile>;

fn read_input() -> String {
    read_file(11)
}

fn parse_input(raw: &str) -> Parsed {
    raw.lines()
        .enumerate()
        .flat_map(move |(y, l)| {
            l.chars()
                .enumerate()
                .map(move |(x, c)| (Position2D { x: x as i64, y: y as i64 }, Tile::from(c)))
        })
        .collect()
}

fn occupied_neighbors(pos: &Position2D, grid: &Parsed) -> usize {
    pos.moore()
        .iter()
        .filter(|p| grid.get(&p).unwrap_or_default() == &Tile::Occupied)
        .count()
}

fn make_step(previous: &Parsed) -> (Parsed, bool) {
    let mut grid = previous.to_owned();
    let mut changed = false;
    for (pos, tile) in grid.iter_mut() {
        match tile {
            Tile::Floor => (),
            Tile::Empty => {
                if occupied_neighbors(&pos, &previous) == 0 {
                    *tile = Tile::Occupied;
                    changed = true;
                }
            }
            Tile::Occupied => {
                if occupied_neighbors(&pos, &previous) >= 4 {
                    *tile = Tile::Empty;
                    changed = true;
                }
            }
        }
    }
    (grid, changed)
}

fn part1(parsed: &Parsed) -> usize {
    let mut p = parsed.to_owned();
    loop {
        let (parsed, changed) = make_step(&p);
        if !changed {
            break;
        }
        // println!("{}", draw_ascii(&parsed));
        p = parsed;
    }
    p.iter().filter(|(_, t)| t == &&Tile::Occupied).count()
}

const DIRECTIONS: [(i64, i64); 8] = [(0, 1), (1, 0), (1, 1), (0, -1), (-1, 0), (-1, -1), (-1, 1), (1, -1)];

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

fn main() {
    let input = parse_input(&read_input());
    println!("Part 1: {}", part1(&input));
    // println!("Part 2: {}", part2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2020::*;
    use paste::paste;
    use test::black_box;

    const TEST_INPUT: &str = "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL";

    const AFTER_1_STEP: &str = "#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##";

    const AFTER_2_STEPS: &str = "#.LL.L#.##
#LLLLLL.L#
L.L.L..L..
#LLL.LL.L#
#.LL.LL.LL
#.LLLL#.##
..L.L.....
#LLLLLLLL#
#.LLLLLL.L
#.#LLLL.##";

    #[test]
    fn step_test() {
        let input = parse_input(TEST_INPUT);
        let (first_step, changed) = make_step(&input);
        let after_first = parse_input(AFTER_1_STEP);
        assert_eq!(draw_ascii(&first_step, Tile::Floor), draw_ascii(&after_first, Tile::Floor));
        assert_eq!((&first_step, changed), (&after_first, true));
        let (second_step, changed) = make_step(&first_step);
        let after_second = parse_input(AFTER_2_STEPS);
        assert_eq!((second_step, changed), (after_second, true));
    }

    test!(part1() == 37);
    //test!(part2() == 0);
    //bench!(part1() == 0);
    //bench!(part2() == 0);
    //bench_input!(len == 0);
}
