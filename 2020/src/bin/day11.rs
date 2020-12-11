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
    #[inline]
    fn from(c: char) -> Self {
        match c {
            '.' => Tile::Floor,
            'L' => Tile::Empty,
            '#' => Tile::Occupied,
            _ => unreachable!(),
        }
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

#[inline]
fn occupied_neighbors(pos: &Position2D, grid: &Parsed) -> usize {
    pos.moore()
        .iter()
        .filter(|p| grid.get(&p).unwrap_or(&Tile::Floor) == &Tile::Occupied)
        .count()
}

const DIRECTIONS: [(i64, i64); 8] = [(0, 1), (1, 0), (1, 1), (0, -1), (-1, 0), (-1, -1), (-1, 1), (1, -1)];

#[inline]
fn neighbors_in_vision(pos: &Position2D, grid: &Parsed) -> usize {
    DIRECTIONS
        .iter()
        .map(|t| Position2D::from(*t))
        .map(|p| {
            (1..)
                .find_map(|n| match grid.get(&(*pos + (p * n))) {
                    Some(&Tile::Occupied) => Some(true),
                    Some(&Tile::Floor) => None,
                    _ => Some(false),
                })
                .unwrap()
        })
        .filter(|&b| b)
        .count()
}

fn make_step<F: Fn(&Position2D, &Parsed) -> usize>(previous: &mut Parsed, count_neighbors: F, limit: usize) -> bool {
    let readonly = previous.to_owned();
    let mut changed = false;
    for (pos, tile) in previous.iter_mut() {
        match tile {
            Tile::Floor => (),
            Tile::Empty => {
                if count_neighbors(&pos, &readonly) == 0 {
                    *tile = Tile::Occupied;
                    changed = true;
                }
            }
            Tile::Occupied => {
                if count_neighbors(&pos, &readonly) >= limit {
                    *tile = Tile::Empty;
                    changed = true;
                }
            }
        }
    }
    changed
}

fn move_until_equilibrium<F: Fn(&Position2D, &Parsed) -> usize>(mut parsed: Parsed, count_neighbors: F, limit: usize) -> usize {
    while make_step(&mut parsed, &count_neighbors, limit) {}
    parsed.iter().filter(|(_, t)| t == &&Tile::Occupied).count()
}

fn part1(parsed: &Parsed) -> usize {
    move_until_equilibrium(parsed.to_owned(), occupied_neighbors, 4)
}

fn part2(parsed: &Parsed) -> usize {
    move_until_equilibrium(parsed.to_owned(), neighbors_in_vision, 5)
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
        let mut grid = parse_input(TEST_INPUT);
        assert!(make_step(&mut grid, occupied_neighbors, 4));
        let after_first = parse_input(AFTER_1_STEP);
        assert_eq!(draw_ascii(&grid), draw_ascii(&after_first));
        assert_eq!(grid, after_first);

        assert!(make_step(&mut grid, occupied_neighbors, 4));
        let after_second = parse_input(AFTER_2_STEPS);
        assert_eq!(grid, after_second);
    }

    const P2_AFTER_1: &str = "#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##";
    const P2_AFTER_2: &str = "#.LL.LL.L#
#LLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLL#
#.LLLLLL.L
#.LLLLL.L#";
    const P2_AFTER_3: &str = "#.L#.##.L#
#L#####.LL
L.#.#..#..
##L#.##.##
#.##.#L.##
#.#####.#L
..#.#.....
LLL####LL#
#.L#####.L
#.L####.L#";

    #[test]
    fn step_test_part2() {
        let mut grid = parse_input(TEST_INPUT);
        assert!(make_step(&mut grid, neighbors_in_vision, 5));
        let after_1 = parse_input(P2_AFTER_1);
        assert_eq!(draw_ascii(&grid), draw_ascii(&after_1));
        assert_eq!(&grid, &after_1);

        assert!( make_step(&mut grid, neighbors_in_vision, 5));
        let after_2 = parse_input(P2_AFTER_2);
        assert_eq!(draw_ascii(&grid), draw_ascii(&after_2));
        assert_eq!(&grid, &after_2);

        assert!(make_step(&mut grid, neighbors_in_vision, 5));
        let after_3 = parse_input(P2_AFTER_3);
        assert_eq!(draw_ascii(&grid), draw_ascii(&after_3));
        assert_eq!(&grid, &after_3);
    }

    test!(part1() == 37);
    test!(part2() == 26);
    bench!(part1() == 2164);
    bench!(part2() == 1974);
    bench_input!(len == 8372);
}
