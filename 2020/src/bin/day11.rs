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

const DIRECTIONS: [(i64, i64); 8] = [(0, 1), (1, 0), (1, 1), (0, -1), (-1, 0), (-1, -1), (-1, 1), (1, -1)];

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

fn make_step<F: Fn(&Position2D, &Parsed) -> usize>(previous: &Parsed, count_neighbors: F, limit: usize) -> (Parsed, bool) {
    let mut grid = previous.to_owned();
    let mut changed = false;
    for (pos, tile) in grid.iter_mut() {
        match tile {
            Tile::Floor => (),
            Tile::Empty => {
                if count_neighbors(&pos, &previous) == 0 {
                    *tile = Tile::Occupied;
                    changed = true;
                }
            }
            Tile::Occupied => {
                if count_neighbors(&pos, &previous) >= limit {
                    *tile = Tile::Empty;
                    changed = true;
                }
            }
        }
    }
    (grid, changed)
}

fn move_until_equilibrium<F: Fn(&Position2D, &Parsed) -> usize>(parsed: &Parsed, count_neighbors: F, limit: usize) -> usize {
    let mut p = parsed.to_owned();
    loop {
        let (parsed, changed) = make_step(&p, &count_neighbors, limit);
        if !changed {
            break;
        }
        p = parsed;
    }
    p.iter().filter(|(_, t)| t == &&Tile::Occupied).count()
}

fn part1(parsed: &Parsed) -> usize {
    move_until_equilibrium(parsed, occupied_neighbors, 4)
}

fn part2(parsed: &Parsed) -> usize {
    move_until_equilibrium(parsed, neighbors_in_vision, 5)
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
        let input = parse_input(TEST_INPUT);
        let (first_step, changed) = make_step(&input, occupied_neighbors, 4);
        let after_first = parse_input(AFTER_1_STEP);

        assert_eq!(draw_ascii(&first_step), draw_ascii(&after_first));
        assert_eq!((&first_step, changed), (&after_first, true));
        let (second_step, changed) = make_step(&first_step, occupied_neighbors, 4);
        let after_second = parse_input(AFTER_2_STEPS);
        assert_eq!((second_step, changed), (after_second, true));
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
        let input = parse_input(TEST_INPUT);
        let (step_1, changed) = make_step(&input, neighbors_in_vision, 5);
        let after_1 = parse_input(P2_AFTER_1);
        assert_eq!(draw_ascii(&step_1), draw_ascii(&after_1));
        assert_eq!((&step_1, changed), (&after_1, true));

        let (step_2, changed) = make_step(&step_1, neighbors_in_vision, 5);
        let after_2 = parse_input(P2_AFTER_2);
        assert_eq!(draw_ascii(&step_2), draw_ascii(&after_2));
        assert_eq!((&step_2, changed), (&after_2, true));

        let (step_3, changed) = make_step(&step_2, neighbors_in_vision, 5);
        let after_3 = parse_input(P2_AFTER_3);
        assert_eq!(draw_ascii(&step_3), draw_ascii(&after_3));
        assert_eq!((&step_3, changed), (&after_3, true));
    }

    test!(part1() == 37);
    test!(part2() == 26);
    bench!(part1() == 2164);
    bench!(part2() == 1974);
    bench_input!(len == 8372);
}
