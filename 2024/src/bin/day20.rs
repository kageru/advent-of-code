#![feature(test)]
extern crate test;
use std::sync::Mutex;

use aoc2024::{
    boilerplate,
    common::*,
    grid::{Grid, VecGrid},
    position::Pos,
};
use fnv::{FnvHashMap, FnvHashSet};
use pathfinding::prelude::{astar, count_paths};

const DAY: usize = 20;
type P = Pos<usize, 2>;
type Parsed = VecGrid<u8>;

fn parse_input(raw: &str) -> Parsed {
    VecGrid::transmute_from_lines(raw)
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
enum Cheat {
    Unused(usize),
    Active(P, usize),
    Used(P, P),
}

fn find_shortest_cheating(grid: &Parsed, start: P, end: &P, steps_until_cheat: usize, limit: usize) -> usize {
    let cheats = Mutex::new(FnvHashSet::default());
    count_paths(
        (start, Cheat::Unused(steps_until_cheat), 0usize),
        |&(pos, c, steps)| {
            pos.manhattan_neighbors_checked()
                .into_iter()
                .filter(move |_| steps < limit)
                .flat_map(move |p| {
                    let is_wall = matches!(grid.get(&p), None | Some(b'#'));
                    match (c, is_wall) {
                        (Cheat::Unused(0), _) => vec![(p, Cheat::Active(pos, 19))],
                        (Cheat::Unused(_), true) => vec![],
                        (Cheat::Unused(n), false) => vec![(p, Cheat::Unused(n - 1))],
                        (Cheat::Used(_, _), true) => vec![],
                        (c @ Cheat::Used(_, _), false) => vec![(p, c)],
                        (Cheat::Active(_, 0), true) => vec![],
                        (Cheat::Active(sp, n @ 1..), true) => vec![(p, Cheat::Active(sp, n - 1))],
                        (Cheat::Active(sp, n @ 1..), false) => {
                            let continue_cheating = Cheat::Active(sp, n - 1);
                            let stop_cheating = Cheat::Used(sp, p);
                            vec![(p, continue_cheating), (p, stop_cheating)]
                        }
                        (Cheat::Active(sp, 0), false) => vec![(p, Cheat::Used(sp, p))],
                    }
                })
                .filter(|(_, c)| if matches!(c, Cheat::Used(_, _)) { cheats.lock().unwrap().insert(*c) } else { true })
                .map(move |(p, c)| (p, c, steps + 1))
        },
        |(p, _, _)| p == end,
    )
    // .unwrap();
    // match steps.last() {
    // Some(&(_, Cheat::Used(start, end))) => (len, start, end),
    // Some(&(_, Cheat::Active(start, _))) => (len, start, *end),
    // _ => unreachable!(),
    // }
}

fn find_shortest(grid: &Parsed, start: &P, end: &P) -> usize {
    astar(
        start,
        |p| p.manhattan_neighbors().into_iter().filter(|&p| grid[p] != b'#').map(|p| (p, 1)),
        |Pos([x, y])| (x.abs_diff(end[0] + y.abs_diff(end[1]))) / 3,
        |p| p == end,
    )
    .unwrap()
    .1
}

fn part1(grid: &Parsed, min_saved: usize) -> usize {
    let start = grid.indices().find(|&p| grid[p] == b'S').expect("No start point");
    let end = grid.indices().find(|&p| grid[p] == b'E').expect("No start point");
    let shortest = find_shortest(grid, &start, &end);
    let mut grid = grid.clone();
    let ylimit = grid.len() - 1;
    let xlimit = grid.0[0].len() - 1;
    (1..ylimit)
        .flat_map(|y| (1..xlimit).map(move |x| Pos([y, x])))
        .filter(|&p| {
            let t = grid[p];
            if t != b'#' {
                return false;
            }
            grid[p] = b'.';
            let cheated_shortest = find_shortest(&grid, &start, &end);
            grid[p] = t;
            shortest - cheated_shortest >= min_saved
        })
        .count()
}

fn part2(grid: &Parsed, min_saved: usize) -> usize {
    let start = grid.indices().find(|&p| grid[p] == b'S').expect("No start point");
    let end = grid.indices().find(|&p| grid[p] == b'E').expect("No start point");
    let shortest = find_shortest(grid, &start, &end);
    (0..(shortest - min_saved))
        .map(|i| find_shortest_cheating(grid, start, &end, i, shortest - min_saved))
        // .unique_by(|&(_, start, end)| (start, end))
        .sum()
    // .filter(|(len, _, _)| dbg!(shortest - len) >= min_saved)
    // .count()
}

boilerplate! {
    TEST_INPUT == "\
###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############"
    for tests: {
        part1: { TEST_INPUT, 1 => 44 },
        part2: { TEST_INPUT, 50 => 285 },
    },
    bench1(100) == 1409,
    bench2(100) == 0,
    bench_parse: Grid::len => 0,
}
