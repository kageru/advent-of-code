#![feature(test, try_blocks)]
extern crate test;
use aoc2023::{boilerplate, common::*};
use fnv::FnvHashSet as Set;
use std::mem::transmute;

#[repr(u8)]
#[allow(dead_code)]
#[derive(Debug)]
// Left and Up don’t actually appear in any outputs
enum Tile {
    Empty = b'.',
    Forest = b'#',
    SlopeRight = b'>',
    SlopeDown = b'v',
}

const DAY: usize = 23;
type Parsed<'a> = Vec<&'a [Tile]>;
type Pos = (usize, usize);
const START: Pos = (0, 1);

fn parse_input(raw: &str) -> Parsed {
    raw.lines().map(|l| unsafe { transmute(l) }).collect()
}

fn step(parsed: &Parsed, visited: Set<Pos>, p: Pos, steps: usize) -> Option<usize> {
    let candidates: Vec<Option<Pos>> = match parsed[p.1][p.0] {
        Tile::SlopeDown => vec![Some((p.0, p.1 + 1))],
        Tile::SlopeRight => vec![Some((p.0 + 1, p.1))],
        _ => vec![try { (p.0.checked_sub(1)?, p.1) }, Some((p.0 + 1, p.1)), try { (p.0, p.1.checked_sub(1)?) }, Some((p.0, p.1 + 1))],
    };
    candidates
        .into_iter()
        .flatten()
        .filter(|&(x, y)| !visited.contains(&(x, y)))
        .filter_map(|(x, y)| match try { parsed.get(y)?.get(x)? } {
            None => Some(steps + 1),
            Some(Tile::Empty) => {
                let mut visited = visited.clone();
                visited.insert((x, y));
                step(parsed, visited, (x, y), steps + 1)
            }
            _ => None,
        })
        .max()
}

fn part1(parsed: &Parsed) -> usize {
    let end = (parsed.len() - 1, parsed[0].len() - 2);
    step(parsed, Set::default(), START, 0).unwrap()
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "\
#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#"
    for tests: {
        part1: { TEST_INPUT => 94 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 0,
    bench2 == 0,
    bench_parse: Vec::len => 141,
}
