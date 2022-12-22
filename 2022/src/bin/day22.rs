#![feature(test)]
extern crate test;
use aoc2022::{
    boilerplate,
    common::*,
    grid::{Grid, HashGrid, PositionND},
};

const DAY: usize = 22;
type Parsed = (HashGrid<bool, 2>, Vec<Instruction>);

#[derive(Copy, Clone, Debug)]
enum Instruction {
    TurnLeft,
    TurnRight,
    Step(usize),
}

fn parse_input(raw: &str) -> Parsed {
    let (maze, moves) = raw.split_once("\n\n").unwrap();
    let grid = maze
        .lines()
        .zip(1..)
        .flat_map(|(line, x)| line.bytes().zip(1..).filter(|(b, _)| *b != b' ').map(move |(b, y)| (PositionND::from([x, y]), b == b'#')))
        .collect();
    let moves = moves
        .split_inclusive(['L', 'R'])
        .map(|s| s.split_at(s.len() - 1))
        .flat_map(|(n, d)| [Instruction::Step(parse_num(n)), if d == "L" { Instruction::TurnLeft } else { Instruction::TurnRight }])
        .collect();
    (grid, moves)
}
fn part1(parsed: &Parsed) -> usize {
    unimplemented!()
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "\
        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5",
    tests: {
        part1: { TEST_INPUT => 0 },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 0,
    bench2 == 0,
    bench_parse: |(g, i): &Parsed| g.len() + i.len() => 19002,
}
