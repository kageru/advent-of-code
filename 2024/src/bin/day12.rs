#![feature(test)]
extern crate test;
use aoc2024::{
    boilerplate,
    common::*,
    grid::{Grid, VecGrid},
    position::Pos,
};
use fnv::FnvHashSet;

const DAY: usize = 12;
type Parsed = VecGrid<u8>;
type P = Pos<usize, 2>;

fn parse_input(raw: &str) -> Parsed {
    VecGrid::from_bytes_2d(raw, |b| b)
}

fn flood(pos: P, grid: &Parsed, area: &mut FnvHashSet<P>, perimeter: &mut usize) {
    if !area.insert(pos) {
        return;
    }
    let current = grid[pos];
    let safe_neighbours = pos.manhattan_neighbors_checked();
    // Neighbors that would go negative
    *perimeter += 4 - safe_neighbours.len();
    for p in safe_neighbours {
        match grid.get(&p) {
            Some(&x) if x == current => flood(p, grid, area, perimeter),
            _ => *perimeter += 1,
        }
    }
}

fn part1(grid: &Parsed) -> usize {
    let mut areas = Vec::<FnvHashSet<P>>::new();
    let mut cost = 0;
    for p in grid.indices() {
        if !areas.iter().any(|a| a.contains(&p)) {
            let mut area = FnvHashSet::default();
            let mut perimeter = 0;
            flood(p, grid, &mut area, &mut perimeter);
            cost += perimeter * area.len();
            areas.push(area);
        }
    }
    cost
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "\
AAAA
BBCD
BBCC
EEEC"
    for tests: {
        part1: { TEST_INPUT => 140 },
        part2: { TEST_INPUT => 80 },
    },
    bench1 == 1477924,
    bench2 == 0,
    bench_parse: Grid::len => 140,
}
