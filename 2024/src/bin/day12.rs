#![feature(test, try_blocks)]
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

fn part2(grid: &Parsed) -> usize {
    let mut areas = Vec::<FnvHashSet<P>>::new();
    let mut cost = 0;
    for p in grid.indices() {
        if !areas.iter().any(|a| a.contains(&p)) {
            let mut area = FnvHashSet::default();
            let mut perimeter = 0;
            flood(p, grid, &mut area, &mut perimeter);
            areas.push(area);
        }
    }
    for area in areas {
        let mut upscaled = area
            .iter()
            .map(|Pos([x, y])| Pos([x * 4, y * 4]))
            .flat_map(|Pos([x, y])| (0..4).flat_map(move |xo| (0..4).map(move |yo| Pos([x + xo, y + yo]))))
            .collect::<FnvHashSet<_>>();
        let readonly = upscaled.clone();
        upscaled.retain(|p| {
            let neighbors = p.manhattan_neighbors_checked();
            neighbors.len() != 4 || neighbors.iter().any(|p| !readonly.contains(p))
        });
        let readonly = upscaled.clone();
        upscaled.retain(|&Pos([x, y])| !match (x, y) {
            (0, 0) => false,
            (0, 1..) => readonly.contains(&Pos([x, y - 1])) && readonly.contains(&Pos([x, y + 1])),
            (1.., 0) => readonly.contains(&Pos([x - 1, y])) && readonly.contains(&Pos([x + 1, y])),
            _ => {
                (readonly.contains(&Pos([x - 1, y])) && readonly.contains(&Pos([x + 1, y])))
                    || (readonly.contains(&Pos([x, y - 1])) && readonly.contains(&Pos([x, y + 1])))
            }
        });
        let duplicate_corners = upscaled.iter().filter(|p| checked_diagonals(**p).iter().any(|p| upscaled.contains(p))).count() / 2;
        let awful_corners = area
            .iter()
            .filter(|p| p.0[0] > 0 && p.0[1] > 0)
            .filter(|&&p| {
                let ns = neighbors_that_dont_ice(p);
                (0..4).any(|i| awful_corner(grid, p, &ns[i * 2..]))
            })
            .count()
            / 2;
        cost += area.len() * (upscaled.len() - duplicate_corners + awful_corners);
    }
    cost
}

fn awful_corner(grid: &Parsed, p: P, ps: &[P]) -> bool {
    let here = grid[p];
    let o: Option<bool> = try { *grid.get(&ps[1])? == here && *grid.get(&ps[0])? != here && *grid.get(&ps[2])? != here };
    o == Some(true)
}

fn neighbors_that_dont_ice(Pos([x, y]): P) -> [P; 9] {
    [[x, y + 1], [x + 1, y + 1], [x + 1, y], [x + 1, y - 1], [x, y - 1], [x - 1, y - 1], [x - 1, y], [x - 1, y + 1], [x, y + 1]].map(Pos)
}

fn checked_diagonals(Pos([x, y]): P) -> Vec<P> {
    [
        Some(Pos([x + 1, y + 1])),
        try { Pos([x.checked_sub(1)?, y + 1]) },
        try { Pos([x.checked_sub(1)?, y.checked_sub(1)?]) },
        try { Pos([x + 1, y.checked_sub(1)?]) },
    ]
    .into_iter()
    .filter_map(|p: Option<P>| p)
    .collect()
}

boilerplate! {
    TEST_INPUT == "\
AAAA
BBCD
BBCC
EEEC"
    for tests: {
        part1: { TEST_INPUT => 140 },
        part2: {
            TEST_INPUT => 80,
"\
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE" => 1206,
"\
EEEEE
EXXXX
EEEEE
EXXXX
EEEEE" => 236,
"\
AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA" => 368,
        },
    },
    bench1 == 1477924,
    bench2 == 841934,
    bench_parse: Grid::len => 140,
}
