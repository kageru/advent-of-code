#![feature(test)]
extern crate test;
use std::collections::HashSet;

use aoc2022::{
    boilerplate,
    common::*,
    grid::{Grid, Position2D, PositionND, VecGrid},
};

const DAY: usize = 12;
type Parsed = (Position2D, Position2D, VecGrid<u8>);

fn find_and_modify(grid: &mut Vec<Vec<u8>>, needle: u8, replacement: u8) -> (usize, usize) {
    grid.iter_mut()
        .enumerate()
        .find_map(|(x, line)| {
            line.iter_mut()
                .enumerate()
                .find_map(|(y, b)| {
                    (b == &needle).then(|| {
                        *b = replacement;
                        y
                    })
                })
                .map(|y| (x, y))
        })
        .unwrap()
}

fn parse_input(raw: &str) -> Parsed {
    let mut grid = VecGrid::from_bytes_2d(raw, |b| b);
    let start = find_and_modify(&mut grid.fields, b'S', b'a');
    let start = Position2D::from([start.0, start.1]);
    let end = find_and_modify(&mut grid.fields, b'E', b'z');
    let end = Position2D::from([end.0, end.1]);
    (start, end, grid)
}

fn part1((start, end, grid): &Parsed) -> usize {
    let xbounds = 0..grid.len();
    let ybounds = 0..grid.fields[0].len();
    let mut distances = VecGrid { fields: vec![vec![usize::MAX; ybounds.end]; xbounds.end] };
    let mut visited = VecGrid { fields: vec![vec![false; ybounds.end]; xbounds.end] };
    let mut points_to_consider = HashSet::from([*start]);
    distances[*start] = 0;
    let mut curr = *start;
    loop {
        if &curr == end {
            return distances[curr];
        }
        points_to_consider.remove(&curr);
        for n in curr
            .neighbors_no_diagonals()
            .into_iter()
            .filter(|&PositionND([x, y])| xbounds.contains(&(x as usize)) && ybounds.contains(&(y as usize)))
        {
            if !visited[n] {
                if grid[curr] >= grid[n] || grid[n] - grid[curr] == 1 {
                    distances[n] = distances[n].min(distances[curr] + 1);
                }
                points_to_consider.insert(n);
            }
        }
        visited[curr] = true;
        curr = *points_to_consider.iter().min_by_key(|&&p| distances[p]).unwrap();
    }
}

fn part2((_, end, grid): &Parsed) -> usize {
    let xbounds = 0..grid.len();
    let ybounds = 0..grid.fields[0].len();
    let mut distances = VecGrid { fields: vec![vec![usize::MAX; ybounds.end]; xbounds.end] };
    let mut visited = VecGrid { fields: vec![vec![false; ybounds.end]; xbounds.end] };
    let mut points_to_consider = HashSet::from([*end]);
    distances[*end] = 0;
    let mut curr = *end;
    loop {
        if grid[curr] == b'a' {
            return distances[curr];
        }
        points_to_consider.remove(&curr);
        for n in curr
            .neighbors_no_diagonals()
            .into_iter()
            .filter(|&PositionND([x, y])| xbounds.contains(&(x as usize)) && ybounds.contains(&(y as usize)))
        {
            if !visited[n] {
                if grid[curr] <= grid[n] || grid[curr] - grid[n] == 1 {
                    distances[n] = distances[n].min(distances[curr] + 1);
                }
                points_to_consider.insert(n);
            }
        }
        visited[curr] = true;
        curr = *points_to_consider.iter().min_by_key(|&&p| distances[p]).unwrap();
    }
}

boilerplate! {
    TEST_INPUT == "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi",
    tests: {
        part1: { TEST_INPUT => 31 },
        part2: { TEST_INPUT => 29 },
    },
    bench1 == 517,
    bench2 == 512,
    bench_parse: |&(PositionND([sx, sy]), PositionND([ex, ey]), _)| ((sx, sy), (ex, ey)) => ((20, 0), (20, 145)),
}
