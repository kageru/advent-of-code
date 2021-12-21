#![feature(drain_filter)]
#![feature(test)]
extern crate test;
use aoc2021::{
    common::*,
    grid::{Grid, PositionND, VecGrid},
};
use itertools::Itertools;

const DAY: usize = 15;
type Parsed = VecGrid<u8>;

fn parse_input(raw: &str) -> Parsed {
    VecGrid::from_bytes_2d(raw, |b| b - b'0')
}

// I win the award for slowest solution, at least.
fn part1(parsed: &Parsed, grid_size: usize) -> usize {
    let mut paths = vec![(PositionND::from([0, 0]), 0)];
    let end = PositionND::from([grid_size - 1, grid_size - 1]);
    let mut max_risk = 0usize;
    loop {
        let min_index = paths.iter().position_min_by_key(|(_, c)| c).unwrap();
        let (next_candidate, risk_so_far) = paths.swap_remove(min_index);
        for (risk, position) in next_candidate.neighbors_no_diagonals().into_iter().filter_map(|p| parsed.get(&p).zip(Some(p))) {
            let new_risk = risk_so_far + *risk as usize;
            max_risk = max_risk.max(new_risk);
            paths.push((position, new_risk));
        }
        paths.sort_unstable_by_key(|(p, _)| p.points[0] as usize * grid_size + p.points[1] as usize);
        paths = paths.into_iter().coalesce(|(p, r), (p2, r2)| if p == p2 { Ok((p, r.min(r2))) } else { Err(((p, r), (p2, r2))) }).collect();
        if let Some((_, risk)) = paths.iter().filter(|(p, _)| p == &end).min_by_key(|(_, r)| r) {
            return *risk;
        }
    }
}

fn part2(_parsed: &Parsed) -> usize {
    todo!()
}

fn main() {
    let input = parse_input(&read_file(DAY));
    println!("Part 1: {}", part1(&input, 100));
    println!("Part 2: {}", part2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2021::*;

    const TEST_INPUT: &str = "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581";

    test!(part1(10) == 40);
    test!(part2() == 0);
    bench!(part1(100) == 656);
    bench!(part2() == 0);
    bench_input!(VecGrid::len => 100);
}
