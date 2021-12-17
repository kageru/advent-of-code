#![feature(core_intrinsics)]
#![feature(test)]
extern crate test;
use std::{intrinsics::unlikely, ops::RangeInclusive};

type TargetArea = (RangeInclusive<isize>, RangeInclusive<isize>);
type Probe = ((isize, isize), (isize, isize));

#[derive(Debug, PartialEq)]
enum ProbeStatus {
    Hit,
    Miss,
    NoLongerReachable,
}

fn calc_status((_, (x, y)): &Probe, (xtarget, ytarget): &TargetArea) -> ProbeStatus {
    if xtarget.contains(x) && ytarget.contains(y) {
        ProbeStatus::Hit
    } else if y < ytarget.start() {
        ProbeStatus::NoLongerReachable
    } else {
        ProbeStatus::Miss
    }
}

fn parse_input() -> TargetArea {
    (34..=67, -215..=-186)
}

fn step(((xvel, yvel), (x, y)): Probe) -> Probe {
    ((xvel - xvel.signum(), yvel - 1), (x + xvel, y + yvel))
}

fn part1(hits: &Vec<((isize, isize), isize)>) -> isize {
    *hits.iter().map(|(_, y)| y).max().unwrap()
}

fn find_hits(target: &TargetArea) -> Vec<((isize, isize), isize)> {
    (0..100)
        .flat_map(move |x| (-1000..1000).map(move |y| (x, y)))
        .filter_map(|(xstart, ystart)| {
            let mut probe = ((xstart, ystart), (0, 0));
            let mut y_high = 0;
            loop {
                probe = step(probe);
                if unlikely(probe.0 .1 == 0) {
                    y_high = probe.1 .1;
                }
                match calc_status(&probe, target) {
                    ProbeStatus::Hit => return Some(((xstart, ystart), y_high)),
                    ProbeStatus::Miss => (),
                    ProbeStatus::NoLongerReachable => return None,
                }
            }
        })
        .collect()
}

fn main() {
    let target = parse_input();
    let hits = find_hits(&target);
    println!("Part 1: {}", part1(&hits));
    println!("Part 2: {}", hits.len());
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2021::*;

    #[test]
    fn part1_test() {
        let input = (20..=30, -10..=-5);
        let hits = find_hits(&input);
        assert_eq!(part1(&hits), 45);
    }

    #[test]
    fn part2_test() {
        let input = (20..=30, -10..=-5);
        let hits = find_hits(&input);
        assert_eq!(hits.len(), 112);
    }

    #[bench]
    fn bench_find_hits(b: &mut test::Bencher) {
        let input = parse_input();
        b.iter(|| assert_eq!(find_hits(&input).len(), 2040))
    }

    #[bench]
    fn bench_part1(b: &mut test::Bencher) {
        let input = parse_input();
        let hits = find_hits(&input);
        b.iter(|| assert_eq!(part1(&hits), 23005))
    }
}
