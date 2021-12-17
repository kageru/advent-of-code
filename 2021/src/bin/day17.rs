#![feature(test)]
extern crate test;
use std::ops::RangeInclusive;

type TargetArea = (RangeInclusive<isize>, RangeInclusive<isize>);
type Probe = ((isize, isize), (isize, isize));

#[derive(Debug, PartialEq)]
enum ProbeStatus {
    Hit,
    Miss,
    NoLongerReachable,
}

#[inline]
fn calc_status(((xvel, _), (x, y)): &Probe, (xtarget, ytarget): &TargetArea) -> ProbeStatus {
    if xtarget.contains(x) && ytarget.contains(y) {
        ProbeStatus::Hit
    } else if y < ytarget.start() || x > xtarget.end() || (xvel == &0 && !xtarget.contains(x)) {
        ProbeStatus::NoLongerReachable
    } else {
        ProbeStatus::Miss
    }
}

#[inline]
fn step(((xvel, yvel), (x, y)): &Probe) -> Probe {
    ((xvel - xvel.signum(), yvel - 1), (x + xvel, y + yvel))
}

fn get_target() -> TargetArea {
    (34..=67, -215..=-186)
}

fn part1(hits: &[isize]) -> isize {
    (1..=*hits.iter().max().unwrap()).sum()
}

fn find_hits(target: &TargetArea) -> Vec<isize> {
    // Doing y in the outer loop and x in the inner would allow us to call last() instead of max()
    // in part1, however, for reasons unknown to me, that makes this function 20% slower.
    (1..=*target.0.end())
        .flat_map(move |x| (*target.1.start()..250).map(move |y| (x, y)))
        .filter_map(|(startx, starty)| {
            let mut probe = ((startx, starty), (0, 0));
            loop {
                probe = step(&probe);
                match calc_status(&probe, target) {
                    ProbeStatus::Hit => return Some(starty),
                    ProbeStatus::Miss => continue,
                    ProbeStatus::NoLongerReachable => return None,
                }
            }
        })
        .collect()
}

fn main() {
    let hits = find_hits(&get_target());
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
        let input = get_target();
        b.iter(|| assert_eq!(find_hits(test::black_box(&input)).len(), 2040))
    }

    #[bench]
    fn bench_part1(b: &mut test::Bencher) {
        let input = get_target();
        let hits = find_hits(&input);
        b.iter(|| assert_eq!(part1(test::black_box(&hits)), 23005))
    }
}
