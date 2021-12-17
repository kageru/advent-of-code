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

fn part1(target: &TargetArea, hits: &Vec<(isize, isize)>) -> isize {
    hits.iter()
        .map(|start_vel| {
            std::iter::successors(Some((*start_vel, (0, 0))), |probe| {
                let probe = step(*probe);
                (calc_status(&probe, target) == ProbeStatus::Miss).then(|| probe)
            })
            .map(|(_, (_, y))| y)
            .max()
            .unwrap()
        })
        .max()
        .unwrap()
}

fn find_hits(target: &TargetArea) -> Vec<(isize, isize)> {
    let mut hits = Vec::new();
    for xstart in -1000..1000 {
        for ystart in -1000..1000 {
            let mut probe = ((xstart, ystart), (0, 0));
            loop {
                probe = step(probe);
                match calc_status(&probe, target) {
                    ProbeStatus::Hit => {
                        hits.push((xstart, ystart));
                        break;
                    }
                    ProbeStatus::Miss => (),
                    ProbeStatus::NoLongerReachable => break,
                }
            }
        }
    }
    hits
}

fn part2(hits: &Vec<(isize, isize)>) -> usize {
    hits.len()
}

fn main() {
    let target = parse_input();
    let hits = find_hits(&target);
    println!("Part 1: {}", part1(&target, &hits));
    println!("Part 2: {}", part2(&hits));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2021::*;

    #[test]
    fn part1_test() {
        let input = (20..=30, -10..=-5);
        let hits = find_hits(&input);
        assert_eq!(part1(&input, &hits), 45);
    }

    #[test]
    fn part2_test() {
        let input = (20..=30, -10..=-5);
        let hits = find_hits(&input);
        assert_eq!(part2(&hits), 112);
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
        b.iter(|| assert_eq!(part1(&input, &hits), 23005))
    }
}
