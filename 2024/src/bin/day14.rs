#![feature(test)]
extern crate test;
use aoc2024::common::*;
use fnv::FnvHashSet;
use itertools::Itertools;
use std::ops::AddAssign;
use tuple_map::TupleMap2;

const DAY: usize = 14;
type I = i32;
type Parsed = Vec<((I, I), (I, I))>;
type Robot<const X: I, const Y: I> = ((Wrapped<X>, Wrapped<Y>), (I, I));

struct Wrapped<const LIMIT: I>(I);

impl<const N: I> AddAssign<I> for Wrapped<N> {
    fn add_assign(&mut self, rhs: I) {
        self.0 += rhs;
        if self.0 < 0 {
            self.0 += N + 1;
        } else if self.0 > N {
            self.0 -= N + 1;
        }
    }
}

fn parse_input(raw: &str) -> Parsed {
    raw.lines()
        .map(|l| l[2..].split_once(" v=").unwrap())
        .map(|t| t.map(|s| s.split_once(',').expect(s).map(|s| s.parse::<I>().expect(s))))
        .collect()
}

fn convert_to_wrapped<const X: I, const Y: I>(parsed: &Parsed) -> Vec<Robot<X, Y>> {
    parsed.iter().map(|&((x, y), speeds)| ((Wrapped::<X>(x), Wrapped::<Y>(y)), speeds)).collect_vec()
}

fn step<const X: I, const Y: I>(((xpos, ypos), (xspeed, yspeed)): &mut Robot<X, Y>) {
    *xpos += *xspeed;
    *ypos += *yspeed;
}

fn part1<const XLIMIT: I, const YLIMIT: I>(parsed: &Parsed) -> usize {
    let mut robots = convert_to_wrapped::<XLIMIT, YLIMIT>(parsed);
    for _ in 0..100 {
        robots.iter_mut().for_each(step);
    }
    let mut q1 = 0;
    let mut q2 = 0;
    let mut q3 = 0;
    let mut q4 = 0;
    let xhalf = XLIMIT / 2;
    let yhalf = YLIMIT / 2;
    for &((Wrapped(x), Wrapped(y)), _) in &robots {
        if x < xhalf && y < yhalf {
            q1 += 1;
        } else if x < xhalf && y > yhalf {
            q2 += 1;
        } else if x > xhalf && y < yhalf {
            q3 += 1;
        } else if x > xhalf && y > yhalf {
            q4 += 1;
        }
    }
    q1 * q2 * q3 * q4
}

fn part2(parsed: &Parsed) -> usize {
    let mut robots = convert_to_wrapped::<100, 102>(parsed);
    (1..)
        .find(|_| {
            robots.iter_mut().for_each(step);
            tree_tip_exists(&robots)
        })
        .unwrap()
}

fn tree_tip_exists<const X: I, const Y: I>(robots: &[Robot<X, Y>]) -> bool {
    let positions: FnvHashSet<_> = robots.iter().map(|((x, y), _)| (x.0, y.0)).collect();
    positions
        .iter()
        .any(|(x, y)| (1..5).flat_map(|offset| [(x + offset, y + offset), (x - offset, y + offset)]).all(|pt| positions.contains(&pt)))
}

fn main() {
    let raw_input = read_file(DAY);
    let input = parse_input(&raw_input);
    println!("Part 1: {}", part1::<100, 102>(&input));
    println!("Part 2: {}", part2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_INPUT: &str = "\
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3";

    #[test]
    fn part1_test() {
        assert_eq!(12, part1::<10, 6>(&parse_input(TEST_INPUT)));
    }

    #[bench]
    fn part1_bench(b: &mut test::Bencher) {
        let raw = &read_file(DAY);
        let input = parse_input(&raw);
        b.iter(|| assert_eq!(part1::<100, 102>(test::black_box(&input)), 223020000));
    }

    #[bench]
    fn part2_bench(b: &mut test::Bencher) {
        let raw = &read_file(DAY);
        let input = parse_input(&raw);
        b.iter(|| assert_eq!(part2(test::black_box(&input)), 7338));
    }
}
