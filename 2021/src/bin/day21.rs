#![feature(test)]
extern crate test;
use itertools::Itertools;

type Parsed = (u16, u16);

const INPUT: Parsed = (7, 3);

fn part1((p1, p2): Parsed) -> usize {
    (1..=100)
        .cycle()
        .tuples()
        .enumerate()
        .inspect(|x| println!("raw: {x:?}"))
        .scan([(p1, 0), (p2, 0)], |mut scores, (round, (_, die, _))| {
            let mut points = (scores[round & 1].0 + die * 3) % 10;
            if points == 0 {
                points = 10;
            }
            println!("Player {} moves {} for {points}", round & 1, die * 3);
            scores[round & 1].0 = points;
            scores[round & 1].1 += points;
            Some((round, *scores))
        })
        .inspect(|x| println!("res: {x:?}"))
        .find_map(|(r, [(_, s1), (_, s2)])| (s1 >= 1000 || s2 >= 1000).then(|| (r + 1) * 3 * (s1.min(s2) as usize)))
        .unwrap()
}

fn part2((p1, p2): Parsed) -> usize {
    unimplemented!()
}

fn main() {
    println!("Part 1: {}", part1(INPUT));
    println!("Part 2: {}", part2(INPUT));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2021::*;

    #[test]
    fn part1_test() {
        assert_eq!(part1((4, 8)), 739785);
    }
}
