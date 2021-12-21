#![feature(test)]
extern crate test;

type Parsed = (u16, u16);

const INPUT: Parsed = (7, 3);

fn part1((p1, p2): Parsed) -> usize {
    (1..=100)
        .cycle()
        .skip(1)
        .step_by(3)
        .enumerate()
        .scan([(p1, 0), (p2, 0)], |mut scores, (round, die)| {
            let mut points = scores[round & 1].0 + die * 3;
            // can’t use mod 10 because we actually want to keep the 10
            while points > 10 {
                points -= 10;
            }
            scores[round & 1].0 = points;
            scores[round & 1].1 += points;
            Some((round + 1, *scores))
        })
        .find_map(|(r, [(_, s1), (_, s2)])| (s1 >= 1000 || s2 >= 1000).then(|| r * 3 * (s1.min(s2) as usize)))
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

    #[bench]
    fn part1_bench(b: &mut test::Bencher) {
        b.iter(|| assert_eq!(part1(test::black_box(INPUT)), 551901))
    }
}
