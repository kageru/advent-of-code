#![feature(test)]
use fnv::FnvHashMap;
extern crate test;

type Parsed = (u16, u16);
const INPUT: Parsed = (7, 3);

fn part1((p1, p2): Parsed) -> usize {
    (1..=100)
        .cycle()
        .skip(1)
        .step_by(3)
        .zip(1..)
        .scan([(p2, 0), (p1, 0)], |mut scores, (die, round)| {
            let mut points = scores[round & 1].0 + die * 3;
            points -= (points - 1) / 10 * 10;
            scores[round & 1].0 = points;
            scores[round & 1].1 += points;
            Some((round, (scores[0].1, scores[1].1)))
        })
        .find_map(|(r, (s1, s2))| (s1 >= 1000 || s2 >= 1000).then(|| r * 3 * (s1.min(s2) as usize)))
        .unwrap()
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
struct GameState {
    odd_round: bool,
    // Tuples are (position, total score)
    scores:    [(u16, usize); 2],
}

// Just this, but cached and grouped because I don’t want to calc it in the loop
// iproduct!(1..=3, 1..=3, 1..=3).map(|(a, b, c)| a + b + c)
const POSSIBLE_ROLLS: [(u16, usize); 7] = [(6, 7), (5, 6), (7, 6), (4, 3), (8, 3), (3, 1), (9, 1)];

fn part2((p1, p2): Parsed) -> usize {
    let start = GameState { odd_round: false, scores: [(p1, 0), (p2, 0)] };
    let mut games = FnvHashMap::default();
    games.insert(start, 1);
    let mut p1_wins = 0;
    let mut p2_wins = 0;
    while !games.is_empty() {
        games = games
            .iter()
            .flat_map(|(start, count)| POSSIBLE_ROLLS.iter().map(move |(die, count2)| (start, count * count2, die)))
            .map(|(start, count, die)| {
                let mut scores = start.scores;
                let mut points = scores[start.odd_round as usize].0 + die;
                points -= (points - 1) / 10 * 10;
                scores[start.odd_round as usize].0 = points;
                scores[start.odd_round as usize].1 += points as usize;
                (GameState { odd_round: !start.odd_round, scores }, count)
            })
            .fold(FnvHashMap::default(), |mut acc, (state, count)| {
                // Remove done games and queue the rest for another round
                if state.scores[0].1 >= 21 {
                    p1_wins += count;
                } else if state.scores[1].1 >= 21 {
                    p2_wins += count;
                } else {
                    *acc.entry(state).or_insert(0) += count;
                }
                acc
            })
    }
    p1_wins.max(p2_wins)
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

    #[test]
    fn part2_test() {
        assert_eq!(part2((4, 8)), 444356092776315);
    }

    #[bench]
    fn part1_bench(b: &mut test::Bencher) {
        b.iter(|| assert_eq!(part1(test::black_box(INPUT)), 551901))
    }

    #[bench]
    fn part2_bench(b: &mut test::Bencher) {
        b.iter(|| assert_eq!(part2(test::black_box(INPUT)), 272847859601291))
    }
}
