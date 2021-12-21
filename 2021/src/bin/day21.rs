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
        .scan(GameState { odd_round: false, scores: [(p1, 0), (p2, 0)] }, |mut game, die| {
            advance_game(&mut game, die * 3);
            Some((game.scores[0].1, game.scores[1].1))
        })
        .zip(1..)
        .find_map(|((s1, s2), r)| (s1 >= 1000 || s2 >= 1000).then(|| r * 3 * (s1.min(s2) as usize)))
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
    let mut wins = [0, 0];
    while !games.is_empty() {
        for k in games.clone().keys() {
            let (start, count) = games.remove_entry(k).unwrap();
            for &(die, count2) in &POSSIBLE_ROLLS {
                let mut new_state = start;
                advance_game(&mut new_state, die);
                if new_state.scores[start.odd_round as usize].1 >= 21 {
                    wins[new_state.odd_round as usize] += count * count2;
                } else {
                    *games.entry(new_state).or_insert(0) += count * count2;
                }
            }
        }
    }
    wins[0].max(wins[1])
}

fn advance_game(game: &mut GameState, die: u16) {
    let index = game.odd_round as usize;
    let mut points = game.scores[index].0 + die;
    points -= (points - 1) / 10 * 10;
    game.scores[index].0 = points;
    game.scores[index].1 += points as usize;
    game.odd_round = !game.odd_round;
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
