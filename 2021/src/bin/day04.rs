#![feature(test)]
extern crate test;
use std::collections::HashSet;

use aoc2021::common::*;
use itertools::Itertools;

const DAY: usize = 4;
const BOARD_SIZE: usize = 5;
type Parsed = BingoGame;
type Board = Vec<HashSet<u8>>;

#[derive(Debug, Clone)]
struct BingoGame {
    input_numbers: Vec<u8>,
    boards:        Vec<Board>,
}

impl BingoGame {
    fn mark_number(&mut self, n: &u8) {
        for board in self.boards.iter_mut() {
            for winning_set in board.iter_mut() {
                winning_set.remove(n);
            }
        }
    }

    fn find_winner(&self) -> Option<&Board> {
        self.boards.iter().find(|b| has_won(b))
    }
}

fn has_won(board: &Board) -> bool {
    board.iter().any(|s| s.is_empty())
}

fn parse_input(raw: &str) -> Parsed {
    let (input_numbers, boards) = raw.split_once("\n\n").unwrap();
    let input_numbers = input_numbers.split(',').map(|n| n.parse().unwrap()).collect();
    let boards = boards
        .split("\n\n")
        .map(|b| b.split_ascii_whitespace().map(|n| n.parse().unwrap()).collect())
        .map(|v: Vec<u8>| {
            debug_assert_eq!(v.len(), 25);
            let mut rows: Vec<HashSet<u8>> = v.chunks_exact(BOARD_SIZE).map(|s| s.into_iter().copied().collect()).collect();
            rows.extend(
                (0..BOARD_SIZE)
                    .flat_map(|i| (0..BOARD_SIZE).map(move |j| i + BOARD_SIZE * j))
                    .map(|i| v[i])
                    .chunks(5)
                    .into_iter()
                    .map(|c| c.collect()),
            );
            rows
        })
        .collect();
    BingoGame { input_numbers, boards }
}

fn board_score(board: &Board, current_number: u8) -> usize {
    let remainder: usize = board.into_iter().flatten().unique().map(|&n| n as usize).sum();
    remainder * (current_number as usize)
}

fn part1(parsed: &Parsed) -> usize {
    let mut game = parsed.to_owned();
    for n in &game.input_numbers.clone() {
        game.mark_number(n);
        if let Some(board) = game.find_winner() {
            return board_score(board, *n);
        }
    }
    unreachable!("Game should have ended at some point")
}

fn part2(parsed: &Parsed) -> usize {
    let mut game = parsed.to_owned();
    for n in &game.input_numbers.clone() {
        game.mark_number(n);
        if game.boards.len() > 1 {
            game.boards.retain(|b| !has_won(b));
        } else {
            if let Some(board) = game.find_winner() {
                return board_score(board, *n);
            }
        }
    }
    unreachable!("Game should have ended at some point")
}

fn main() {
    let input = parse_input(&read_file(DAY));
    println!("{input:?}");
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2021::*;

    const TEST_INPUT: &str = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7";

    test!(part1() == 4512);
    test!(part2() == 1924);
    bench!(part1() == 0);
    bench!(part2() == 0);
    // bench_input!(Vec::len => 0);
}
