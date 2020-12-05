#![feature(test)]
extern crate test;
use std::collections::HashSet;

const NUM_ROWS: usize = 128;
const NUM_COLS: usize = 8;

#[derive(Debug, PartialEq, Hash, Eq)]
struct Position {
    row: usize,
    col: usize,
}

fn get_position(pass: &str) -> Position {
    Position {
        row: pass[0..7]
            .chars()
            .zip(1..)
            .filter(|(c, _)| *c == 'B')
            .fold(0, |acc, (_, n)| acc + (NUM_ROWS >> n)),
        col: pass[7..]
            .chars()
            .zip(1..)
            .filter(|(c, _)| *c == 'R')
            .fold(0, |acc, (_, n)| acc + (NUM_COLS >> n)),
    }
}

fn calculate_id(p: &Position) -> usize {
    p.row * 8 + p.col
}

fn main() {
    let positions: HashSet<_> = read_input().lines().map(get_position).map(|p| calculate_id(&p)).collect();
    let p1 = positions.iter().max().unwrap();
    println!("Part 1: {}", p1);

    let p2 = (1..NUM_ROWS - 1)
        .flat_map(|row| {
            (0..NUM_COLS)
                .map(|col| calculate_id(&Position { row, col }))
                .filter(|id| !positions.contains(&id) && positions.contains(&(id - 1)) && positions.contains(&(id + 1)))
                .collect::<Vec<_>>()
        })
        .next()
        .unwrap();
    println!("Part 2: {}", p2);
}

fn read_input() -> String {
    std::fs::read_to_string("input").unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::black_box;

    const PASS_1: &str = "BFFFBBFRRR";
    const PASS_2: &str = "FFFBBBFRRR";
    const PASS_3: &str = "BBFFBBFRLL";

    const POS_1: Position = Position { row: 70, col: 7 };
    const POS_2: Position = Position { row: 14, col: 7 };
    const POS_3: Position = Position { row: 102, col: 4 };

    const SID_1: usize = 567;
    const SID_2: usize = 119;
    const SID_3: usize = 820;

    #[test]
    fn test_get_position() {
        assert_eq!(get_position(PASS_1), POS_1);
        assert_eq!(get_position(PASS_2), POS_2);
        assert_eq!(get_position(PASS_3), POS_3);
    }

    #[test]
    fn test_calculate_id() {
        assert_eq!(calculate_id(&POS_1), SID_1);
        assert_eq!(calculate_id(&POS_2), SID_2);
        assert_eq!(calculate_id(&POS_3), SID_3);
    }
}
