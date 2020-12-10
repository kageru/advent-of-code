#![feature(test, map_first_last, binary_heap_into_iter_sorted)]
extern crate test;
use itertools::Itertools;
use std::{collections::BinaryHeap, env};

const NUM_ROWS: usize = 128;
const NUM_COLS: usize = 8;

#[derive(Debug, PartialEq)]
struct Position {
    row: usize,
    col: usize,
}

#[inline]
fn get_position(pass: &str) -> Position {
    Position {
        row: find_spot(&pass[0..7], 'B', NUM_ROWS),
        col: find_spot(&pass[7..], 'R', NUM_COLS),
    }
}

fn find_spot(s: &str, needle: char, max: usize) -> usize {
    s.chars()
        .zip(1..)
        .filter(|(c, _)| *c == needle)
        .fold(0, |acc, (_, n)| acc + (max >> n))
}

#[inline]
fn calculate_id(p: &Position) -> usize {
    p.row * 8 + p.col
}

fn collect_ids(s: &str) -> BinaryHeap<usize> {
    s.lines().map(get_position).map(|p| calculate_id(&p)).collect()
}

fn find_missing(ids: BinaryHeap<usize>) -> usize {
    ids.into_iter_sorted().tuple_windows().find(|(a, b)| a - b == 2).unwrap().0 - 1
}

fn main() {
    let ids = collect_ids(&read_input());
    let p1 = ids.peek().unwrap();
    println!("Part 1: {}", p1);

    let p2 = find_missing(ids);
    println!("Part 2: {}", p2);
}

fn read_input() -> String {
    std::fs::read_to_string(
        env::args()
            .nth(1)
            .filter(|n| n != "--bench")
            .unwrap_or_else(|| String::from("inputs/day05")),
    )
    .unwrap()
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

    #[bench]
    fn bench_part_1(b: &mut test::Bencher) {
        let raw = read_input();
        b.iter(|| assert_eq!(collect_ids(black_box(&raw)).peek(), Some(&913)))
    }

    #[bench]
    fn bench_part_2(b: &mut test::Bencher) {
        let ids = collect_ids(&read_input());
        b.iter(|| assert_eq!(find_missing(black_box(ids.clone())), 717))
    }
}
