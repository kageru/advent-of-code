#![feature(test)]
extern crate test;
use aoc2021::common::*;
use std::iter;

const DAY: usize = 6;
const FERTILITY_CYCLE: usize = 7;
const INITIAL_DELAY: usize = FERTILITY_CYCLE + 2;
type Parsed = Vec<usize>;

fn parse_input(raw: &str) -> Parsed {
    parse_nums_comma(raw)
}

fn simulate<const LIMIT: usize>(parsed: &Parsed) -> usize {
    let next_fertile_day = |day: &usize| Some(day + FERTILITY_CYCLE).filter(|n| n < &LIMIT);
    let mut fish_from_day = [1; LIMIT];
    for i in (0..LIMIT - INITIAL_DELAY).rev() {
        fish_from_day[i] += iter::successors(Some(i + INITIAL_DELAY), next_fertile_day).map(|n| fish_from_day[n]).sum::<usize>()
    }
    let adult_from_day: Vec<usize> =
        (0..FERTILITY_CYCLE).map(|i| iter::successors(Some(i), next_fertile_day).map(|i| fish_from_day[i]).sum()).collect();
    parsed.iter().map(|&i| 1 + adult_from_day[i]).sum()
}

fn main() {
    let input = parse_input(&read_file(DAY));
    println!("Part 1: {}", simulate::<80>(&input));
    println!("Part 2: {}", simulate::<256>(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2021::*;

    const TEST_INPUT: &str = "3,4,3,1,2";

    test!(simulate<80>() == 5934);
    test!(simulate<256>() == 26984457539);
    bench!(simulate<80>() == 365862);
    bench!(simulate<256>() == 1653250886439);
    bench_input!(Vec::len => 300);
}
