#![feature(test, bool_to_option)]
extern crate test;
use aoc2020::common::*;
use itertools::Itertools;

fn read_input() -> String {
    read_file(1)
}

fn parse_input(input: &str) -> Vec<usize> {
    input.lines().filter_map(|l| l.parse().ok()).collect()
}

fn part1(input: &[usize]) -> usize {
    input
        .iter()
        .tuple_combinations()
        .find_map(|(&a, &b)| (a + b == 2020).then_some(a * b))
        .unwrap()
}

fn part2(input: &[usize]) -> usize {
    let mut p2_table = [None; 2020];
    for (&a, &b) in input.iter().tuple_combinations() {
        if a + b < 2020 {
            p2_table[a + b] = Some((a, b))
        }
    }
    let (a, b) = input.iter().find_map(|x| p2_table[2020 - x]).unwrap();
    a * b * (2020 - a - b)
}

fn main() {
    let input = parse_input(&read_input());
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2020::*;
    use paste::paste;
    use test::black_box;

    bench!(part1() == 731731);
    bench!(part2() == 116115990);
}
