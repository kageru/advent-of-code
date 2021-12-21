#!/bin/sh

today=$(date +%d)
aocd > inputs/day$today

echo '#![feature(test)]
extern crate test;
use aoc2021::common::*;

const DAY: usize = '$today';
type Parsed = Vec<usize>;

fn parse_input(raw: &str) -> Parsed {
    todo!()
}

fn part1(parsed: &Parsed) -> usize {
    todo!()
}

fn part2(parsed: &Parsed) -> usize {
    todo!()
}

fn main() {
    let input = parse_input(&read_file(DAY));
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2021::*;

    const TEST_INPUT: &str = "";

    test!(part1() == 0);
    test!(part2() == 0);
    bench!(part1() == 0);
    bench!(part2() == 0);
    bench_input!(Vec::len => 0);
}' > src/bin/day$today.rs
