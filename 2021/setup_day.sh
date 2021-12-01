#!/bin/sh

today=$(date +%d)
aocd > inputs/day$today

echo '#![feature(test)]
extern crate test;
use aoc2021::common::*;

type Parsed = Vec<usize>;

fn read_input() -> String {
    read_file('$today')
}


fn parse_input(raw: &str) -> Parsed {
    unimplemented!()
}

fn part1(parsed: &Parsed) -> usize {
    unimplemented!()
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

fn main() {
    let input = parse_input(&read_input());
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2021::*;
    use paste::paste;
    use test::black_box;

    const TEST_INPUT: &str = "";

    test!(part1() == 0);
    test!(part2() == 0);
    bench!(part1() == 0);
    bench!(part2() == 0);
    bench_input!(len == 0);
}' > src/bin/day$today.rs
