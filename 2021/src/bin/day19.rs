#![feature(test)]
extern crate test;
use aoc2021::common::*;
use itertools::Itertools;

const DAY: usize = 19;
type Parsed = Vec<Scanner>;

#[derive(Debug)]
struct Scanner {
    no:     usize,
    coords: Vec<ScanPoint>,
}

type ScanPoint = [isize; 3];

fn parse_input(raw: &str) -> Parsed {
    raw.split("\n\n")
        .map(|raw_scanner| {
            raw_scanner.lines().skip(1).map(|l| l.split(',').map(|n| n.parse().unwrap()).collect_vec().try_into().unwrap()).collect()
        })
        .enumerate()
        .map(|(no, coords)| Scanner { no, coords })
        .collect()
}

fn part1(parsed: &Parsed) -> usize {
    unimplemented!()
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
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

    const TEST_INPUT: &str = include_str!("../../inputs/day19_test");

    test!(part1() == 0);
    test!(part2() == 0);
    bench!(part1() == 0);
    bench!(part2() == 0);
    bench_input!(Vec::len => 37);
}
