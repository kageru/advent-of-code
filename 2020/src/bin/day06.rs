#![feature(test)]
extern crate test;
use aoc2020::common::*;
use std::collections::HashSet;

fn main() {
    let input = parse_input(&read_input());
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

fn read_input() -> String {
    read_file(6)
}

fn parse_input(raw: &str) -> Vec<Vec<HashSet<char>>> {
    raw.split("\n\n")
        .map(|group| group.lines().map(|l| l.chars().collect()).collect())
        .collect()
}

fn part1(answers: &[Vec<HashSet<char>>]) -> usize {
    answers.iter().map(|v| v.iter().fold(HashSet::new(), |a, b| &a | b).len()).sum()
}

fn part2(answers: &[Vec<HashSet<char>>]) -> usize {
    answers.iter().map(|v| v.iter().fold(v[0].clone(), |a, b| &a & b).len()).sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2020::*;
    use paste::paste;
    use test::black_box;

    const TEST_INPUT: &str = "abc

a
b
c

ab
ac

a
a
a
a

b";

    const TEST_INPUT_2: &str = "fsl
fsg
fs
fts
sf

a
bdfghjkl

hdcl
cdlh
hdcl
hldc
ldhc";

    fn part2_help(answers: &[Vec<HashSet<char>>]) -> Vec<usize> {
        answers.iter().map(|v| v.iter().fold(v[0].clone(), |a, b| &a & b).len()).collect()
    }

    #[test]
    fn part2_test() {
        assert_eq!(part2_help(&parse_input(TEST_INPUT_2)), vec![2, 0, 4]);
        assert_eq!(part2(&parse_input(TEST_INPUT)), 6);
    }

    bench_input!(len == 490);
    test!(part1 == 11);
    bench!(part1 == 6735);
    bench!(part2 == 3221);
}
