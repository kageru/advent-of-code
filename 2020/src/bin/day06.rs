#![feature(test)]
extern crate test;
use std::{collections::HashSet, env};

fn main() {
    let input = parse_input(&read_input());
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

fn read_input() -> String {
    std::fs::read_to_string(
        env::args()
            .nth(1)
            .filter(|n| n != "--bench")
            .unwrap_or(String::from("inputs/day06")),
    )
    .unwrap()
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

    #[test]
    fn part1_test() {
        assert_eq!(part1(&parse_input(TEST_INPUT)), 11);
    }

    fn part2_help(answers: &[Vec<HashSet<char>>]) -> Vec<usize> {
        answers.iter().map(|v| v.iter().fold(v[0].clone(), |a, b| &a & b).len()).collect()
    }

    #[test]
    fn part2_test() {
        assert_eq!(part2_help(&parse_input(TEST_INPUT_2)), vec![2, 0, 4]);
        assert_eq!(part2(&parse_input(TEST_INPUT)), 6);
    }

    #[bench]
    fn bench_input_parsing(b: &mut test::Bencher) {
        let s = read_input();
        b.iter(|| assert_eq!(parse_input(black_box(&s)).len(), 490))
    }

    #[bench]
    fn bench_part2(b: &mut test::Bencher) {
        let i = parse_input(&read_input());
        b.iter(|| assert_eq!(part2(black_box(&i)), 3221))
    }

    #[bench]
    fn bench_part1(b: &mut test::Bencher) {
        let i = parse_input(&read_input());
        b.iter(|| assert_eq!(part1(black_box(&i)), 6735))
    }
}
