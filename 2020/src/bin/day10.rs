#![feature(test)]
extern crate test;
use std::env;

use itertools::Itertools;

type Parsed = Vec<usize>;

const STEP_SIZE: usize = 3;

fn read_input() -> String {
    std::fs::read_to_string(
        env::args()
            .nth(1)
            .filter(|n| n != "--bench")
            .unwrap_or_else(|| String::from("inputs/day10")),
    )
    .unwrap()
}

fn parse_input(raw: &str) -> Parsed {
    let mut xs: Vec<usize> = raw.lines().map(|l| l.parse().unwrap()).collect();
    xs.push(0);
    // faster than using sorted() directly on the iterator
    xs.sort_unstable();
    xs
}

fn part1(input: &Parsed) -> (usize, usize) {
    input.iter().tuple_windows().fold((0, 1), |(one, three), (a, b)| match b - a {
        1 => (one + 1, three),
        2 => (one, three),
        3 => (one, three + 1),
        _ => unreachable!(),
    })
}

fn part2(input: &Parsed) -> usize {
    let mut iter = input.iter().rev();
    let max = *iter.next().unwrap();
    let mut paths = vec![0; max + STEP_SIZE];
    paths[max] = 1;
    for &i in iter {
        paths[i] = (1..=STEP_SIZE).map(|j| paths[i + j]).sum();
    }
    paths[0]
}

fn main() {
    let input = parse_input(&read_input());
    let (ones, threes) = part1(&input);
    println!("Part 1: {}", ones * threes);
    println!("Part 2: {}", part2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::black_box;

    const TEST_INPUT: &str = "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3";

    #[test]
    fn part1_test() {
        let input = parse_input(TEST_INPUT);
        assert_eq!(part1(&input), (22, 10));
    }

    #[test]
    fn part2_test() {
        let input = parse_input(TEST_INPUT);
        assert_eq!(part2(&input), 19208);
    }

    #[bench]
    fn bench_input_parsing(b: &mut test::Bencher) {
        let raw = read_input();
        b.iter(|| parse_input(black_box(&raw)))
    }

    #[bench]
    fn bench_part1(b: &mut test::Bencher) {
        let input = parse_input(&read_input());
        b.iter(|| assert_eq!(part1(black_box(&input)), (69, 24)));
    }

    #[bench]
    fn bench_part2(b: &mut test::Bencher) {
        let input = parse_input(&read_input());
        b.iter(|| assert_eq!(part2(black_box(&input)), 56693912375296));
    }
}
