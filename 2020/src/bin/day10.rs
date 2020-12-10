#![feature(test)]
extern crate test;
use aoc2020::common::*;

use itertools::Itertools;

type Parsed = Vec<usize>;

const STEP_SIZE: usize = 3;

fn read_input() -> String {
    read_file(10)
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
    use aoc2020::*;
    use paste::paste;
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

    test!(part1 == (22, 10));
    test!(part2 == 19208);
    bench!(part1 == (69, 24));
    bench!(part2 == 56693912375296);
    bench_input!(len == 93);
}
