#![feature(test)]
extern crate test;
use std::env;

use itertools::{Itertools, MinMaxResult};

fn read_input() -> String {
    std::fs::read_to_string(
        env::args()
            .nth(1)
            .filter(|n| n != "--bench")
            .unwrap_or_else(|| String::from("inputs/day09")),
    )
    .unwrap()
}

fn parse_input(raw: &str) -> Vec<usize> {
    raw.lines().map(|l| l.parse().unwrap()).collect()
}

fn part1(input: &[usize], window_size: usize) -> usize {
    *input
        .windows(window_size + 1)
        .find_map(|xs| {
            xs.last()
                .filter(|last| !xs[..window_size].iter().tuple_combinations().any(|(a, b)| a + b == **last))
        })
        .unwrap()
}

fn part2(input: &[usize], bad_num: usize) -> usize {
    (0..input.len()).find_map(|i| sum_up_to(&input[i..], bad_num)).unwrap()
}

fn sum_up_to(input: &[usize], n: usize) -> Option<usize> {
    let mut acc = input[0];
    for x in 1..input.len() {
        acc += input[x];
        if acc == n {
            return match input[..x].iter().minmax() {
                MinMaxResult::MinMax(min, max) => Some(min + max),
                _ => unreachable!(),
            };
        }
        if acc > n {
            return None;
        }
    }
    return None;
}

fn main() {
    let input = parse_input(&read_input());
    let p1 = part1(&input, 25);
    println!("Part 1: {}", p1);
    println!("Part 2: {}", part2(&input, p1));
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::black_box;

    const TEST_INPUT: &str = "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576";

    #[test]
    fn part1_test() {
        let input = parse_input(TEST_INPUT);
        assert_eq!(part1(&input, 5), 127);
    }

    #[test]
    fn part2_test() {
        let input = parse_input(TEST_INPUT);
        assert_eq!(part2(&input, 127), 62);
    }

    #[bench]
    fn bench_part1(b: &mut test::Bencher) {
        let input = parse_input(&read_input());
        b.iter(|| assert_eq!(part1(black_box(&input), 25), 393911906));
    }

    #[bench]
    fn bench_part2(b: &mut test::Bencher) {
        let input = parse_input(&read_input());
        b.iter(|| assert_eq!(part2(black_box(&input), 393911906), 59341885));
    }
}
