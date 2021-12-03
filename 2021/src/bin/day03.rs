#![feature(int_log)]
#![feature(test)]
extern crate test;
use aoc2021::common::*;

const DAY: usize = 03;
type Parsed = Vec<usize>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines().map(|line| usize::from_str_radix(line, 2).unwrap()).collect()
}

fn bit_at(x: usize, n: usize) -> bool {
    (x >> n) & 1 != 0
}

fn most_common(parsed: &Parsed, bits: usize) -> usize {
    (0..bits).rev().map(|n| most_common_at(parsed, n)).fold(0, |acc, b| (acc | (b as usize)) << 1) >> 1
}

fn most_common_at(parsed: &Parsed, n: usize) -> bool {
    parsed.iter().filter(|&&x| bit_at(x, n)).count() * 2 >= parsed.len()
}

fn invert(n: usize) -> usize {
    !n & ((1 << n.log2()) - 1)
}

fn part1(parsed: &Parsed, bits: usize) -> usize {
    let gamma = most_common(parsed, bits);
    let epsilon = invert(gamma);
    gamma * epsilon
}

fn part2(parsed: &Parsed, bits: usize) -> usize {
    let mut matching_gamma = parsed.clone();
    let mut matching_epsilon = parsed.clone();

    for i in (0..bits).rev() {
        let gamma = most_common_at(&matching_gamma, i);
        let epsilon = !most_common_at(&matching_epsilon, i);
        matching_gamma.retain(|&n| bit_at(n, i) == gamma);
        if matching_epsilon.len() > 1 {
            matching_epsilon.retain(|&n| bit_at(n, i) == epsilon);
        }
    }
    debug_assert_eq!(matching_gamma.len(), 1);
    debug_assert_eq!(matching_epsilon.len(), 1);
    matching_gamma[0] * matching_epsilon[0]
}

fn main() {
    let input = parse_input(&read_file(DAY));
    println!("Part 1: {}", part1(&input, 12));
    println!("Part 2: {}", part2(&input, 12));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2021::*;

    const TEST_INPUT: &str = "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010";

    #[test]
    fn most_common_test() {
        let parsed = parse_input(TEST_INPUT);
        assert_eq!(most_common(&parsed, 5), 0b10110)
    }

    #[test]
    fn invert_test() {
        let gamma = 0b10110;
        assert_eq!(invert(gamma), 0b01001);
    }

    #[test]
    fn most_common_at_test() {
        let parsed = parse_input(TEST_INPUT);
        assert_eq!(most_common_at(&parsed, 4), true);
    }

    #[test]
    fn bit_at_test() {
        assert_eq!(bit_at(0b111, 0), true);
        assert_eq!(bit_at(0b111, 1), true);
        assert_eq!(bit_at(0b111, 2), true);
        assert_eq!(bit_at(0b111, 3), false);
        assert_eq!(bit_at(0b101, 1), false);
        assert_eq!(bit_at(0b11101, 3), true);
    }

    test!(part1(5) == 198);
    test!(part2(5) == 230);
    bench!(part1(12) == 3549854);
    bench!(part2(12) == 3765399);
    bench_input!(len == 1000);
}
