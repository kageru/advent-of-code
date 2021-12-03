#![feature(test)]
extern crate test;
use aoc2021::common::*;
use itertools::Itertools;

const DAY: usize = 03;
type Parsed = Vec<Vec<bool>>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines().map(|line| line.chars().map(|c| c == '1').collect()).collect()
}

fn most_common(parsed: &Parsed) -> String {
    parsed
        .iter()
        .skip(1)
        .fold(parsed[0].iter().map(|&b| b as usize).collect_vec(), |acc, bits| {
            acc.iter().zip(bits).map(|(a, &b)| a + (b as usize)).collect()
        })
        .into_iter()
        .map(|b| if b as f32 >= (parsed.len() as f32) / 2.0 { '1' } else { '0' })
        .collect()
}

fn part1(parsed: &Parsed) -> usize {
    let gamma = most_common(parsed);
    let epsilon: String = gamma.chars().map(|c| if c == '0' { '1' } else { '0' }).collect();
    let gamma = usize::from_str_radix(&gamma, 2).unwrap();
    let epsilon = usize::from_str_radix(&epsilon, 2).unwrap();
    gamma * epsilon
}

fn part2(parsed: &Parsed) -> usize {
    let mut matching_gamma = parsed.clone();
    let mut matching_epsilon = parsed.clone();

    for i in 0..parsed[0].len() {
        let gamma = most_common(&matching_gamma);
        let epsilon = most_common(&matching_epsilon);
        let epsilon: String = epsilon.chars().map(|c| if c == '0' { '1' } else { '0' }).collect();
        matching_gamma.retain(|n| n[i] == (gamma.chars().nth(i).unwrap() == '1'));
        if matching_epsilon.len() > 1 {
            matching_epsilon.retain(|n| n[i] == (epsilon.chars().nth(i).unwrap() == '1'));
        }
    }
    let gamma: String = matching_gamma[0].iter().map(|&b| if b { '1' } else { '0' }).collect();
    let epsilon: String = matching_epsilon[0].iter().map(|&b| if b { '1' } else { '0' }).collect();
    let gamma = usize::from_str_radix(&gamma, 2).unwrap();
    let epsilon = usize::from_str_radix(&epsilon, 2).unwrap();
    gamma * epsilon
}

fn main() {
    let raw = read_file(DAY);
    let input = parse_input(&raw);
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
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

    test!(part1() == 198);
    test!(part2() == 230);
    bench!(part1() == 3549854);
    bench!(part2() == 3765399);
    bench_input!(len == 1000);
}
