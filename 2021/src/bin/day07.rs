#![feature(int_abs_diff)]
#![feature(test)]
extern crate test;
use aoc2021::common::*;

const DAY: usize = 7;
type Parsed = Vec<usize>;

fn parse_input(raw: &str) -> Parsed {
    parse_nums_comma(raw)
}

fn part1(parsed: &Parsed) -> usize {
    let mut nums = parsed.to_owned();
    nums.sort();
    let median = nums[nums.len() / 2];
    parsed.iter().map(|p| p.abs_diff(median)).sum()
}

fn part2(parsed: &Parsed) -> usize {
    let avg = parsed.iter().sum::<usize>() / parsed.len();
    parsed.iter().map(|p| p.abs_diff(avg)).map(|n| (0..=n).sum::<usize>()).sum()
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

    const TEST_INPUT: &str = "16,1,2,0,4,2,7,1,2,14";

    test!(part1() == 37);
    bench!(part1() == 328318);
    bench!(part2() == 89791146);
    bench_input!(Vec::len => 1000);
}
