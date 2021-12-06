#![feature(test)]
extern crate test;
use aoc2021::common::*;
use std::iter;

const DAY: usize = 6;
const DAYS_PER_CHILD: usize = 6;
const DAYS_UNTIL_FERTILE: usize = 2;
type Parsed = Vec<usize>;

fn parse_input(raw: &str) -> Parsed {
    parse_nums_comma(raw)
}

fn part1(parsed: &Parsed, generations: usize) -> usize {
    iter::successors(Some(parsed.to_owned()), |fish| {
        let fish = fish
            .into_iter()
            .flat_map(|n| match n {
                0 => vec![DAYS_PER_CHILD, DAYS_PER_CHILD + DAYS_UNTIL_FERTILE],
                &n => vec![n - 1],
            })
            .collect();
        Some(fish)
    })
    .take(generations + 1)
    .last()
    .unwrap()
    .len()
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

fn main() {
    let input = parse_input(&read_file(DAY));
    println!("Part 1: {}", part1(&input, 80));
    println!("Part 2: {}", part2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2021::*;

    const TEST_INPUT: &str = "3,4,3,1,2";

    test!(part1(80) == 5934);
    test!(part2() == 0);
    bench!(part1(80) == 364461);
    bench!(part2() == 0);
    bench_input!(Vec::len => 0);
}
