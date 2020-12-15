#![feature(test)]
extern crate test;
use std::collections::HashMap;

type Parsed = Vec<usize>;

fn read_input() -> String {
    String::from("15,12,0,14,3,1")
}

fn parse_input(raw: &str) -> Parsed {
    raw.split(',').filter_map(|x| x.parse().ok()).collect()
}

#[rustfmt::skip]
fn part1(initial: &Parsed, limit: usize) -> usize {
    (initial.len()..limit - 1).fold(
        (initial.iter().enumerate().map(|(i, n)| (*n, i)).collect::<HashMap<_, _>>(), 0),
        |(mut prev, curr), i| {
            let next = prev.get(&curr).map(|p| i - p).unwrap_or(0);
            prev.insert(curr, i);
            (prev, next)
        },
    ).1
}

// only here so the test/bench macro works
#[inline]
fn part2(parsed: &Parsed, limit: usize) -> usize {
    part1(parsed, limit)
}

fn main() {
    let input = parse_input(&read_input());
    println!("Part 1: {}", part1(&input, 2020));
    println!("Part 2: {}", part1(&input, 30000000));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2020::*;
    use paste::paste;
    use test::black_box;

    const TEST_INPUT: &str = "0,3,6";

    test!(part1(2020) == 436);
    test!(part2(30000000) == 175594);
    bench!(part1(2020) == 249);
    // bench!(part2(30000000) == 41687);
    bench_input!(len == 6);
}
