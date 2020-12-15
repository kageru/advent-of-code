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

fn part1(parsed: &Parsed, limit: usize) -> usize {
    let mut iter = parsed.iter();
    let mut previous = HashMap::new();
    let mut current = 0;
    for i in 0..limit - 1 {
        if let Some(&n) = iter.next() {
            current = n;
            previous.insert(current, i);
            current = 0;
            continue;
        }
        println!("Adding {} to {:?}", current, previous);
        match previous.get(&current) {
            Some(&position) => {
                previous.insert(current, i);
                current = i.saturating_sub(position);
            }
            None => {
                previous.insert(current, i);
                current = 0;
            }
        }
    }
    current
}

// only here so the test/bench macro works
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
    //bench!(part1() == 0);
    //bench!(part2() == 0);
    bench_input!(len == 6);
}
