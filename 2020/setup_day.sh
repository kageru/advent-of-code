#!/bin/sh

today=$(date +%d)
# this assumes that your puzzle input is already in your clipboard
xsel -b > inputs/$today
# add trailing newline if necessary
sed -i -e '$a\' inputs/$today

echo '
#![feature(test)]
extern crate test;
use std::env;

fn read_input() -> String {
    std::fs::read_to_string(env::args().nth(1).filter(|n| n != "--bench").unwrap_or(String::from("inputs/day'$today'"))).unwrap()
}

fn parse_input(raw: &str) -> Vec<!> {
    unimplemented!()
}

fn main() {
    let input = parse_input(&read_input());
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::black_box;

    const TEST_INPUT: &str = "";

}' > src/day$today.rs
