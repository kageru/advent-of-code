#!/bin/sh

today=$(date +%d)
mkdir "$today"
cd "$today"
cargo init --name "day$today"
echo 'Initialized cargo project'
cargo add itertools

# this assumes that your puzzle input is already in your clipboard
xsel -b > input
# add trailing newline if necessary
sed -i -e '$a\' input

echo '
#![feature(test)]
extern crate test;
use std::env;

fn read_input() -> String {
    std::fs::read_to_string(env::args().nth(1).unwrap_or(String::from("input"))).unwrap()
}

fn parse_input(raw: &str) -> ! {
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

}' >> src/main.rs
