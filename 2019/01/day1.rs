use std::io::{self, BufRead};
use std::ops::Div;

fn main() {
    let lines: Vec<u32> = io::stdin()
        .lock()
        .lines()
        .map(|l| l.unwrap().parse().unwrap())
        .collect();
    println!("Part 1: {}", part1(&lines));
    println!("Part 2: {}", part2(&lines));
}

fn part1(numbers: &Vec<u32>) -> u32 {
    numbers.iter().map(|&m| cost(m)).sum()
}

fn part2(numbers: &Vec<u32>) -> u32 {
    numbers.iter().map(|&m| cost_rec(m, 0)).sum()
}

fn cost(mass: u32) -> u32 {
    mass.div(3).saturating_sub(2)
}

fn cost_rec(mass: u32, acc: u32) -> u32 {
    match cost(mass) {
        0 => acc,
        c => cost_rec(c, acc + c)
    }
}
