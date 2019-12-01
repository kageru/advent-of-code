use std::io::{self, BufRead};
use std::ops::Div;

fn main() {
    let lines: Vec<u32> = io::stdin()
        .lock()
        .lines()
        .map(|l| l.unwrap().parse().unwrap())
        .collect();
    println!("Part 1: {}", part1(lines.clone()));
    println!("Part 2: {}", part2(lines));
}

fn part1(numbers: Vec<u32>) -> u32 {
    numbers.into_iter().map(cost).sum()
}

fn part2(numbers: Vec<u32>) -> u32 {
    numbers.into_iter().map(|m| cost_rec(m, 0)).sum()
}

fn cost(mass: u32) -> u32 {
    mass.div(3).saturating_sub(2)
}

fn cost_rec(mass: u32, acc: u32) -> u32 {
    let c = cost(mass);
    match c {
        0 => acc,
        _ => cost_rec(c, acc + c)
    }
}
