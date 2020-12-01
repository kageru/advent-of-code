#![feature(bool_to_option)]
use itertools::Itertools;
use std::io::BufRead;

fn main() {
    let input: Vec<usize> = std::io::stdin().lock().lines().filter_map(|l| l.unwrap().parse().ok()).collect();
    let p1 = input
        .iter()
        .tuple_combinations()
        .find_map(|(&a, &b)| (a + b == 2020).then_some(a * b))
        .unwrap();
    println!("Part 1: {}", p1);

    let mut p2_table = vec![None; 2020];
    for (&a, &b) in input.iter().tuple_combinations() {
        if a + b < 2020 {
            p2_table[a + b] = Some((a, b))
        }
    }
    let (a, b) = input.iter().find_map(|x| p2_table[2020 - x]).unwrap();
    println!("Part 2: {}", a * b * (2020 - a - b));
}
