#![feature(test,bool_to_option)]
extern crate test;
use itertools::Itertools;

fn read_input() -> Vec<usize> {
    std::fs::read_to_string("input")
        .unwrap()
        .lines()
        .filter_map(|l| l.parse().ok())
        .collect()
}

fn part1(input: &[usize]) -> usize {
    input
        .iter()
        .tuple_combinations()
        .find_map(|(&a, &b)| (a + b == 2020).then_some(a * b))
        .unwrap()
}

fn part2(input: &[usize]) -> usize {
    let mut p2_table = [None; 2020];
    for (&a, &b) in input.iter().tuple_combinations() {
        if a + b < 2020 {
            p2_table[a + b] = Some((a, b))
        }
    }
    let (a, b) = input.iter().find_map(|x| p2_table[2020 - x]).unwrap();
    a * b * (2020 - a - b)
}

fn main() {
    let input = read_input();
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::{black_box, Bencher};

    #[bench]
    fn bench_part1(b: &mut Bencher) {
        let input = read_input();
        b.iter(|| black_box(part1(black_box(&input))));
    }

    #[bench]
    fn bench_part2(b: &mut Bencher) {
        let input = read_input();
        b.iter(|| black_box(part2(black_box(&input))));
    }
}
