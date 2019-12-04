#![feature(is_sorted)]
use itertools::Itertools;

pub fn main() {
    let valid_part1 = (172_930..683_082)
        .map(|n| n.to_string().chars().collect::<Vec<_>>())
        .filter(|n| n.is_sorted())
        .filter(|v| v.clone().iter().dedup().count() != v.len());
    println!("Part 1: {}", valid_part1.clone().count());

    let part2 = valid_part1.filter(|vec| {
        vec.iter()
            .group_by(move |n| *n)
            .into_iter()
            .fold(false, |acc, (_, v)| acc || v.count() == 2)
    });
    println!("Part 2: {}", part2.count());
}
