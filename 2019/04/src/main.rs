#![feature(is_sorted)]
use itertools::Itertools;

pub fn main() {
    let (lower, upper) = (172930, 683082);
    let valid_part1 = (lower..upper)
        .map(|n| n.to_string().chars().collect::<Vec<_>>())
        .filter(|n| n.is_sorted())
        .filter(|v| v.clone().iter().dedup().count() != v.len());
    println!("Part 1: {}", valid_part1.clone().count());
    let part2 = valid_part1
        .filter(|v| {
            let (mut last, mut streak, mut double) = (' ', 0, false);
            v.iter().for_each(|c| {
                if streak == 2 && c != &last {
                    double = true
                }
                streak = if c == &last { streak + 1 } else { 1 };
                last = *c;
            });
            double || streak == 2
        })
        .count();
    println!("Part 2: {}", part2);
}
