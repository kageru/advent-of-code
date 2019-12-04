#![feature(is_sorted)]
use itertools::Itertools;

pub fn main() {
    let (lower, upper) = (172_930, 683_082);
    let valid_part1 = (lower..upper)
        .map(|n| n.to_string().chars().collect::<Vec<_>>())
        .filter(|n| n.is_sorted())
        .filter(|v| v.clone().iter().dedup().count() != v.len());
    println!("Part 1: {}", valid_part1.clone().count());

    let part2 = valid_part1
        .filter(|vec| {
            vec.iter()
                .group_by(move |n| *n)
                .into_iter()
                .filter_map(|(_, v)| if v.count() == 2 { Some(true) } else { None })
                // can’t do this because I’m too smol brained for the borrow checker
                //.filter(|(_, v)| v.count() == 2)
                .count()
                > 0
        })
        .count();
    println!("Part 2: {}", part2);
}
