#![feature(test)]
extern crate test;
use std::collections::{HashMap, HashSet};

use aoc2021::common::*;
use itertools::Itertools;

const DAY: usize = 12;
type Parsed<'a> = HashMap<Node<'a>, Vec<Node<'a>>>;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum Node<'a> {
    Start,
    End,
    Small(&'a str),
    Big(&'a str),
}

impl<'a> From<&'a str> for Node<'a> {
    fn from(s: &'a str) -> Self {
        match s {
            "start" => Node::Start,
            "end" => Node::End,
            cave if cave.chars().all(|c| c.is_ascii_uppercase()) => Node::Big(cave),
            cave if cave.chars().all(|c| c.is_ascii_lowercase()) => Node::Small(cave),
            _ => unreachable!(),
        }
    }
}

fn parse_input(raw: &str) -> Parsed {
    raw.lines()
        .map(|l| l.split_once('-').unwrap())
        .map(|(from, to)| (Node::from(from), Node::from(to)))
        .flat_map(|(a, b)| [(a, b), (b, a)]) // connections always go both ways
        .into_group_map()
}

fn part1(parsed: &Parsed) -> usize {
    possible_paths(parsed, &Node::Start, HashSet::new(), false)
}

fn part2(parsed: &Parsed) -> usize {
    possible_paths(parsed, &Node::Start, HashSet::new(), true)
}

fn possible_paths<'a>(map: &'a Parsed, position: &'a Node<'a>, mut visited: HashSet<&'a Node<'a>>, small_cave_allowed: bool) -> usize {
    if matches!(position, &Node::Small(_)) {
        visited.insert(position);
    }
    map.get(position)
        .unwrap()
        .iter()
        .map(|p| match p {
            Node::Big(_) => possible_paths(map, p, visited.clone(), small_cave_allowed),
            Node::Small(_) if !visited.contains(&p) => possible_paths(map, p, visited.clone(), small_cave_allowed),
            Node::Small(_) if small_cave_allowed => possible_paths(map, p, visited.clone(), false),
            Node::Small(_) | Node::Start => 0,
            Node::End => 1,
        })
        .sum()
}

fn main() {
    let raw = read_file(DAY);
    let input = parse_input(&raw);
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2021::*;

    const TEST_INPUT: &str = "start-A
start-b
A-c
A-b
b-d
A-end
b-end";

    const TEST_INPUT_2: &str = "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc";

    const TEST_INPUT_3: &str = "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW";

    test!(part1() == 10);
    test!(with _2: part1() == 19);
    test!(with _3: part1() == 226);
    test!(part2() == 36);
    test!(with _2: part2() == 103);
    test!(with _3: part2() == 3509);
    bench!(part1() == 4411);
    bench!(part2() == 136767);
    bench_input!(HashMap::len => 13);
}
