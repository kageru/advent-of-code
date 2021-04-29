#![feature(test)]
extern crate test;
use aoc2020::common::*;
use itertools::Itertools;
use std::ops::RangeInclusive;
use text_io::scan;

#[derive(Debug, Clone, PartialEq)]
struct Parsed {
    rules:          Vec<Rule>,
    my_ticket:      Ticket,
    nearby_tickets: Vec<Ticket>,
}

#[derive(Debug, Clone, PartialEq)]
struct Rule {
    fieldname: String,
    lower:     RangeInclusive<usize>,
    upper:     RangeInclusive<usize>,
}

type Ticket = Vec<usize>;

impl From<&str> for Rule {
    fn from(s: &str) -> Self {
        let name: String;
        let lo1: usize;
        let lo2: usize;
        let hi1: usize;
        let hi2: usize;
        scan!(s.bytes() => "{}: {}-{} or {}-{}", name, lo1, hi1, lo2, hi2);
        Rule {
            fieldname: name,
            lower:     lo1..=hi1,
            upper:     lo2..=hi2,
        }
    }
}

fn read_input() -> String {
    read_file(16)
}

fn parse_input(raw: &str) -> Parsed {
    let (rules, my_ticket, nearby) = raw.split("\n\n").next_tuple().unwrap();
    Parsed {
        rules:          rules.lines().map_into().collect(),
        my_ticket:      parse_nums(my_ticket.split_once('\n').unwrap().1),
        nearby_tickets: nearby.lines().skip(1).map(parse_nums).collect(),
    }
}

// Could be optimized by merging overlapping ranges into one before checking all the tickets.
// … or so I thought until I tried, benchmarked, and saw that that takes twice as long.
fn part1(parsed: &Parsed) -> usize {
    parsed
        .nearby_tickets
        .iter()
        .flat_map(|i| i.iter())
        .filter(|n| !parsed.rules.iter().any(|r| r.lower.contains(n) || r.upper.contains(n)))
        .sum()
}

fn remove_invalid(parsed: &Parsed) -> Vec<&Ticket> {
    parsed
        .nearby_tickets
        .iter()
        .filter(|i| {
            i.iter()
                .all(|n| parsed.rules.iter().any(|r| r.lower.contains(n) || r.upper.contains(n)))
        })
        .collect()
}

fn part2(parsed: &Parsed, key: &str) -> usize {
    let len = parsed.my_ticket.len();
    let only_valid = remove_invalid(parsed);
    let mut remaining_rules = parsed.rules.clone();
    let mut rules = vec![None; len];
    for i in (0..len).cycle() {
        let at_index = only_valid.iter().map(|v| v[i]).collect_vec();
        if rules[i].is_some() {
            continue;
        }
        if let Some(rule) = remaining_rules
            .iter()
            .filter(|r| at_index.iter().all(|i| r.lower.contains(i) || r.upper.contains(i)))
            .exactly_one()
            .ok()
            .cloned()
        {
            // there must be a better way
            let index = remaining_rules.iter().position(|r| r == &rule).unwrap();
            remaining_rules.swap_remove(index);
            rules[i] = Some(rule);
            if rules.iter().all(|r| r.is_some()) {
                break;
            }
        }
    }
    rules
        .into_iter()
        .enumerate()
        .filter_map(|(n, r)| r.filter(|r| r.fieldname.starts_with(key)).map(|_| parsed.my_ticket[n]))
        .product()
}

fn main() {
    let input = parse_input(&read_input());
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input, "departure"));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2020::*;
    use paste::paste;
    use test::black_box;

    const TEST_INPUT: &str = "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12";

    #[bench]
    fn bench_input_parsing(b: &mut test::Bencher) {
        let raw = read_input();
        b.iter(|| assert_eq!(parse_input(black_box(&raw)).rules.len(), 20));
    }

    test!(part1() == 71);
    bench!(part1() == 26026);
    bench!(part2("departure") == 1305243193339);
}
