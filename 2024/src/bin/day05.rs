#![feature(test)]
extern crate test;
use aoc2024::{boilerplate, common::*};

const DAY: usize = 05;
type I = usize;
type Rules = [Vec<I>; 100];
type Parsed = (Rules, Vec<Vec<I>>);

fn parse_input(raw: &str) -> Parsed {
    let (rules, prints) = raw.split_once("\n\n").unwrap();
    let prints = prints.lines().map(|l| parse_nums_separator(l, ',')).collect();
    let rules: Vec<(I, I)> = rules.lines().filter_map(|l| l.split_once('|')).map(|(a, b)| (parse_num(a), parse_num(b))).collect();
    let mut full_rules = [const { Vec::new() }; 100];
    for (k, v) in rules {
        full_rules[v].push(k);
    }
    let mut changed = false;
    while changed {
        changed = false;
        for i in 0..100 {
            for before in full_rules[i].clone() {
                if !full_rules[before].contains(&i) {
                    changed = true;
                    full_rules[before].push(i);
                }
            }
        }
    }
    // full_rules[i] now contains all the values that need to appear before `i`
    (full_rules, prints)
}

fn invalid_at(rules: &Rules, print: &[I]) -> Option<(I, I)> {
    (0..print.len() - 1).find_map(|i| rules[print[i]].iter().find_map(|r| print[i..].iter().position(|n| n == r).map(|pos| (i, i + pos))))
}

fn middle_value(v: &Vec<I>) -> I {
    v[v.len() / 2]
}

fn part1((rules, prints): &Parsed) -> usize {
    prints.iter().filter(|p| invalid_at(rules, p).is_none()).map(middle_value).sum()
}

fn fix_print(rules: &Rules, mut print: Vec<I>) -> Vec<I> {
    while let Some((i, j)) = invalid_at(rules, &print) {
        print.swap(i, j);
    }
    print
}

fn part2((rules, prints): &Parsed) -> usize {
    prints.iter().filter(|p| invalid_at(rules, p).is_some()).cloned().map(|p| middle_value(&fix_print(rules, p))).sum()
}

boilerplate! {
    TEST_INPUT == "\
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"
    for tests: {
        part1: { TEST_INPUT => 143 },
        part2: { TEST_INPUT => 123 },
    },
    bench1 == 5091,
    bench2 == 4681,
    bench_parse: |(_, v): &Parsed| v.len() => 185,
}
