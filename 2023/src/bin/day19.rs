#![feature(test, iter_array_chunks, inline_const)]
extern crate test;
use self::Result::*;
use aoc2023::{boilerplate, common::*};
use fnv::FnvHashMap as HashMap;
use std::ops::RangeInclusive;

const DAY: usize = 19;
const LIMIT: I = 4000;

type I = usize;
type Rule<'a> = (Comp, Result<'a>);
type Rules<'a> = HashMap<&'a str, Vec<Rule<'a>>>;
type Parsed<'a> = (Rules<'a>, Vec<Part>);
type Part = [I; 4];
type Ranges = [RangeInclusive<I>; 4];

#[derive(Clone, Copy, Debug)]
enum Result<'a> {
    Accepted,
    Rejected,
    Jump(&'a str),
}

#[derive(Clone, Copy, Debug)]
enum Comp {
    GT(usize, I),
    LT(usize, I),
    None,
}

fn parse_target(t: &str) -> Result<'_> {
    match t {
        "R" => Rejected,
        "A" => Accepted,
        s => Jump(s),
    }
}

fn parse_rule<'a>(r: &'a str) -> Rule {
    match r.split_once(':') {
        None => (Comp::None, parse_target(r)),
        Some((condition, target)) => {
            let value = parse_num(&condition[2..]);
            let idx = match condition.as_bytes()[0] {
                b'x' => 0,
                b'm' => 1,
                b'a' => 2,
                b's' => 3,
                _ => unreachable!(),
            };
            let comp = match condition.as_bytes()[1] {
                b'<' => Comp::LT(idx, value),
                b'>' => Comp::GT(idx, value),
                _ => unreachable!(),
            };
            (comp, parse_target(target))
        }
    }
}

fn parse_input(raw: &str) -> Parsed {
    let (rules, parts) = raw.split_once("\n\n").unwrap();
    let parts = parts.lines().map(|l| l[1..(l.len() - 1)].split(',').array_chunks().next().unwrap().map(|s| parse_num(&s[2..]))).collect();
    let rules = rules
        .lines()
        .map(|line| line[..(line.len() - 1)].split_once('{').unwrap())
        .map(|(name, rules)| (name, rules.split(',').map(parse_rule).collect()))
        .collect();
    (rules, parts)
}

fn resolve<'a>(part: &Part, rules: &'a [Rule]) -> &'a Result<'a> {
    for (comp, res) in rules {
        match comp {
            Comp::None => return res,
            &Comp::LT(idx, val) if part[idx] < val => return res,
            &Comp::GT(idx, val) if part[idx] > val => return res,
            _ => continue,
        }
    }
    unreachable!()
}

fn part1((rules, parts): &Parsed) -> I {
    parts
        .iter()
        .map(|p| {
            let mut rule = rules.get("in").unwrap();
            loop {
                match resolve(p, &rule) {
                    Jump(r) => rule = rules.get(r).unwrap(),
                    Accepted => return p.iter().sum(),
                    Rejected => return 0,
                }
            }
        })
        .sum()
}

fn num_ranges(ranges: &Ranges) -> usize {
    ranges.iter().map(|r| r.end() - r.start() + 1).product()
}

fn split(all_rules: &Rules, rules: &[Rule], rule_idx: usize, matching_range: Ranges, nonmatching_range: Ranges, res: Result<'_>) -> usize {
    let nonmatching = accepted(all_rules, rules, rule_idx + 1, nonmatching_range);
    match res {
        Jump(dst) => accepted(all_rules, all_rules.get(dst).unwrap(), 0, matching_range) + nonmatching,
        Accepted => num_ranges(&matching_range) + nonmatching,
        Rejected => nonmatching,
    }
}

fn accepted(all_rules: &Rules, rules: &[Rule], rule_idx: usize, mut ranges: Ranges) -> usize {
    match rules[rule_idx] {
        (Comp::None, Jump(dst)) => accepted(all_rules, all_rules.get(dst).unwrap(), 0, ranges),
        (Comp::None, Accepted) => num_ranges(&ranges),
        (Comp::None, Rejected) => 0,
        (Comp::LT(idx, val), res) => {
            let mut matching_ranges = ranges.clone();
            matching_ranges[idx] = *matching_ranges[idx].start()..=val - 1;
            ranges[idx] = val..=*ranges[idx].end();
            split(all_rules, rules, rule_idx, matching_ranges, ranges, res)
        }
        (Comp::GT(idx, val), res) => {
            let mut matching_ranges = ranges.clone();
            matching_ranges[idx] = (val + 1)..=*matching_ranges[idx].end();
            ranges[idx] = *ranges[idx].start()..=val;
            split(all_rules, rules, rule_idx, matching_ranges, ranges, res)
        }
    }
}

fn part2((rules, _): &Parsed) -> usize {
    accepted(rules, rules.get("in").unwrap(), 0, [const { 1..=LIMIT }; 4])
}

boilerplate! {
    TEST_INPUT == "\
px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}"
    for tests: {
        part1: { TEST_INPUT => 19114 },
        part2: { TEST_INPUT => 167409079868000 },
    },
    unittests : {
        num_ranges: { &[1..=10, 1..=10, 1..=10, 1..=10] => 10_000 },
    },
    bench1 == 333263,
    bench2 == 130745440937650,
    bench_parse: |(rules, parts): &Parsed| (rules.len(), parts.len()) => (572, 200),
}
