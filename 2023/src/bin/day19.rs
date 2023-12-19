#![feature(test)]
extern crate test;
use self::Result::*;
use aoc2023::{boilerplate, common::*};
use fnv::FnvHashMap as HashMap;
use itertools::Itertools;
use tuple_map::TupleMap4;

const DAY: usize = 19;
type I = u32;
type Rule<'a> = Box<dyn Fn(&Part) -> Result<'a> + 'a>;
type Parsed<'a> = (HashMap<&'a str, Vec<Rule<'a>>>, Vec<Part>);

#[derive(Clone, Copy, Debug)]
enum Result<'a> {
    Accepted,
    Rejected,
    Next,
    Jump(&'a str),
}

#[derive(Debug)]
struct Part {
    x: I,
    m: I,
    a: I,
    s: I,
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
        None => {
            let target = parse_target(r);
            Box::new(move |_| target)
        }
        Some((condition, target)) => {
            let target = parse_target(target);
            let value = parse_num(&condition[2..]);
            match &condition[0..2] {
                "x<" => Box::new(move |p| if p.x < value { target } else { Next }),
                "x>" => Box::new(move |p| if p.x > value { target } else { Next }),
                "m<" => Box::new(move |p| if p.m < value { target } else { Next }),
                "m>" => Box::new(move |p| if p.m > value { target } else { Next }),
                "a<" => Box::new(move |p| if p.a < value { target } else { Next }),
                "a>" => Box::new(move |p| if p.a > value { target } else { Next }),
                "s<" => Box::new(move |p| if p.s < value { target } else { Next }),
                "s>" => Box::new(move |p| if p.s > value { target } else { Next }),
                _ => unreachable!(),
            }
        }
    }
}

fn parse_input(raw: &str) -> Parsed {
    let (rules, parts) = raw.split_once("\n\n").unwrap();
    let parts = parts
        .lines()
        .map(|l| l[1..].trim_matches('}').split(',').collect_tuple::<(_, _, _, _)>().unwrap().map(|s| parse_num(&s[2..])))
        .map(|(x, m, a, s)| Part { x, m, a, s })
        .collect();
    let rules = rules
        .lines()
        .map(|line| line.trim_end_matches('}').split_once('{').unwrap())
        .map(|(name, rules)| (name, rules.split(',').map(parse_rule).collect()))
        .collect();
    (rules, parts)
}

fn resolve<'a>(part: &Part, rules: &'a [Rule]) -> Result<'a> {
    for rule in rules {
        match rule(part) {
            Next => continue,
            res => return res,
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
                    Accepted => return p.x + p.m + p.a + p.s,
                    Rejected => return 0,
                    Next => unreachable!(),
                }
            }
        })
        .sum()
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
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
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 333263,
    bench2 == 0,
    bench_parse: |(rules, parts): &Parsed| (rules.len(), parts.len()) => (572, 200),
}
