#![feature(test, bool_to_option)]
extern crate test;
use aoc2020::common::*;
use itertools::Itertools;

#[derive(Debug, PartialEq, Clone)]
enum Rule {
    Char(u8),
    And(Vec<usize>),
    AndOr(Vec<usize>, Vec<usize>),
    Any(Vec<usize>),
    Useless(usize), // just delegates to another rule and makes parsing annoying
    Either(Vec<usize>, Vec<usize>),
}

type Parsed<'a> = (RuleSet, &'a str);

#[derive(Debug, Clone)]
struct RuleSet(Vec<Rule>);

impl RuleSet {
    fn and_combinations(&self, s: &str, rules: &[usize], start: usize) -> Vec<usize> {
        if let Some(&rule) = rules.first() {
            if s.len() <= start {
                return vec![];
            }
            self.matches(&s[start..], self.0[rule].clone())
                .into_iter()
                .flat_map(|i| self.and_combinations(s, &rules[1..], start + i).into_iter())
                .collect()
        } else {
            vec![start]
        }
    }

    fn matches(&self, s: &str, rule: Rule) -> Vec<usize> {
        if s.is_empty() {
            return vec![];
        }
        match rule {
            Rule::Char(c) => (s.as_bytes().first() == Some(&c)).then(|| vec![1]).unwrap_or_else(Vec::new),
            Rule::And(v) => self.and_combinations(s, v.as_slice(), 0),
            Rule::AndOr(v1, v2) => self
                .matches(s, Rule::And(v1))
                .into_iter()
                .chain(self.matches(s, Rule::And(v2)))
                .collect(),
            Rule::Any(v) => v.into_iter().flat_map(|r| self.matches(s, self.0[r].clone())).collect(),
            Rule::Useless(r) => self.matches(s, self.0[r].clone()),
            Rule::Either(first, second) => self
                .matches(s, Rule::And(first))
                .into_iter()
                .chain(self.matches(s, Rule::And(second)))
                .collect(),
        }
    }
}

fn read_input() -> String {
    read_file(19)
}

fn parse_input(raw: &'_ str) -> Parsed<'_> {
    let (rules, inputs) = raw.split_once("\n\n").unwrap();
    let rules = RuleSet(
        rules
            .lines()
            .filter_map(|l| l.split_once(": "))
            // why isn’t this already sorted like it was in the example?
            .sorted_by_key(|t| t.0.parse::<i32>().unwrap())
            .map(|(_, r)| match r {
                or if or.contains('|') => {
                    let mut split = or.split(' ').filter(|s| s != &"|").map(|s| s.parse().unwrap());
                    split
                        .clone()
                        .next_tuple()
                        .map(|(a, b, c, d)| Rule::AndOr(vec![a, b], vec![c, d]))
                        .or_else(|| split.next_tuple().map(|(a, b)| Rule::Any(vec![a, b])))
                        .unwrap()
                }
                chr if chr.contains('"') => Rule::Char(chr.bytes().skip_while(|&b| b != b'"').nth(1).unwrap()),
                and if and.contains(' ') => Rule::And(and.split(' ').map(|s| s.parse().unwrap()).collect()),
                useless => Rule::Useless(useless.parse().unwrap()),
            })
            .collect(),
    );
    (rules, inputs)
}

fn part1((rules, input): &Parsed) -> usize {
    input
        .lines()
        .filter(|l| rules.matches(l, rules.0[0].clone()).contains(&l.len()))
        .count()
}

fn part2(parsed: &Parsed) -> usize {
    let mut rules = parsed.0.clone();
    rules.0[8] = Rule::Either(vec![42], vec![42, 8]);
    rules.0[11] = Rule::Either(vec![42, 31], vec![42, 11, 31]);
    parsed
        .1
        .lines()
        .filter(|l| rules.matches(l, rules.0[0].clone()).contains(&l.len()))
        .count()
}

fn main() {
    let raw = read_input();
    let input = parse_input(&raw);
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2020::*;
    use paste::paste;
    use test::black_box;

    const TEST_INPUT: &str = r#"0: 8 11
1: "a"
2: 1 24 | 14 4
3: 5 14 | 16 1
4: 1 1
5: 1 14 | 15 1
6: 14 14 | 1 14
7: 14 5 | 1 21
8: 42
9: 14 27 | 1 26
10: 23 14 | 28 1
11: 42 31
12: 24 14 | 19 1
13: 14 3 | 1 12
14: "b"
15: 1 | 14
16: 15 1 | 14 14
17: 14 2 | 1 7
18: 15 15
19: 14 1 | 14 14
20: 14 14 | 1 15
21: 14 1 | 1 14
22: 14 14
23: 25 1 | 22 14
24: 14 1
25: 1 1 | 1 14
26: 14 22 | 1 20
27: 1 6 | 14 18
28: 16 1
29: 29
30: 30
31: 14 17 | 1 13
32: 32
33: 33
34: 34
35: 35
36: 36
37: 37
38: 38
39: 39
40: 40
41: 41
42: 9 14 | 10 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"#;

    test!(part1() == 3);
    test!(part2() == 12);
    bench!(part1() == 235);
    bench!(part2() == 379);
    // bench_input!(0.len == 0);
}
