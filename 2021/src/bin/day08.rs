#![feature(array_from_fn)]
#![feature(once_cell)]
#![feature(test)]
extern crate test;
use aoc2021::common::*;
use itertools::{iproduct, Itertools};
use std::{array, lazy::Lazy, ops};

const DAY: usize = 08;
type Parsed<'a> = Vec<([&'a str; 10], [&'a str; 4])>;

const VALID_DISPLAYS: Lazy<[(&'static str, SSD); 10]> =
    Lazy::new(|| ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"].map(|s| (s, SSD::from(s))));

#[derive(Debug, PartialEq)]
struct SSD {
    a: bool,
    b: bool,
    c: bool,
    d: bool,
    e: bool,
    f: bool,
    g: bool,
}

struct Mapping([char; 7]);

impl Mapping {
    fn translate(&self, c: char) -> char {
        self.0[(c as u8 - b'a') as usize]
    }
}

impl From<&str> for SSD {
    fn from(s: &str) -> Self {
        SSD {
            a: s.contains('a'),
            b: s.contains('b'),
            c: s.contains('c'),
            d: s.contains('d'),
            e: s.contains('e'),
            f: s.contains('f'),
            g: s.contains('g'),
        }
    }
}

impl ops::Sub<&SSD> for &SSD {
    type Output = SSD;
    fn sub(self, rhs: &SSD) -> Self::Output {
        Self::Output {
            a: self.a && !rhs.a,
            b: self.b && !rhs.b,
            c: self.c && !rhs.c,
            d: self.d && !rhs.d,
            e: self.e && !rhs.e,
            f: self.f && !rhs.f,
            g: self.g && !rhs.g,
        }
    }
}

impl SSD {
    fn active_digits(&self) -> u8 {
        self.a as u8 + self.b as u8 + self.c as u8 + self.d as u8 + self.e as u8 + self.f as u8 + self.g as u8
    }

    fn to_array(&self) -> [bool; 7] {
        [self.a, self.b, self.c, self.d, self.e, self.f, self.g]
    }
}

fn parse_input(raw: &str) -> Parsed {
    raw.lines()
        .map(|l| l.split_once(" | ").unwrap())
        .map(|(input, output)| {
            let mut input = input.split(' ').map_into();
            let mut output = output.split(' ').map_into();
            (array::from_fn(|_| input.next().unwrap()), array::from_fn(|_| output.next().unwrap()))
        })
        .collect()
}

fn part1<'a>(parsed: &Parsed<'a>) -> usize {
    parsed.iter().flat_map(|(_, output)| output).filter(|&&input| [2, 3, 4, 7].contains(&SSD::from(input).active_digits())).count()
}

fn part2<'a>(parsed: &Parsed<'a>) -> usize {
    parsed
        .iter()
        .map(|(raw_input, raw_output)| {
            let input = raw_input.map(SSD::from);
            let one = input.iter().find(|d| d.active_digits() == 2).unwrap();
            let four = input.iter().find(|d| d.active_digits() == 4).unwrap();
            let seven = input.iter().find(|d| d.active_digits() == 3).unwrap();
            // We know the position of a for sure because it’s the only difference between 7 and 1
            let a = (seven - one).to_array().iter().position(|&b| b).unwrap();
            // And c and f are these two (both used in 1)
            let c_or_f = one.to_array().iter().positions(|&b| b).collect_vec();
            debug_assert_eq!(c_or_f.len(), 2);
            // 4 uses b, c, d, f, but we already know c and f from 1, so this leaves b and d
            let b_or_d = (four - one).to_array().iter().positions(|&b| b).collect_vec();
            debug_assert_eq!(b_or_d.len(), 2);
            // Now e and g have to be in the remaining two positions
            let e_or_g = (0..7).filter(|n| ![a, b_or_d[0], b_or_d[1], c_or_f[0], c_or_f[1]].contains(n)).collect_vec();
            debug_assert_eq!(e_or_g.len(), 2);
            debug_assert_eq!(
                [a, b_or_d[0], b_or_d[1], c_or_f[0], c_or_f[1], e_or_g[0], e_or_g[1]].into_iter().sorted().collect_vec(),
                (0..7).collect_vec()
            );
            // Now there are 8 possible combinations from multiplying the 3 x_or_y we constructed above.
            let mapping = iproduct!(
                [&b_or_d, &b_or_d.iter().copied().rev().collect()],
                [&c_or_f, &c_or_f.iter().copied().rev().collect()],
                [&e_or_g, &e_or_g.iter().copied().rev().collect()]
            )
            .map(|(b_d, c_f, e_g)| {
                let mut m = [' '; 7];
                m[a] = 'a';
                m[b_d[0]] = 'b';
                m[c_f[0]] = 'c';
                m[b_d[1]] = 'd';
                m[e_g[0]] = 'e';
                m[c_f[1]] = 'f';
                m[e_g[1]] = 'g';
                Mapping(m)
            })
            .find(|m| {
                raw_input.iter().all(|i| {
                    let translated: String = i.chars().map(|n| m.translate(n)).collect();
                    let ssd = SSD::from(translated.as_ref());
                    VALID_DISPLAYS.iter().any(|(_, d)| d == &ssd)
                })
            })
            .unwrap();
            raw_output
                .iter()
                .map(|i| i.chars().map(|n| mapping.translate(n)).collect::<String>())
                .map(|t| SSD::from(t.as_ref()))
                .map(|ssd| VALID_DISPLAYS.iter().position(|(_, d)| &ssd == d).unwrap())
                .fold(0, |acc, n| (acc + n) * 10)
                / 10
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

    const TEST_INPUT: &str = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce";

    test!(part1() == 26);
    test!(part2() == 61229);
    bench!(part1() == 239);
    bench!(part2() == 946346);
    bench_input!(Vec::len => 200);
}
