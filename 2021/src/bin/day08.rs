#![feature(array_from_fn)]
#![feature(array_zip)]
#![feature(once_cell)]
#![feature(test)]
extern crate test;
use aoc2021::common::*;
use itertools::{iproduct, Itertools};
use std::{array, lazy::Lazy, ops};

const DAY: usize = 08;
type Parsed<'a> = Vec<([&'a str; 10], [&'a str; 4])>;

const VALID_DISPLAYS: Lazy<[SSD; 10]> =
    Lazy::new(|| ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"].map(SSD::from));

#[derive(Debug, PartialEq)]
struct SSD([bool; 7]);

struct Mapping([char; 7]);

impl Mapping {
    fn translate(&self, c: char) -> char {
        self.0[(c as u8 - b'a') as usize]
    }
}

impl From<&str> for SSD {
    fn from(s: &str) -> Self {
        SSD([s.contains('a'), s.contains('b'), s.contains('c'), s.contains('d'), s.contains('e'), s.contains('f'), s.contains('g')])
    }
}

impl ops::Sub<&SSD> for &SSD {
    type Output = SSD;
    fn sub(self, rhs: &SSD) -> Self::Output {
        SSD(self.0.zip(rhs.0).map(|(l, r)| l && !r))
    }
}

impl SSD {
    fn active_digits(&self) -> u8 {
        self.0.iter().map(|&b| b as u8).sum()
    }

    fn to_array(&self) -> [bool; 7] {
        self.0
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
    parsed.iter().flat_map(|(_, output)| output).filter(|&&input| [2, 3, 4, 7].contains(&input.len())).count()
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
            // And c and f are these two (both used in 1).
            // Countrary to the name, these two values are both c_or_f,
            // so we know c and f are these two, but we don’t know which is which.
            let (c, f) = one.to_array().iter().positions(|&b| b).next_tuple().unwrap();
            // 4 uses b, c, d, f, but we already know c and f from 1, so this leaves b and d.
            let (b, d) = (four - one).to_array().iter().positions(|&b| b).next_tuple().unwrap();
            // Now e and g have to be in the remaining two positions.
            let (e, g) = (0..7).filter(|n| ![a, b, c, d, f].contains(n)).next_tuple().unwrap();
            debug_assert_eq!([a, b, c, d, e, f, g].into_iter().sorted().collect_vec(), (0..7).collect_vec());
            // Now there are 8 possible combinations from multiplying the 3 x_or_y we constructed above.
            let mapping = iproduct!([[b, d], [d, b]], [[c, f], [f, c]], [[e, g], [g, e]])
                .map(|([b, d], [c, f], [e, g])| {
                    let mut m = [' '; 7];
                    m[a] = 'a';
                    m[b] = 'b';
                    m[c] = 'c';
                    m[d] = 'd';
                    m[e] = 'e';
                    m[f] = 'f';
                    m[g] = 'g';
                    Mapping(m)
                })
                .find(|m| {
                    raw_input.iter().all(|i| {
                        let translated: String = i.chars().map(|n| m.translate(n)).collect();
                        let ssd = SSD::from(translated.as_ref());
                        VALID_DISPLAYS.iter().any(|d| d == &ssd)
                    })
                })
                .unwrap();
            raw_output
                .iter()
                .map(|i| i.chars().map(|n| mapping.translate(n)).collect::<String>())
                .map(|t| SSD::from(t.as_ref()))
                .map(|ssd| VALID_DISPLAYS.iter().position(|d| &ssd == d).unwrap())
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
