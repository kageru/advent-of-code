#![feature(array_from_fn)]
#![feature(array_zip)]
#![feature(test)]
extern crate test;
use aoc2021::common::*;
use itertools::Itertools;
use std::array;

const DAY: usize = 8;
type Parsed<'a> = Vec<([SSD; 10], [SSD; 4])>;

const VALID_DISPLAYS: [SSD; 10] = [119, 36, 93, 109, 46, 107, 123, 37, 127, 111];

type SSD = u32;

struct Mapping([SSD; 7]);

impl Mapping {
    fn translate(&self, i: SSD) -> SSD {
        1 << self.0[i as usize]
    }
}

fn parse(s: &str) -> SSD {
    ['g', 'f', 'e', 'd', 'c', 'b', 'a'].iter().map(|&c| s.contains(c)).fold(0, |acc, b| (acc | (b as SSD)) << 1) >> 1
}

fn bit_at(x: SSD, n: SSD) -> bool {
    (x >> n) & 1 != 0
}

fn difference(lhs: SSD, rhs: SSD) -> SSD {
    lhs & !rhs
}

fn parse_input(raw: &str) -> Parsed {
    raw.lines()
        .map(|l| l.split_once(" | ").unwrap())
        .map(|(input, output)| {
            let mut input = input.split(' ').map_into();
            let mut output = output.split(' ').map_into();
            (array::from_fn(|_| parse(input.next().unwrap())), array::from_fn(|_| parse(output.next().unwrap())))
        })
        .collect()
}

fn part1<'a>(parsed: &Parsed<'a>) -> usize {
    parsed.iter().flat_map(|(_, output)| output).filter(|&&input| [2, 3, 4, 7].contains(&input.count_ones())).count()
}

fn part2<'a>(parsed: &Parsed<'a>) -> usize {
    parsed
        .iter()
        .map(|(input, raw_output)| {
            let [&one, &four, &seven] = [2, 4, 3].map(|n| input.iter().find(|s| s.count_ones() == n).unwrap());
            // We know the position of a for sure because it’s the only difference between 7 and 1
            let a = (0..7).position(|n| bit_at(difference(seven, one), n)).unwrap();
            // And c and f are these two (both used in 1).
            // Contrary to the name, these two values are both c_or_f,
            // so we know c and f are these two, but we don’t know which is which.
            let (c, f) = (0..7).positions(|n| bit_at(one, n)).next_tuple().unwrap();
            // 4 uses b, c, d, f, but we already know c and f from 1, so this leaves b and d.
            let (b, d) = (0..7).positions(|n| bit_at(difference(four, one), n)).next_tuple().unwrap();
            // Now e and g have to be in the remaining two positions.
            let (e, g) = (0..7).filter(|n| ![a, b, c, d, f].contains(n)).next_tuple().unwrap();
            // Now there are 8 possible combinations from multiplying the 3 x_or_y we constructed above.
            // This is a manual implementation of itertools::iproduct specialized for 3 small
            // arrays because it’s much faster this way.
            let mapping = [[c, f], [f, c]]
                .into_iter()
                .flat_map(|[c, f]| [[c, f, b, d], [c, f, d, b]])
                .flat_map(|[c, f, b, d]| [[c, f, b, d, e, g], [c, f, b, d, g, e]])
                .map(|[c, f, b, d, e, g]| {
                    let mut m = [0; 7];
                    let mut cur = 0;
                    for i in [a, b, c, d, e, f, g] {
                        m[i] = cur;
                        cur += 1;
                    }
                    Mapping(m)
                })
                .find(|m| input.iter().all(|&i| VALID_DISPLAYS.contains(&(0..7).map(|n| (bit_at(i, n) as SSD) * m.translate(n)).sum())))
                .unwrap();
            raw_output
                .iter()
                .map(|&i| (0..7).map(|n| (bit_at(i, n) as SSD) * mapping.translate(n)).sum())
                .map(|ssd: SSD| VALID_DISPLAYS.iter().position(|d| &ssd == d).unwrap())
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

    #[test]
    fn test_parse() {
        assert_eq!(parse("cgeb"), 86);
    }

    test!(part1() == 26);
    test!(part2() == 61229);
    bench!(part1() == 239);
    bench!(part2() == 946346);
    bench_input!(Vec::len => 200);
}
