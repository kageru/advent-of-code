#![feature(array_from_fn)]
#![feature(array_zip)]
#![feature(test)]
extern crate test;
use aoc2021::common::*;
use itertools::Itertools;
use std::array;

const DAY: usize = 8;
type Parsed = Vec<([Ssd; 10], [Ssd; 4])>;

const VALID_DISPLAYS: [Ssd; 10] = [119, 36, 93, 109, 46, 107, 123, 37, 127, 111];

type Ssd = u32;

struct Mapping([Ssd; 7]);

impl Mapping {
    fn translate(&self, i: usize) -> Ssd {
        1 << self.0[i]
    }
}

const INPUT_MASK: [Ssd; 8] = [1, 2, 4, 8, 16, 32, 64, 128];

fn parse(s: &str) -> Ssd {
    s.bytes().map(|b| INPUT_MASK[(b - b'a') as usize]).sum()
}

fn bit_at(x: Ssd, n: usize) -> bool {
    (x >> n) & 1 != 0
}

fn difference(lhs: Ssd, rhs: Ssd) -> Ssd {
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

fn part1(parsed: &Parsed) -> usize {
    parsed.iter().flat_map(|(_, output)| output).filter(|input| [2, 3, 4, 7].contains(&input.count_ones())).count()
}

fn part2(parsed: &Parsed) -> usize {
    parsed
        .iter()
        .map(|(input, output)| {
            let [&one, &four, &seven] = [2, 4, 3].map(|n| input.iter().find(|s| s.count_ones() == n).unwrap());
            // We know the position of a for sure because it’s the only difference between 7 and 1
            let a = (0..7).position(|n| bit_at(difference(seven, one), n)).unwrap();
            // And c and f are these two (both used in 1).
            let (c, f) = (0..7).positions(|n| bit_at(one, n)).next_tuple().unwrap();
            // Determine which is which by their frequency in the input.
            let (c, f) = if input.iter().filter(|&&i| bit_at(i, c)).count() == 8 { (c, f) } else { (f, c) };
            // 4 uses b, c, d, f, but we already know c and f from 1, so this leaves b and d.
            let (b, d) = (0..7).positions(|n| bit_at(difference(four, one), n)).next_tuple().unwrap();
            let (b, d) = if input.iter().filter(|&&i| bit_at(i, b)).count() == 6 { (b, d) } else { (d, b) };
            // Now e and g have to be in the remaining two positions.
            let (e, g) = (0..7).filter(|n| ![a, b, c, d, f].contains(n)).map_into().next_tuple().unwrap();
            let (e, g) = if input.iter().filter(|&&i| bit_at(i, e)).count() == 4 { (e, g) } else { (g, e) };
            let mut m = [0; 7];
            let mut cur = 0;
            #[allow(clippy::explicit_counter_loop)] // it’s faster this way
            for i in [a, b, c, d, e, f, g] {
                // We know they’re all in range, and this is actually a few % faster.
                unsafe { *m.get_unchecked_mut(i) = cur };
                cur += 1;
            }
            let mapping = Mapping(m);
            output
                .iter()
                .map(|&i| (0..7).map(|n| (bit_at(i, n) as Ssd) * mapping.translate(n)).sum())
                .map(|ssd: Ssd| VALID_DISPLAYS.iter().position(|d| &ssd == d).unwrap())
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
