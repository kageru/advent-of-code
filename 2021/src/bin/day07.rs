#![feature(int_abs_diff)]
#![feature(test)]
extern crate test;
use aoc2021::common::*;
use itertools::{Itertools, MinMaxResult};

const DAY: usize = 7;
type Parsed = Vec<usize>;

fn parse_input(raw: &str) -> Parsed {
    parse_nums_comma(raw)
}

fn min_max(parsed: &Vec<usize>) -> (usize, usize) {
    match parsed.iter().minmax() {
        MinMaxResult::MinMax(&min, &max) => (min, max),
        _ => unreachable!("List not long enough"),
    }
}

fn part1(parsed: &Parsed) -> usize {
    let (min, max) = min_max(parsed);
    (min..=max).map(|x| parsed.iter().map(|p| p.abs_diff(x)).sum()).min().unwrap()
}

fn sum_to(n: usize) -> usize {
    (1..=n).sum()
}

fn part2(parsed: &Parsed) -> usize {
    let (min, max) = min_max(parsed);
    (min..=max).map(|x| parsed.iter().map(|p| p.abs_diff(x)).map(sum_to).sum()).min().unwrap()
}

fn main() {
    let input = parse_input(&read_file(DAY));
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2021::*;

    const TEST_INPUT: &str = "16,1,2,0,4,2,7,1,2,14";

    test!(part1() == 37);
    test!(part2() == 168);
    bench!(part1() == 328318);
    bench!(part2() == 89791146);
    bench_input!(Vec::len => 1000);

    #[bench]
    fn bench_sum_to(b: &mut test::Bencher) {
        b.iter(|| assert_eq!(sum_to(test::black_box(1000000)), 500000500000))
    }
}
