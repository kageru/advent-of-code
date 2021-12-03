#![feature(array_windows)]
#![feature(test)]
extern crate test;
use aoc2021::common::{parse_nums, *};

const DAY: usize = 1;
type Parsed = Vec<usize>;

fn parse_input(raw: &str) -> Parsed {
    parse_nums(raw)
}

fn part1(parsed: &Parsed) -> usize {
    parsed.array_windows().filter(|[a, b]| a < b).count()
}

fn part2(parsed: &Parsed) -> usize {
    parsed.array_windows().filter(|[a, _, _, b]| a < b).count()
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

    const TEST_INPUT: &str = "199
200
208
210
200
207
240
269
260
263";

    test!(part1() == 7);
    test!(part2() == 5);
    bench!(part1() == 1316);
    bench!(part2() == 1344);
    bench_input!(Vec::len => 2000);
}
