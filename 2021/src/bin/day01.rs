#![feature(array_windows)]
#![feature(test)]
extern crate test;
use aoc2021::common::{parse_nums, *};

type Parsed = Vec<usize>;

fn read_input() -> String {
    read_file(01)
}

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
    let input = parse_input(&read_input());
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2021::*;
    use paste::paste;
    use test::black_box;

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
    bench_input!(len == 2000);
}
