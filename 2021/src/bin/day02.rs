#![feature(test)]
extern crate test;
use aoc2021::common::*;

const DAY: usize = 2;
type Parsed = Vec<Movement>;

#[derive(Debug, Clone, Copy)]
enum Movement {
    Up(i64),
    Down(i64),
    Forward(i64),
}

fn parse_input(raw: &str) -> Parsed {
    raw.lines()
        .filter_map(|l| l.split_once(' '))
        .map(|(dir, dist)| match dir {
            "up" => Movement::Up(dist.parse().unwrap()),
            "down" => Movement::Down(dist.parse().unwrap()),
            "forward" => Movement::Forward(dist.parse().unwrap()),
            _ => unreachable!(),
        })
        .collect()
}

fn part1(parsed: &Parsed) -> i64 {
    let (depth, distance) = parsed.iter().fold((0, 0), |(depth, distance), &mov| match mov {
        Movement::Up(x) => (depth - x, distance),
        Movement::Down(x) => (depth + x, distance),
        Movement::Forward(x) => (depth, distance + x),
    });
    depth * distance
}

fn part2(parsed: &Parsed) -> i64 {
    let (depth, distance, _) = parsed.iter().fold((0, 0, 0), |(depth, distance, aim), &mov| match mov {
        Movement::Up(x) => (depth, distance, aim - x),
        Movement::Down(x) => (depth, distance, aim + x),
        Movement::Forward(x) => (depth + aim * x, distance + x, aim),
    });
    depth * distance
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

    const TEST_INPUT: &str = "forward 5
down 5
forward 8
up 3
down 8
forward 2";

    test!(part1() == 150);
    test!(part2() == 900);
    bench!(part1() == 1698735);
    bench!(part2() == 1594785890);
    bench_input!(len == 1000);
}
