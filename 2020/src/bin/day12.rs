#![feature(test)]
#![allow(clippy::ptr_arg)]
extern crate test;
use aoc2020::{common::*, grid::*};

type Parsed = Vec<(char, i64)>;

fn read_input() -> String {
    read_file(12)
}

fn parse_input(raw: &str) -> Parsed {
    raw.lines()
        .map(|l| (l.bytes().next().unwrap() as char, l[1..].parse().unwrap()))
        .collect()
}

fn part1(parsed: &Parsed) -> i64 {
    let end_pos = parsed
        .iter()
        .fold((Direction::Right, PositionND::from([0, 0])), |(dir, pos), (cmd, dist)| match cmd {
            'N' => (dir, pos + PositionND::from([0, *dist])),
            'S' => (dir, pos - PositionND::from([0, *dist])),
            'E' => (dir, pos + PositionND::from([*dist, 0])),
            'W' => (dir, pos - PositionND::from([*dist, 0])),
            'R' => (dir + (dist / 90) as i8, pos),
            'L' => (dir + -(dist / 90) as i8, pos),
            'F' => (dir, pos + PositionND::from(dir) * *dist),
            _ => unreachable!(),
        })
        .1;
    end_pos.points[0].abs() + end_pos.points[1].abs()
}

fn rotate_waypoint(mut wp: PositionND<2>, deg: i64) -> PositionND<2> {
    for _ in 0..((deg / 90 + 4) & 3) {
        wp = PositionND::from([wp.points[1], -wp.points[0]]);
    }
    wp
}

fn part2(parsed: &Parsed) -> i64 {
    let end_pos = parsed
        .iter()
        .fold(
            (PositionND::from([10, 1]), PositionND::from([0, 0])),
            |(wp, pos), (cmd, dist)| match cmd {
                'N' => (wp + PositionND::from([0, *dist]), pos),
                'S' => (wp - PositionND::from([0, *dist]), pos),
                'E' => (wp + PositionND::from([*dist, 0]), pos),
                'W' => (wp - PositionND::from([*dist, 0]), pos),
                'R' => (rotate_waypoint(wp, *dist), pos),
                'L' => (rotate_waypoint(wp, -dist), pos),
                'F' => (wp, pos + wp * *dist),
                _ => unreachable!(),
            },
        )
        .1;
    end_pos.points[0].abs() + end_pos.points[1].abs()
}

fn main() {
    let input = parse_input(&read_input());
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2020::*;
    use paste::paste;
    use test::black_box;

    const TEST_INPUT: &str = "F10
N3
F7
R90
F11";

    test!(part1() == 25);
    test!(part2() == 286);
    bench!(part1() == 1838);
    bench!(part2() == 89936);
    bench_input!(len == 786);
}
