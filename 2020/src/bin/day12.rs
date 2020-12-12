#![feature(test)]
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
        .fold((Direction::Right, Position2D { x: 0, y: 0 }), |(dir, pos), (cmd, dist)| match cmd {
            'N' => (dir, pos + Position2D::from((0, *dist))),
            'S' => (dir, pos - Position2D::from((0, *dist))),
            'E' => (dir, pos + Position2D::from((*dist, 0))),
            'W' => (dir, pos - Position2D::from((*dist, 0))),
            'R' => (dir + (dist / 90) as i8, pos),
            'L' => (dir + -(dist / 90) as i8, pos),
            'F' => (dir, pos + Position2D::from(dir) * *dist),
            _ => unreachable!(),
        })
        .1;
    end_pos.x.abs() + end_pos.y.abs()
}

fn rotate_waypoint(mut wp: Position2D, deg: i64) -> Position2D {
    for _ in 0..((deg / 90 + 4) % 4) {
        wp = Position2D { x: wp.y, y: -wp.x };
    }
    wp
}

fn part2(parsed: &Parsed) -> i64 {
    let end_pos = parsed
        .iter()
        .fold(
            (Position2D { x: 10, y: 1 }, Position2D { x: 0, y: 0 }),
            |(wp, pos), (cmd, dist)| match cmd {
                'N' => (wp + Position2D::from((0, *dist)), pos),
                'S' => (wp - Position2D::from((0, *dist)), pos),
                'E' => (wp + Position2D::from((*dist, 0)), pos),
                'W' => (wp - Position2D::from((*dist, 0)), pos),
                'R' => (rotate_waypoint(wp, *dist), pos),
                'L' => (rotate_waypoint(wp, -dist), pos),
                'F' => (wp, pos + wp * *dist),
                _ => unreachable!(),
            },
        )
        .1;
    end_pos.x.abs() + end_pos.y.abs()
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
