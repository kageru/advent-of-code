#![feature(int_abs_diff)]
#![feature(test)]
extern crate test;
use aoc2021::common::*;

const DAY: usize = 5;
type Coordinate = (i16, i16); // twice as fast as using i64s ¯\_(ツ)_/¯
type Parsed = Vec<(Coordinate, Coordinate)>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines()
        .filter_map(|line| line.split_once(" -> "))
        .filter_map(|(c1, c2)| c1.split_once(',').zip(c2.split_once(',')))
        .map(|((x1, y1), (x2, y2))| ((x1.parse().unwrap(), y1.parse().unwrap()), (x2.parse().unwrap(), y2.parse().unwrap())))
        .collect()
}

fn solve<F: FnMut(&(Coordinate, Coordinate)) -> Vec<Coordinate>>(parsed: &Parsed, f: F) -> usize {
    let max_x = *parsed.iter().flat_map(|((x1, _), (x2, _))| [x1, x2]).max().unwrap() + 2;
    let max_y = *parsed.iter().flat_map(|((_, y1), (_, y2))| [y1, y2]).max().unwrap() + 2;
    let mut counts = vec![vec![0; max_x as _]; max_y as _];
    parsed.iter().flat_map(f).for_each(|(x, y)| counts[x as usize][y as usize] += 1);
    counts.into_iter().flatten().filter(|n| n > &1).count()
}

fn part1(parsed: &Parsed) -> usize {
    solve(parsed, |&cs| match cs {
        ((x1, y1), (x2, y2)) if x1 == x2 => (y1.min(y2)..=y1.max(y2)).map(|y| (x1, y)).collect(),
        ((x1, y1), (x2, y2)) if y1 == y2 => (x1.min(x2)..=x1.max(x2)).map(|x| (x, y1)).collect(),
        _ => vec![],
    })
}

fn part2(parsed: &Parsed) -> usize {
    let offset = |x1, x2| (x1 < x2) as i16 - (x1 > x2) as i16;
    solve(parsed, |&((mut x1, mut y1), (x2, y2))| {
        let mut coords = Vec::with_capacity(x1.abs_diff(x2).max(y1.abs_diff(y2)) as usize + 1);
        let x_offset = offset(x1, x2);
        let y_offset = offset(y1, y2);
        loop {
            coords.push((x1, y1));
            if x1 == x2 && y1 == y2 {
                break;
            }
            x1 += x_offset;
            y1 += y_offset;
        }
        coords
    })
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

    const TEST_INPUT: &str = "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2";

    test!(part1() == 5);
    test!(part2() == 12);
    bench!(part1() == 5280);
    bench!(part2() == 16716);
    bench_input!(Vec::len => 500);
}
