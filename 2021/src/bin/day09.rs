#![feature(test)]
extern crate test;
use std::collections::HashSet;

use aoc2021::common::*;

const DAY: usize = 9;
type Parsed = Vec<Vec<u8>>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines().map(|l| l.bytes().map(|b| b - b'0').collect()).collect()
}

fn part1(parsed: &Parsed) -> usize {
    find_lows(parsed).into_iter().map(|(x, y)| parsed[x][y] as usize + 1).sum()
}

fn find_lows(parsed: &Parsed) -> Vec<(usize, usize)> {
    (0..parsed.len())
        .flat_map(|x| (0..parsed[x].len()).map(move |y| (x, y)))
        .filter(|&(x, y)| {
            // There’s gotta be some incomplete_windows or similar that makes this not as horrible
            let cur = parsed[x][y];
            all_neighbors(x, y).into_iter().filter_map(|(x, y)| parsed.get(x).and_then(|ys| ys.get(y))).all(|&n| n > cur)
        })
        .collect()
}

fn all_neighbors(x: usize, y: usize) -> Vec<(usize, usize)> {
    [x.checked_add(1).map(|x| (x, y)), x.checked_sub(1).map(|x| (x, y)), y.checked_add(1).map(|y| (x, y)), y.checked_sub(1).map(|y| (x, y))]
        .into_iter()
        .flatten()
        .collect()
}

fn grow_basin(parsed: &Parsed, points_in_basin: &mut HashSet<(usize, usize)>, (x, y): (usize, usize)) -> bool {
    let cur = parsed[x][y];
    let mut new_points = Vec::new();
    for (x, y) in all_neighbors(x, y).into_iter().filter(|p| !points_in_basin.contains(p)) {
        if parsed.get(x).and_then(|ys| ys.get(y)).unwrap_or(&0) > &cur {
            new_points.push((x, y));
        }
    }
    if new_points.iter().any(|&p| grow_basin(parsed, points_in_basin, p)) {
        points_in_basin.insert((x, y));
        true
    } else {
        false
    }
}

fn part2(parsed: &Parsed) -> usize {
    let lows = find_lows(parsed);
    let mut basins = Vec::new();
    for (x, y) in lows {
        let mut points_in_basin = HashSet::new();
        grow_basin(parsed, &mut points_in_basin, (x, y));
        basins.push(points_in_basin);
    }
    basins.sort_unstable_by_key(HashSet::len);
    basins.reverse();
    println!("{basins:?}");
    // basins.iter().take(3).map(|b| b.len()).product();
    unimplemented!()
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

    const TEST_INPUT: &str = "2199943210
3987894921
9856789892
8767896789
9899965678";

    test!(part1() == 15);
    // test!(part2() == 1134);
    bench!(part1() == 478);
    // bench!(part2() == 0);
    bench_input!(Vec::len => 100);
}
