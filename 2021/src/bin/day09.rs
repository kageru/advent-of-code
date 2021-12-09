#![feature(test)]
extern crate test;
use aoc2021::common::*;

const DAY: usize = 09;
type Parsed = Vec<Vec<u8>>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines().map(|l| l.bytes().map(|b| b - b'0').collect()).collect()
}

fn part1(parsed: &Parsed) -> usize {
    (0..parsed.len())
        .flat_map(|x| (0..parsed[x].len()).map(move |y| (x, y)))
        .filter_map(|(x, y)| {
            // There’s gotta be some incomplete_windows or similar that makes this not as horrible
            let cur = parsed[x][y];
            [Step::XP, Step::XM, Step::YP, Step::YM]
                .into_iter()
                .filter_map(|s| try_step(x, y, s))
                .filter_map(|(x, y)| parsed.get(x).and_then(|ys| ys.get(y)))
                .all(|&n| n > cur)
                .then(|| cur as usize + 1)
        })
        .sum()
}

#[rustfmt::skip]
enum Step { XP, XM, YP, YM }

fn try_step(x: usize, y: usize, step: Step) -> Option<(usize, usize)> {
    match step {
        Step::XP => Some((x.checked_add(1)?, y)),
        Step::XM => Some((x.checked_sub(1)?, y)),
        Step::YP => Some((x, y.checked_add(1)?)),
        Step::YM => Some((x, y.checked_sub(1)?)),
    }
}

fn part2(parsed: &Parsed) -> usize {
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
    test!(part2() == 0);
    bench!(part1() == 478);
    bench!(part2() == 0);
    bench_input!(Vec::len => 100);
}
