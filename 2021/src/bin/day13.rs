#![feature(test)]
extern crate test;
use aoc2021::common::*;
use itertools::Itertools;

const DAY: usize = 13;
// Turns out the grid is so sparse, a set would have been better. Welp.
type Parsed = (Vec<Vec<bool>>, Vec<Fold>);

enum Fold {
    X(usize),
    Y(usize),
}

impl Fold {
    fn fold(&self, grid: &mut Vec<Vec<bool>>) {
        match *self {
            Fold::Y(at) => {
                for x in 0..at {
                    for y in 0..grid[x].len() {
                        grid[x][y] |= grid.get(at + at - x).map(|ys| ys[y]).unwrap_or(false);
                    }
                }
                grid.truncate(at);
            }
            Fold::X(at) => {
                for ys in grid {
                    for y in 0..at {
                        ys[y] |= *ys.get(at + at - y).unwrap_or(&false);
                    }
                    ys.truncate(at);
                }
            }
        }
    }
}

fn parse_input(raw: &str) -> Parsed {
    let (points, folds) = raw.split_once("\n\n").unwrap();
    let points: Vec<(usize, usize)> =
        points.lines().map(|line| line.split_once(',').unwrap()).map(|(x, y)| (parse_num(x), parse_num(y))).collect();
    let mut grid = vec![vec![false; points.iter().map(|&(x, _)| x).max().unwrap() + 1]; points.iter().map(|&(_, y)| y).max().unwrap() + 1];
    for (y, x) in points {
        grid[x][y] = true;
    }
    let folds = folds
        .lines()
        .map(|line| {
            if let Some(x) = line.strip_prefix("fold along x=") {
                Fold::X(parse_num(x))
            } else if let Some(y) = line.strip_prefix("fold along y=") {
                Fold::Y(parse_num(y))
            } else {
                unreachable!("{}", line)
            }
        })
        .collect();
    (grid, folds)
}

fn part1((grid, folds): &Parsed) -> usize {
    let mut folded = grid.to_owned();
    folds[0].fold(&mut folded);
    folded.into_iter().flatten().filter(|&b| b).count()
}

fn part2((grid, instructions): &Parsed) -> String {
    let mut paper = grid.to_owned();
    for instruction in instructions {
        instruction.fold(&mut paper); // :thanking:
    }
    const OUTPUT_CHARS: [char; 2] = [' ', '#'];
    paper.into_iter().map(|ys| ys.into_iter().map(|b| OUTPUT_CHARS[b as usize]).collect::<String>()).join("\n")
}

fn main() {
    let input = parse_input(&read_file(DAY));
    println!("Part 1: {}", part1(&input));
    println!("Part 2:\n{}", part2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2021::*;

    const TEST_INPUT: &str = "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5";

    test!(part1() == 17);
    bench!(part1() == 661);
    bench_input!(input_len_for_bench => 902);

    #[bench]
    fn part2_bench(b: &mut test::Bencher) {
        let parsed = parse_input(&read_file(DAY));
        b.iter(|| assert_eq!(part2(&parsed).len(), 245));
    }

    fn input_len_for_bench((grid, folds): &Parsed) -> usize {
        grid.len() + folds.len()
    }
}
