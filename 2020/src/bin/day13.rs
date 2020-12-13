#![feature(test, destructuring_assignment)]
extern crate test;
use aoc2020::common::*;
use itertools::Itertools;

// Port of the rosetta code Python implementation
fn chinese_remainder(divs: Vec<i64>, rems: Vec<i64>) -> i64 {
    fn mul_inv(mut a: i64, mut b: i64) -> i64 {
        if b == 1 {
            return 1;
        }
        let (mut x0, mut x1, b0) = (0, 1, b);
        while a > 1 {
            let q = a / b;
            (a, b) = (b, a % b);
            (x0, x1) = (x1 - q * x0, x0);
        }
        if x1 < 0 {
            x1 += b0;
        }
        return x1;
    }
    let mut sum = 0;
    let prod: i64 = divs.iter().product();
    for (div, rem) in divs.iter().zip(rems.iter()) {
        let p = prod / div;
        sum += rem * mul_inv(p, *div) * p;
    }
    return sum % prod;
}

type Parsed = (i64, Vec<Option<i64>>);

fn read_input() -> String {
    read_file(13)
}

fn parse_input(raw: &str) -> Parsed {
    let (first, second) = raw.lines().next_tuple().unwrap();
    (first.parse().unwrap(), second.split(',').map(|n| n.parse().ok()).collect())
}

fn part1((start, nums): &Parsed) -> i64 {
    let nums = nums.iter().filter_map(|&n| n).collect_vec();
    let (eta, line) = (*start..).find_map(|t| nums.iter().find(|&n| t % n == 0).map(|n| (t, n))).unwrap();
    (eta - start) * line
}

fn part2((_, lines): &Parsed) -> i64 {
    let rems = lines.iter().enumerate().filter_map(|(n, l)| l.map(|l| l - n as i64)).collect_vec();
    let lines = lines.iter().filter_map(|&l| l).collect_vec();
    chinese_remainder(lines, rems)
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

    const TEST_INPUT: &str = "939
7,13,x,x,59,x,31,19";

    #[test]
    fn chinese_remainder_test() {
        let divs = vec![3, 5, 7];
        let rems = vec![2, 3, 2];
        assert_eq!(chinese_remainder(divs, rems), 23);
    }

    test!(part1() == 295);
    test!(part2() == 1068781);
    bench!(part1() == 102);
    bench!(part2() == 327300950120029);
}
