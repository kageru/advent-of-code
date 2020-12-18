#![feature(test)]
extern crate test;
use aoc2020::common::*;
use itertools::Itertools;

type Parsed = Vec<Vec<Operation>>;

#[derive(Debug, Copy, Clone, PartialEq)]
enum Operation {
    Number(usize),
    Multiply,
    Add,
    OpenParen,
    CloseParen,
}

fn read_input() -> String {
    read_file(18)
}

fn parse_input(raw: &str) -> Parsed {
    raw.lines()
        .map(|l| {
            l.bytes()
                .filter(|&b| b != b' ')
                .map(|op| match op {
                    b'+' => Operation::Add,
                    b'*' => Operation::Multiply,
                    b'(' => Operation::OpenParen,
                    b')' => Operation::CloseParen,
                    n => Operation::Number((n - 48) as usize),
                })
                .collect()
        })
        .collect()
}

fn part1(parsed: &Parsed) -> usize {
    parsed.iter().map(|l| p1_rec(l)).sum()
}

fn p1_rec(ops: &[Operation]) -> usize {
    // dbg!(ops);
    let mut stack = vec![];
    let mut parens = 0;
    for i in 0..ops.len() {
        match ops[i] {
            Operation::OpenParen if parens > 0 => {
                parens += 1;
            }
            Operation::CloseParen if parens > 0 => {
                parens -= 1;
            }
            _ if parens > 0 => continue,
            Operation::Add => stack.push(ops[i]),
            Operation::Multiply => stack.push(ops[i]),
            Operation::Number(n) => stack.push(Operation::Number(n)),
            Operation::OpenParen => {
                stack.push(Operation::Number(p1_rec(&ops[i + 1..])));
                parens += 1;
            }
            Operation::CloseParen => break,
        }
        while stack.len() > 2 {
            let drained = stack.drain(..3).collect_vec();
            // dbg!(&drained, parens);
            match drained[..] {
                [Operation::Number(x), Operation::Add, Operation::Number(y)] => stack.push(Operation::Number(x + y)),
                [Operation::Number(x), Operation::Multiply, Operation::Number(y)] => stack.push(Operation::Number(x * y)),
                _ => unreachable!("Invalid stack: {:?} of size {}", drained, drained.len()),
            }
        }
    }
    if let Operation::Number(n) = stack[0] {
        n
    } else {
        unreachable!()
    }
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
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

    const TEST_INPUT: &str = "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2";

    test!(part1() == 13632);
    //test!(part2() == 0);
    //bench!(part1() == 0);
    //bench!(part2() == 0);
    //bench_input!(len == 0);
}
