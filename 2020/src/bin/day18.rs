#![feature(test)]
extern crate test;
use aoc2020::common::*;
use itertools::Itertools;
use std::collections::VecDeque;

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
    parsed.iter().map(|l| build_stack(l, execute_stack_p1)).sum()
}

fn execute_stack_p1(mut stack: VecDeque<Operation>) -> usize {
    while stack.len() > 2 {
        let drained = stack.drain(..3).collect_vec();
        match drained[..] {
            [Operation::Number(x), Operation::Add, Operation::Number(y)] => stack.push_front(Operation::Number(x + y)),
            [Operation::Number(x), Operation::Multiply, Operation::Number(y)] => stack.push_front(Operation::Number(x * y)),
            _ => unreachable!("Invalid stack: {:?}", drained),
        }
    }
    if let Operation::Number(n) = stack[0] {
        n
    } else {
        unreachable!()
    }
}

fn execute_stack_p2(mut stack: VecDeque<Operation>) -> usize {
    while let Some(i) = stack.iter().position(|o| o == &Operation::Add) {
        let drained = stack.drain(i - 1..=i + 1).collect_vec();
        match drained[..] {
            [Operation::Number(x), Operation::Add, Operation::Number(y)] => stack.insert(i - 1, Operation::Number(x + y)),
            _ => unreachable!("Invalid stack: {:?}", drained),
        }
    }
    while stack.len() > 2 {
        let drained = stack.drain(..3).collect_vec();
        match drained[..] {
            [Operation::Number(x), Operation::Multiply, Operation::Number(y)] => stack.push_front(Operation::Number(x * y)),
            _ => unreachable!("Invalid stack: {:?}", drained),
        }
    }
    if let Operation::Number(n) = stack[0] {
        n
    } else {
        unreachable!()
    }
}

fn build_stack<F: FnOnce(VecDeque<Operation>) -> usize + Copy>(ops: &[Operation], execute_stack: F) -> usize {
    let mut stack = VecDeque::new();
    let mut parens = 0;
    for i in 0..ops.len() {
        match ops[i] {
            Operation::OpenParen if parens > 0 => parens += 1,
            #[rustfmt::skip]
            Operation::CloseParen => if parens > 0 { parens -= 1 } else { break },
            // skip the entries that one of the recursive children is already processing
            _ if parens > 0 => continue,
            Operation::Add => stack.push_back(ops[i]),
            Operation::Multiply => stack.push_back(ops[i]),
            Operation::Number(n) => stack.push_back(Operation::Number(n)),
            Operation::OpenParen => {
                stack.push_back(Operation::Number(build_stack(&ops[i + 1..], execute_stack)));
                parens += 1;
            }
        }
    }
    execute_stack(stack)
}

fn part2(parsed: &Parsed) -> usize {
    parsed.iter().map(|l| build_stack(l, execute_stack_p2)).sum()
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
    test!(part2() == 23340);
    bench!(part1() == 510009915468);
    bench!(part2() == 321176691637769);
    bench_input!(len == 375);
}
