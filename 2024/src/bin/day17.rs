#![feature(test)]
extern crate test;
use aoc2024::{boilerplate, common::*};
use itertools::Itertools;
use std::array;

const DAY: usize = 17;
type I = usize;
type Registers = [I; 3];
type Parsed = (Registers, Vec<I>);

fn parse_input(raw: &str) -> Parsed {
    let mut lines = raw.lines();
    let registers = array::from_fn(|_| parse_num(&lines.next().unwrap()[12..]));
    let ops = parse_nums_separator(lines.last().unwrap().trim_start_matches("Program: "), ',');
    (registers, ops)
}

fn combo(operand: I, registers: &Registers) -> I {
    match operand {
        0..=3 => operand % 8,
        4..=6 => registers[operand - 4] % 8,
        _ => unreachable!(),
    }
}

fn div(registers: &Registers, operand: I) -> I {
    registers[0] / 2usize.pow(combo(operand, registers) as u32)
}

fn step(opcode: I, operand: I, registers: &mut Registers, out: &mut Vec<I>) -> Option<I> {
    match opcode {
        0 => registers[0] = div(registers, operand),
        1 => registers[1] ^= operand,
        2 => registers[1] = combo(operand, registers),
        3 if registers[0] != 0 => return Some(operand),
        3 => (),
        4 => registers[1] ^= registers[2],
        5 => out.push(combo(operand, registers)),
        6 => registers[1] = div(registers, operand),
        7 => registers[2] = div(registers, operand),
        _ => unreachable!(),
    };
    None
}

fn run(registers: &mut Registers, ops: &[I]) -> Vec<I> {
    let (mut ins, mut out) = (0, Vec::new());
    while ins + 1 < ops.len() {
        ins = step(ops[ins], ops[ins + 1], registers, &mut out).unwrap_or(ins + 2);
    }
    out
}

fn part1((registers, ops): &Parsed) -> String {
    run(&mut registers.clone(), ops).into_iter().map(|b| (b as u8 + b'0') as char).join(",")
}

// The idea is that every output consumes 3 bits of the initial A value (found that on /g/),
// so we can try 3 bits at a time instead of all 48 bits at once.
// What I don’t understand is why the program behaves this way
// (we only ever write to A in `adv`, but I don’t see why that’s 3 bits per `out` operation),
// but implementing something under that assumption seems to work fine.
fn part2(([_, b, c], ops): &Parsed) -> usize {
    let mut candidates = vec![(0, ops.len() - 1)];
    while let Some((previous_a, start_digit)) = candidates.pop() {
        for a in (0..8).map(|n| (previous_a << 3) + n).filter(|&a| run(&mut [a, *b, *c], ops) == ops[start_digit..]) {
            if start_digit == 0 {
                return a;
            }
            candidates.push((a, start_digit - 1));
        }
    }
    unreachable!()
}

#[cfg(test)]
mod tests2 {
    use super::*;

    #[test]
    fn example_test_cases() {
        for (ops, mut registers, expected_registers, expected_out) in [
            // If register C contains 9, the program 2,6 would set register B to 1.
            (vec![2, 6], [0, 0, 9], [0, 1, 9], vec![]),
            // If register A contains 10, the program 5,0,5,1,5,4 would output 0,1,2.
            (vec![5, 0, 5, 1, 5, 4], [10, 0, 0], [10, 0, 0], vec![0, 1, 2]),
            // If register A contains 2024, the program 0,1,5,4,3,0 would output 4,2,5,6,7,7,7,7,3,1,0 and leave 0 in register A.
            (vec![0, 1, 5, 4, 3, 0], [2024, 0, 0], [0, 0, 0], vec![4, 2, 5, 6, 7, 7, 7, 7, 3, 1, 0]),
            // If register B contains 29, the program 1,7 would set register B to 26.
            (vec![1, 7], [0, 26, 0], [0, 29, 0], vec![]),
            // If register B contains 2024 and register C contains 43690, the program 4,0 would set register B to 44354.
            (vec![4, 0], [0, 2024, 43690], [0, 44354, 43690], vec![]),
        ] {
            println!("Testing {ops:?}");
            let out = run(&mut registers, &ops);
            assert_eq!(registers, expected_registers);
            assert_eq!(out, expected_out);
        }
    }

    #[test]
    fn part1_test() {
        let res = part1(&parse_input(super::tests::TEST_INPUT));
        assert_eq!(res, "4,6,3,5,6,3,5,2,1,0");
    }
}

boilerplate! {
    TEST_INPUT == "\
Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0"
    for tests: {}, // not using these because my macro can’t handle commas in the return value.
    bench1 == "2,1,0,1,7,2,5,0,3",
    bench2 == 267265166222235,
    bench_parse: |(_, ops): &Parsed| ops.len() => 16,
}
