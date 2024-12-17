#![feature(test)]
extern crate test;
use std::array;

use aoc2024::{boilerplate, common::*};

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
        0..=3 => operand,
        4 => registers[0],
        5 => registers[1],
        6 => registers[2],
        _ => unreachable!(),
    }
}

fn div(registers: &Registers, operand: I) -> I {
    registers[0] / 2usize.pow(combo(operand, registers) as u32)
}

fn step(opcode: I, operand: I, registers: &mut Registers, out: &mut I) -> Option<I> {
    match opcode {
        0 => registers[0] = div(registers, operand),
        1 => registers[1] ^= operand,
        2 => registers[1] = combo(operand, registers) & 0b111,
        3 if registers[0] != 0 => return Some(operand),
        3 => (),
        4 => registers[1] ^= registers[2],
        5 => *out = *out * 10 + (combo(operand, registers) & 0b111),
        6 => registers[1] = div(registers, operand),
        7 => registers[2] = div(registers, operand),
        _ => unreachable!(),
    };
    None
}

fn run(registers: &mut Registers, ops: &Vec<I>) -> I {
    let mut ins = 0;
    let mut out = 0;
    loop {
        if ins + 1 >= ops.len() {
            break;
        }
        match step(ops[ins], ops[ins + 1], registers, &mut out) {
            Some(jmp) => ins = jmp,
            None => ins += 2,
        }
    }
    out
}

fn part1((registers, ops): &Parsed) -> I {
    run(&mut registers.clone(), ops)
}

fn part2(([_, b, c], ops): &Parsed) -> usize {
    let target = ops.iter().fold(0, |acc, n| acc * 10 + n);
    println!("Target : {target}");
    (0..)
        .inspect(|i| if i & 16777215 == 16777215 { println!("{i}") })
        .find(|&i| run(&mut [i, *b, *c], ops) == target).unwrap()
}

#[cfg(test)]
mod tests2 {
    use super::*;

    #[test]
    fn example_test_cases() {
        for (ops, mut registers, expected_registers, expected_out) in [
            // If register C contains 9, the program 2,6 would set register B to 1.
            (vec![2, 6], [0, 0, 9], [0, 1, 9], 0),
            // If register A contains 10, the program 5,0,5,1,5,4 would output 0,1,2.
            (vec![5, 0, 5, 1, 5, 4], [10, 0, 0], [10, 0, 0], 12),
            // If register A contains 2024, the program 0,1,5,4,3,0 would output 4,2,5,6,7,7,7,7,3,1,0 and leave 0 in register A.
            (vec![0, 1, 5, 4, 3, 0], [2024, 0, 0], [0, 0, 0], 42567777310),
            // If register B contains 29, the program 1,7 would set register B to 26.
            (vec![1, 7], [0, 26, 0], [0, 29, 0], 0),
            // If register B contains 2024 and register C contains 43690, the program 4,0 would set register B to 44354.
            (vec![4, 0], [0, 2024, 43690], [0, 44354, 43690], 0),
        ] {
            println!("Testing {ops:?}");
            let out = run(&mut registers, &ops);
            assert_eq!(registers, expected_registers);
            assert_eq!(out, expected_out);
        }
    }
}

boilerplate! {
    TEST_INPUT == "\
Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0"
    for tests: {
        part1: { TEST_INPUT => 4635635210 },
        part2: { "\
Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0" => 117440 },
    },
    bench1 == 210172503,
    bench2 == 0,
    bench_parse: |(_, ops): &Parsed| ops.len() => 16,
}
