use itertools::Itertools;
use std::io;
use std::io::BufRead;

#[derive(Debug)]
enum Operation {
    Add { x: i32, y: i32, addr: usize },
    Multiply { x: i32, y: i32, addr: usize },
    Input { value: i32, addr: usize },
    Output { value: i32 },
    JumpIfTrue { value: i32, addr: i32 },
    JumpIfFalse { value: i32, addr: i32 },
    LessThan { first: i32, second: i32, addr: i32 },
    Equals { first: i32, second: i32, addr: i32 },
}

#[derive(Debug)]
enum Mode {
    Immediate,
    Position,
}

enum ParameterPosition {
    First,
    Second,
}

impl Into<usize> for ParameterPosition {
    fn into(self) -> usize {
        match self {
            ParameterPosition::First => 2,
            ParameterPosition::Second => 3,
        }
    }
}

impl Into<Mode> for &char {
    fn into(self) -> Mode {
        match self {
            '0' => Mode::Position,
            '1' => Mode::Immediate,
            _ => unreachable!(),
        }
    }
}

fn get_next(input: &[i32], pos: &mut i32, mode: Mode) -> i32 {
    let value = input[*pos as usize];
    let next = match mode {
        Mode::Position => input[value as usize],
        Mode::Immediate => value,
    };
    *pos += 1;
    next
}

fn get_mode(raw_opcode: &[char], pos: ParameterPosition) -> Mode {
    raw_opcode.get::<usize>(pos.into()).unwrap_or(&'0').into()
}

fn next_operation(
    input: &[i32],
    pos: &mut i32,
    inputs: &mut dyn Iterator<Item = i32>,
) -> Option<Operation> {
    let next = get_next(input, pos, Mode::Immediate);
    let mut raw_opcode: Vec<_> = next.to_string().chars().collect();
    raw_opcode.reverse();
    match raw_opcode[0] {
        '1' => Some(Operation::Add {
            x: get_next(input, pos, get_mode(&raw_opcode, ParameterPosition::First)),
            y: get_next(input, pos, get_mode(&raw_opcode, ParameterPosition::Second)),
            addr: get_next(input, pos, Mode::Immediate) as usize,
        }),
        '2' => Some(Operation::Multiply {
            x: get_next(input, pos, get_mode(&raw_opcode, ParameterPosition::First)),
            y: get_next(input, pos, get_mode(&raw_opcode, ParameterPosition::Second)),
            addr: get_next(input, pos, Mode::Immediate) as usize,
        }),
        '3' => Some(Operation::Input {
            value: inputs.next().unwrap(),
            addr: get_next(input, pos, Mode::Immediate) as usize,
        }),
        '4' => Some(Operation::Output {
            value: get_next(input, pos, get_mode(&raw_opcode, ParameterPosition::First)),
        }),
        '5' => Some(Operation::JumpIfTrue {
            value: get_next(input, pos, get_mode(&raw_opcode, ParameterPosition::First)),
            addr: get_next(input, pos, get_mode(&raw_opcode, ParameterPosition::Second)),
        }),
        '6' => Some(Operation::JumpIfFalse {
            value: get_next(input, pos, get_mode(&raw_opcode, ParameterPosition::First)),
            addr: get_next(input, pos, get_mode(&raw_opcode, ParameterPosition::Second)),
        }),
        '7' => Some(Operation::LessThan {
            first: get_next(input, pos, get_mode(&raw_opcode, ParameterPosition::First)),
            second: get_next(input, pos, get_mode(&raw_opcode, ParameterPosition::Second)),
            addr: get_next(input, pos, Mode::Immediate),
        }),
        '8' => Some(Operation::Equals {
            first: get_next(input, pos, get_mode(&raw_opcode, ParameterPosition::First)),
            second: get_next(input, pos, get_mode(&raw_opcode, ParameterPosition::Second)),
            addr: get_next(input, pos, Mode::Immediate),
        }),
        '9' => None,
        _ => unreachable!(),
    }
}

fn execute(op: Operation, input: &mut Vec<i32>, pos: &mut i32) -> Option<i32> {
    match op {
        Operation::Add { x, y, addr } => {
            input[addr as usize] = x + y;
            None
        }
        Operation::Multiply { x, y, addr } => {
            input[addr as usize] = x * y;
            None
        }
        Operation::Input { value, addr } => {
            input[addr] = value;
            None
        }
        Operation::Output { value } => Some(value),
        Operation::JumpIfTrue { value, addr } => {
            if value != 0 {
                *pos = addr
            }
            None
        }
        Operation::JumpIfFalse { value, addr } => {
            if value == 0 {
                *pos = addr
            }
            None
        }
        Operation::LessThan {
            first,
            second,
            addr,
        } => {
            input[addr as usize] = (first < second) as i32;
            None
        }
        Operation::Equals {
            first,
            second,
            addr,
        } => {
            input[addr as usize] = (first == second) as i32;
            None
        }
    }
}

pub fn main() {
    let input: Vec<i32> = io::stdin()
        .lock()
        .lines()
        .next()
        .unwrap()
        .unwrap()
        .split(',')
        .map(|n| n.parse().unwrap())
        .collect();

    println!("Part 1:");
    for amps in (0..5).permutations(5) {
        let mut last_output = 0;
        for amp in amps {
            //for amp in 0..5 {
            let mut pos = 0;
            let mut part1_input = input.clone();
            let mut inputs = vec![amp, last_output].into_iter();
            while let Some(op) = next_operation(&part1_input, &mut pos, &mut inputs) {
                if let Some(o) = execute(op, &mut part1_input, &mut pos) {
                    last_output = o;
                    break;
                }
            }
            println!("{}", last_output);
        }
    }
}
