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

fn get_next(input: &Vec<i32>, pos: &mut i32, mode: Mode) -> i32 {
    let value = input[*pos as usize];
    let next = match mode {
        Mode::Position => input[value as usize],
        Mode::Immediate => value,
    };
    *pos += 1;
    next
}

fn get_mode(raw_opcode: &Vec<char>, pos: ParameterPosition) -> Mode {
    raw_opcode.get::<usize>(pos.into()).unwrap_or(&'0').into()
}

fn next_operation(input: &Vec<i32>, pos: &mut i32, inp: i32) -> Option<Operation> {
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
            value: inp,
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

fn execute(op: Operation, input: &mut Vec<i32>, pos: &mut i32) {
    match op {
        Operation::Add { x, y, addr } => input[addr as usize] = x + y,
        Operation::Multiply { x, y, addr } => input[addr as usize] = x * y,
        Operation::Input { value, addr } => input[addr] = value,
        Operation::Output { value } => {
            if value != 0 {
                println!("{}", value)
            }
        }
        Operation::JumpIfTrue { value, addr } => {
            if value != 0 {
                *pos = addr
            }
        }
        Operation::JumpIfFalse { value, addr } => {
            if value == 0 {
                *pos = addr
            }
        }
        Operation::LessThan {
            first,
            second,
            addr,
        } => input[addr as usize] = (first < second) as i32,
        Operation::Equals {
            first,
            second,
            addr,
        } => input[addr as usize] = (first == second) as i32,
    }
}

pub fn main() {
    let mut input: Vec<i32> = io::stdin()
        .lock()
        .lines()
        .next()
        .unwrap()
        .unwrap()
        .split(",")
        .map(|n| n.parse().unwrap())
        .collect();
    let mut pos = 0;
    let mut part2_input = input.clone();

    println!("Part 1:");
    while let Some(op) = next_operation(&input, &mut pos, 1) {
        execute(op, &mut input, &mut pos);
    }

    pos = 0;
    println!("Part 2:");
    while let Some(op) = next_operation(&part2_input, &mut pos, 5) {
        execute(op, &mut part2_input, &mut pos);
    }
}
