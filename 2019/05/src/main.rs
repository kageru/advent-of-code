use std::io;
use std::io::BufRead;

#[derive(Debug)]
enum Operation {
    Add { x: i32, y: i32, addr: usize },
    Multiply { x: i32, y: i32, addr: usize },
    Input { value: i32, addr: usize },
    Output { value: i32 },
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
    //dbg!(&mode, &pos, value);
    let next = match mode {
        Mode::Position => input[value as usize],
        Mode::Immediate => value,
    };
    //dbg!(&next);
    *pos += 1;
    next
}

fn get_mode(raw_opcode: &Vec<char>, pos: ParameterPosition) -> Mode {
    raw_opcode.get::<usize>(pos.into()).unwrap_or(&'0').into()
}

fn next_operation(input: &Vec<i32>, pos: &mut i32) -> Option<Operation> {
    let next = get_next(input, pos, Mode::Immediate);
    let mut raw_opcode: Vec<_> = next.to_string().chars().collect();
    raw_opcode.reverse();
    //dbg!(&raw_opcode);
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
            value: 1,
            addr: get_next(input, pos, Mode::Immediate) as usize,
        }),
        '4' => Some(Operation::Output {
            value: get_next(input, pos, get_mode(&raw_opcode, ParameterPosition::First)),
        }),
        '9' => None,
        _ => unreachable!(),
    }
}

fn execute(op: Operation, input: &mut Vec<i32>) {
    //dbg!(&op);
    match op {
        Operation::Add { x, y, addr } => input[addr as usize] = x + y,
        Operation::Multiply { x, y, addr } => input[addr as usize] = x * y,
        Operation::Input { value, addr } => input[addr] = value,
        Operation::Output { value } => println!("{}", value),
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
    //dbg!(&input);

    while let Some(op) = next_operation(&input, &mut pos) {
        execute(op, &mut input);
    }
}
