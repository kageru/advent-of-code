use itertools::Itertools;
use std::io;
use std::io::BufRead;

#[derive(Debug)]
enum Operation {
    Add { x: i64, y: i64, addr: usize },
    Multiply { x: i64, y: i64, addr: usize },
    Input { value: i64, addr: usize },
    Output { value: i64 },
    JumpIfTrue { value: i64, addr: i64 },
    JumpIfFalse { value: i64, addr: i64 },
    LessThan { first: i64, second: i64, addr: i64 },
    Equals { first: i64, second: i64, addr: i64 },
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

fn get_next(input: &[i64], pos: &mut i64, mode: Mode) -> i64 {
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

fn next_operation(input: &[i64], pos: &mut i64, inputs: &mut Vec<i64>) -> Option<Operation> {
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
            value: inputs.pop().unwrap(),
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

fn execute(op: Operation, input: &mut Vec<i64>, pos: &mut i64) -> Option<i64> {
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
            input[addr as usize] = (first < second) as i64;
            None
        }
        Operation::Equals {
            first,
            second,
            addr,
        } => {
            input[addr as usize] = (first == second) as i64;
            None
        }
    }
}

pub fn main() {
    let input: Vec<i64> = io::stdin()
        .lock()
        .lines()
        .next()
        .unwrap()
        .unwrap()
        .split(',')
        .map(|n| n.parse().unwrap())
        .collect();

    let p1: i64 = (0..5)
        .permutations(5)
        .map(|amps| {
            let mut last_output = 0;
            for amp in amps {
                let mut pos = 0;
                let mut tape = input.clone();
                let mut params = vec![last_output, amp];
                match execute_machine(&mut tape, &mut pos, &mut params) {
                    Ok(o) => last_output = o,
                    Err(o) => last_output = o,
                }
            }
            last_output
        })
        .max()
        .unwrap();
    println!("Part 1: {}", p1);

    let p2: i64 = (5..10)
        .permutations(5)
        .map(|amps| {
            let mut last_output = 0;
            let mut inputs: Vec<_> = amps.into_iter().map(move |amp| vec![amp]).collect();
            let mut positions = vec![0, 0, 0, 0, 0];
            let mut states: Vec<_> = (0..5).map(|_| input.clone()).collect();
            for state in (0..5).cycle() {
                let mut part1_input = states.get_mut(state).unwrap();
                let mut pos = positions.get_mut(state).unwrap();
                let mut params = inputs.get_mut(state).unwrap();
                params.insert(0, last_output);
                match execute_machine(&mut part1_input, &mut pos, &mut params) {
                    Err(output) => last_output = output,
                    Ok(_) => break,
                }
            }
            last_output
        })
        .max()
        .unwrap();
    println!("Part 2: {}", p2);
}

fn execute_machine(tape: &mut Vec<i64>, pos: &mut i64, params: &mut Vec<i64>) -> Result<i64, i64> {
    loop {
        match next_operation(tape, pos, params) {
            Some(op) => {
                if let Some(o) = execute(op, tape, pos) {
                    return Err(o);
                }
            }
            None => {
                return Ok((params.pop().unwrap()));
            }
        }
    }
}
