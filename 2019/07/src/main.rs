use itertools::Itertools;
use std::io;
use std::io::BufRead;
use std::ops::Range;

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

struct Machine {
    pos: i64,
    tape: Vec<i64>,
    params: Vec<i64>,
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

#[rustfmt::skip]
fn next_operation(machine: &mut Machine) -> Option<Operation> {
    let next = get_next(&machine.tape, &mut machine.pos, Mode::Immediate);
    let mut raw_opcode: Vec<_> = next.to_string().chars().collect();
    raw_opcode.reverse();
    match raw_opcode[0] {
        '1' => Some(Operation::Add {
            x: get_next(&machine.tape, &mut machine.pos, get_mode(&raw_opcode, ParameterPosition::First)),
            y: get_next(&machine.tape, &mut machine.pos, get_mode(&raw_opcode, ParameterPosition::Second)),
            addr: get_next(&machine.tape, &mut machine.pos, Mode::Immediate) as usize,
        }),
        '2' => Some(Operation::Multiply {
            x: get_next(&machine.tape, &mut machine.pos, get_mode(&raw_opcode, ParameterPosition::First)),
            y: get_next(&machine.tape, &mut machine.pos, get_mode(&raw_opcode, ParameterPosition::Second)),
            addr: get_next(&machine.tape, &mut machine.pos, Mode::Immediate) as usize,
        }),
        '3' => Some(Operation::Input {
            value: machine.params.pop().unwrap(),
            addr: get_next(&machine.tape, &mut machine.pos, Mode::Immediate) as usize,
        }),
        '4' => Some(Operation::Output {
            value: get_next(&machine.tape, &mut machine.pos, get_mode(&raw_opcode, ParameterPosition::First)),
        }),
        '5' => Some(Operation::JumpIfTrue {
            value: get_next(&machine.tape, &mut machine.pos, get_mode(&raw_opcode, ParameterPosition::First)),
            addr: get_next(&machine.tape, &mut machine.pos, get_mode(&raw_opcode, ParameterPosition::Second)),
        }),
        '6' => Some(Operation::JumpIfFalse {
            value: get_next(&machine.tape, &mut machine.pos, get_mode(&raw_opcode, ParameterPosition::First)),
            addr: get_next(&machine.tape, &mut machine.pos, get_mode(&raw_opcode, ParameterPosition::Second)),
        }),
        '7' => Some(Operation::LessThan {
            first: get_next(&machine.tape, &mut machine.pos, get_mode(&raw_opcode, ParameterPosition::First)),
            second: get_next(&machine.tape, &mut machine.pos, get_mode(&raw_opcode, ParameterPosition::Second)),
            addr: get_next(&machine.tape, &mut machine.pos, Mode::Immediate),
        }),
        '8' => Some(Operation::Equals {
            first: get_next(&machine.tape, &mut machine.pos, get_mode(&raw_opcode, ParameterPosition::First)),
            second: get_next(&machine.tape, &mut machine.pos, get_mode(&raw_opcode, ParameterPosition::Second)),
            addr: get_next(&machine.tape, &mut machine.pos, Mode::Immediate),
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

fn find_max(range: Range<i64>, input: &Vec<i64>) -> i64 {
    range
        .permutations(5)
        .map(|amps| {
            let mut last_output = 0;
            let mut machines: Vec<_> = amps
                .into_iter()
                .map(|amp| Machine {
                    tape: input.clone(),
                    pos: 0,
                    params: vec![amp],
                })
                .collect();
            for state in (0..5).cycle() {
                let mut machine = machines.get_mut(state).unwrap();
                machine.params.insert(0, last_output);
                match execute_machine(&mut machine) {
                    Err(output) => last_output = output,
                    Ok(_) => break,
                }
            }
            last_output
        })
        .max()
        .unwrap()
}

fn execute_machine(machine: &mut Machine) -> Result<i64, i64> {
    loop {
        match next_operation(machine) {
            Some(op) => {
                if let Some(o) = execute(op, &mut machine.tape, &mut machine.pos) {
                    return Err(o);
                }
            }
            None => return Ok(machine.params.pop().unwrap()),
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

    println!("Part 1: {}", find_max(0..5, &input));
    println!("Part 2: {}", find_max(5..10, &input));
}
