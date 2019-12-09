pub struct Amplifier {
    pub pos: i64,
    pub tape: Vec<i64>,
    pub params: Vec<i64>,
}

impl Amplifier {
    pub fn run(&mut self) -> Result<i64, i64> {
        loop {
            match self.decode_next() {
                Some(op) => {
                    if let Some(o) = self.execute(op) {
                        return Err(o);
                    }
                }
                None => return Ok(self.params.pop().unwrap()),
            }
        }
    }

    fn get_next(&mut self, mode: Mode) -> i64 {
        let value = self.tape[self.pos as usize];
        let next = match mode {
            Mode::Position => self.tape[value as usize],
            Mode::Immediate => value,
        };
        self.pos += 1;
        next
    }

    fn decode_next(&mut self) -> Option<Operation> {
        let next = self.get_next(Mode::Immediate);
        let mut raw_opcode: Vec<_> = next.to_string().chars().collect();
        raw_opcode.reverse();
        match raw_opcode[0] {
            '1' => Some(Operation::Add {
                x: self.get_next(get_mode(&raw_opcode, ParameterPosition::First)),
                y: self.get_next(get_mode(&raw_opcode, ParameterPosition::Second)),
                addr: self.get_next(Mode::Immediate) as usize,
            }),
            '2' => Some(Operation::Multiply {
                x: self.get_next(get_mode(&raw_opcode, ParameterPosition::First)),
                y: self.get_next(get_mode(&raw_opcode, ParameterPosition::Second)),
                addr: self.get_next(Mode::Immediate) as usize,
            }),
            '3' => Some(Operation::Input {
                value: self.params.pop().unwrap(),
                addr: self.get_next(Mode::Immediate) as usize,
            }),
            '4' => Some(Operation::Output {
                value: self.get_next(get_mode(&raw_opcode, ParameterPosition::First)),
            }),
            '5' => Some(Operation::JumpIfTrue {
                value: self.get_next(get_mode(&raw_opcode, ParameterPosition::First)),
                addr: self.get_next(get_mode(&raw_opcode, ParameterPosition::Second)),
            }),
            '6' => Some(Operation::JumpIfFalse {
                value: self.get_next(get_mode(&raw_opcode, ParameterPosition::First)),
                addr: self.get_next(get_mode(&raw_opcode, ParameterPosition::Second)),
            }),
            '7' => Some(Operation::LessThan {
                first: self.get_next(get_mode(&raw_opcode, ParameterPosition::First)),
                second: self.get_next(get_mode(&raw_opcode, ParameterPosition::Second)),
                addr: self.get_next(Mode::Immediate),
            }),
            '8' => Some(Operation::Equals {
                first: self.get_next(get_mode(&raw_opcode, ParameterPosition::First)),
                second: self.get_next(get_mode(&raw_opcode, ParameterPosition::Second)),
                addr: self.get_next(Mode::Immediate),
            }),
            '9' => None,
            _ => unreachable!(),
        }
    }

    fn execute(&mut self, op: Operation) -> Option<i64> {
        match op {
            Operation::Add { x, y, addr } => {
                self.tape[addr as usize] = x + y;
                None
            }
            Operation::Multiply { x, y, addr } => {
                self.tape[addr as usize] = x * y;
                None
            }
            Operation::Input { value, addr } => {
                self.tape[addr] = value;
                None
            }
            Operation::Output { value } => Some(value),
            Operation::JumpIfTrue { value, addr } => {
                if value != 0 {
                    self.pos = addr
                }
                None
            }
            Operation::JumpIfFalse { value, addr } => {
                if value == 0 {
                    self.pos = addr
                }
                None
            }
            Operation::LessThan {
                first,
                second,
                addr,
            } => {
                self.tape[addr as usize] = (first < second) as i64;
                None
            }
            Operation::Equals {
                first,
                second,
                addr,
            } => {
                self.tape[addr as usize] = (first == second) as i64;
                None
            }
        }
    }
}

fn get_mode(raw_opcode: &[char], pos: ParameterPosition) -> Mode {
    raw_opcode.get::<usize>(pos.into()).unwrap_or(&'0').into()
}

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