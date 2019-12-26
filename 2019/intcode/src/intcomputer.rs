#[derive(Clone)]
pub struct IntComputer {
    pub pos: i64,
    pub tape: Vec<i64>,
    pub params: Vec<i64>,
    relative_base: i64,
    cmd_buffer: Vec<Operation>,
}

pub enum IntComputerResult {
    Halt,
    Output(i64),
    Continue,
}

impl IntComputerResult {
    pub fn unwrap(self) -> i64 {
        match self {
            IntComputerResult::Halt => panic!("Attempted to get value of halt operation"),
            IntComputerResult::Output(o) => o,
            IntComputerResult::Continue => panic!("Attempted to get value of non-output operation"),
        }
    }
}

impl IntComputer {
    pub fn without_params(tape: Vec<i64>) -> Self {
        IntComputer {
            pos: 0,
            tape,
            params: Vec::new(),
            relative_base: 0,
            cmd_buffer: Vec::new(),
        }
    }

    pub fn new(tape: Vec<i64>, pos: i64, params: Vec<i64>) -> Self {
        IntComputer {
            pos,
            tape,
            params,
            relative_base: 0,
            cmd_buffer: Vec::new(),
        }
    }

    pub fn step(&mut self) -> IntComputerResult {
        match self.cmd_buffer.pop().unwrap_or_else(|| self.decode_next()) {
            Operation::Halt {} => IntComputerResult::Halt,
            op => {
                if let Some(o) = self.execute(op) {
                    IntComputerResult::Output(o)
                } else {
                    IntComputerResult::Continue
                }
            }
        }
    }

    pub fn run(&mut self) -> IntComputerResult {
        loop {
            match self.step() {
                IntComputerResult::Halt => return IntComputerResult::Halt,
                IntComputerResult::Output(o) => return IntComputerResult::Output(o),
                IntComputerResult::Continue => (),
            }
        }
    }

    pub fn get_all_outputs(&mut self) -> Vec<i64> {
        let mut outputs = Vec::new();
        while let IntComputerResult::Output(o) = self.run() {
            outputs.push(o);
        }
        outputs
    }

    pub fn peek_operation(&mut self) -> Operation {
        if self.cmd_buffer.is_empty() {
            let next = self.decode_next();
            self.cmd_buffer.push(next);
        }
        self.cmd_buffer[0].clone()
    }

    #[rustfmt::skip]
    fn get_next(&mut self, mode: Mode) -> i64 {
        let value = *self.tape.get(self.pos as usize).unwrap();
        let next = match mode {
            Mode::Position => *self.tape.get(value as usize).unwrap_or(&0),
            Mode::Immediate => value,
            Mode::Relative => *self.tape.get((value + self.relative_base) as usize).unwrap_or(&0),
        };
        self.pos += 1;
        next
    }

    #[rustfmt::skip]
    fn get_next_address(&mut self, mode: Mode) -> i64 {
        let value = match mode {
            Mode::Position => *self.tape.get(self.pos as usize).unwrap_or(&0),
            Mode::Immediate => unreachable!(),
            Mode::Relative => *self.tape.get(self.pos as usize).unwrap_or(&0) + self.relative_base,
        };
        self.pos += 1;
        value
    }

    fn decode_next(&mut self) -> Operation {
        let next = self.get_next(Mode::Immediate);
        let mut full_opcode: Vec<_> = next.to_string().chars().collect();
        full_opcode.reverse();
        let raw_opcode = [
            full_opcode.get(1).unwrap_or(&'0').to_owned(),
            full_opcode[0],
        ];
        match raw_opcode {
            ['0', '1'] => Operation::Add {
                x: self.get_next(get_mode(&full_opcode, ParameterPosition::First)),
                y: self.get_next(get_mode(&full_opcode, ParameterPosition::Second)),
                addr: self.get_next_address(get_mode(&full_opcode, ParameterPosition::Third))
                    as usize,
            },
            ['0', '2'] => Operation::Multiply {
                x: self.get_next(get_mode(&full_opcode, ParameterPosition::First)),
                y: self.get_next(get_mode(&full_opcode, ParameterPosition::Second)),
                addr: self.get_next_address(get_mode(&full_opcode, ParameterPosition::Third))
                    as usize,
            },
            ['0', '3'] => Operation::Input {
                value: self.params.pop().expect("Encountered input instruction with empty parameters"),
                addr: self.get_next_address(get_mode(&full_opcode, ParameterPosition::First))
                    as usize,
            },
            ['0', '4'] => Operation::Output {
                value: self.get_next(get_mode(&full_opcode, ParameterPosition::First)),
            },
            ['0', '5'] => Operation::JumpIfTrue {
                value: self.get_next(get_mode(&full_opcode, ParameterPosition::First)),
                addr: self.get_next(get_mode(&full_opcode, ParameterPosition::Second)) as usize,
            },
            ['0', '6'] => Operation::JumpIfFalse {
                value: self.get_next(get_mode(&full_opcode, ParameterPosition::First)),
                addr: self.get_next(get_mode(&full_opcode, ParameterPosition::Second)) as usize,
            },
            ['0', '7'] => Operation::LessThan {
                first: self.get_next(get_mode(&full_opcode, ParameterPosition::First)),
                second: self.get_next(get_mode(&full_opcode, ParameterPosition::Second)),
                addr: self.get_next_address(get_mode(&full_opcode, ParameterPosition::Third))
                    as usize,
            },
            ['0', '8'] => Operation::Equals {
                first: self.get_next(get_mode(&full_opcode, ParameterPosition::First)),
                second: self.get_next(get_mode(&full_opcode, ParameterPosition::Second)),
                addr: self.get_next_address(get_mode(&full_opcode, ParameterPosition::Third))
                    as usize,
            },
            ['0', '9'] => Operation::AdjustRelativeBase {
                value: self.get_next(get_mode(&full_opcode, ParameterPosition::First)),
            },
            ['9', '9'] => Operation::Halt {},
            _ => unreachable!("Unknown opcode"),
        }
    }

    fn execute(&mut self, op: Operation) -> Option<i64> {
        fn safe_write(tape: &mut Vec<i64>, index: usize, value: i64) {
            if tape.len() < index + 1 {
                tape.resize(index + 1, 0);
            }
            tape[index] = value;
        }
        match op {
            Operation::Add { x, y, addr } => safe_write(&mut self.tape, addr, x + y),
            Operation::Multiply { x, y, addr } => safe_write(&mut self.tape, addr, x * y),
            Operation::Input { value, addr } => safe_write(&mut self.tape, addr, value),
            Operation::AdjustRelativeBase { value } => self.relative_base += value,
            Operation::JumpIfTrue { value, addr } => {
                if value != 0 {
                    self.pos = addr as i64
                }
            }
            Operation::JumpIfFalse { value, addr } => {
                if value == 0 {
                    self.pos = addr as i64
                }
            }
            Operation::LessThan {
                first,
                second,
                addr,
            } => safe_write(&mut self.tape, addr as usize, (first < second) as i64),
            Operation::Equals {
                first,
                second,
                addr,
            } => safe_write(&mut self.tape, addr, (first == second) as i64),
            Operation::Output { value } => {
                return Some(value);
            }
            Operation::Halt {} => unreachable!(),
        };
        None
    }
}

#[inline]
fn get_mode(raw_opcode: &[char], pos: ParameterPosition) -> Mode {
    raw_opcode.get::<usize>(pos.into()).unwrap_or(&'0').into()
}

#[derive(Debug, Clone)]
pub enum Operation {
    Add {
        x: i64,
        y: i64,
        addr: usize,
    },
    Multiply {
        x: i64,
        y: i64,
        addr: usize,
    },
    Input {
        value: i64,
        addr: usize,
    },
    Output {
        value: i64,
    },
    JumpIfTrue {
        value: i64,
        addr: usize,
    },
    JumpIfFalse {
        value: i64,
        addr: usize,
    },
    LessThan {
        first: i64,
        second: i64,
        addr: usize,
    },
    Equals {
        first: i64,
        second: i64,
        addr: usize,
    },
    AdjustRelativeBase {
        value: i64,
    },
    Halt {},
}

#[derive(Debug)]
enum Mode {
    Immediate,
    Position,
    Relative,
}

enum ParameterPosition {
    First,
    Second,
    Third,
}

impl Into<usize> for ParameterPosition {
    fn into(self) -> usize {
        match self {
            ParameterPosition::First => 2,
            ParameterPosition::Second => 3,
            ParameterPosition::Third => 4,
        }
    }
}

impl Into<Mode> for &char {
    fn into(self) -> Mode {
        match self {
            '0' => Mode::Position,
            '1' => Mode::Immediate,
            '2' => Mode::Relative,
            _ => unreachable!(),
        }
    }
}
