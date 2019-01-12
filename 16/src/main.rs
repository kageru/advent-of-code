#[macro_use] extern crate text_io;
use std::collections::HashMap;
mod functions;
use crate::functions::*;

struct Call {
    before: [usize; 4],
    call: [usize; 4],
    after: [usize; 4],
}

impl Call {
    pub fn new() -> Self {
        Call {
            before: [0, 0, 0, 0],
            call: [0, 0, 0, 0],
            after: [0, 0, 0, 0],
        }
    }
}

fn check(func: &fn(usize, usize, usize, &[usize; 4]) -> [usize; 4], call: &Call) -> bool {
    return func(call.call[1], call.call[2], call.call[3], &call.before) == call.after;
}

fn verify_fn(func: &fn(usize, usize, usize, &[usize; 4]) -> [usize; 4], calls: &Vec<&Call>) -> bool {
    for call in calls {
        if !check(&func, &call) {
            return false;
        }
    }
    return true;
}

fn part1(calls: &Vec<Call>, fns: &[fn(usize, usize, usize, &[usize; 4]) -> [usize; 4]; 16]) {
    let mut more_than_three = 0;
    for call in calls {
        let mut correct = 0;
        for func in fns.iter() {
            if check(&func, &call) {
                correct += 1;
            }
        }
        if correct >= 3 {
            more_than_three += 1;
        }
    }
    println!("{}", more_than_three);
}

fn get_mapping(calls: &Vec<Call>, fns: &[fn(usize, usize, usize, &[usize; 4]) -> [usize; 4]; 16])
    -> HashMap<usize, fn(usize, usize, usize, &[usize; 4]) -> [usize; 4]> {
    let mut mapping: HashMap<usize, fn(usize, usize, usize, &[usize; 4]) -> [usize; 4]> = HashMap::new();
    let mut funcs = fns.into_iter().enumerate().collect::<Vec<_>>();
    while funcs.len() > 0 {
        for i in 0..16 {
            let op_calls = calls.iter().filter(|&c| c.call[0] == i).collect::<Vec<_>>();
            let valid = funcs.iter().filter(|&f| verify_fn(&f.1, &op_calls)).collect::<Vec<_>>();

            if valid.len() == 1 {
                let (found_id, found) = *valid.get(0).unwrap();
                mapping.insert(i, *found.clone());
                let index = funcs.iter().position(|f| f.0 == *found_id).unwrap();
                funcs.remove(index);
            }
        }
    }
    return mapping;
}

fn num_string_to_array(input: &str) -> [usize; 4] {
    let mut out: [usize; 4] = [0, 0, 0, 0];
    scan!(input.bytes() => "{} {} {} {}", out[0], out[1], out[2], out[3]);
    return out;
}

fn main() {
    let fns = [addi, addr, muli, mulr, seti, setr, bani, banr, bori, borr, gtir, gtri, gtrr, eqir, eqri, eqrr];
    // Apparently, this needs to be mutable for me to take elements inside the loop later
    let mut lines = include_str!("../input").lines().filter(|line| line != &"").peekable();
    let mut calls: Vec<Call> = Vec::new();

    while lines.peek() != None {
        let mut call = Call::new();

        scan!(lines.next().unwrap().bytes() => "Before: [{}, {}, {}, {}]", call.before[0], call.before[1], call.before[2], call.before[3]);
        call.call = num_string_to_array(lines.next().unwrap());
        scan!(lines.next().unwrap().bytes() => "After:  [{}, {}, {}, {}]", call.after[0], call.after[1], call.after[2], call.after[3]);

        calls.push(call);
    }

    part1(&calls, &fns);

    // Part 2
    let program_lines = include_str!("../input2").lines();
    let mapping = get_mapping(&calls, &fns);
    println!("garbage ends");

    let mut registers: [usize; 4] = [0, 0, 0, 0];
    //scan!(program_lines.next().unwrap().bytes() => "{} {} {} {}", registers[0], registers[1], registers[2], registers[3]);
    for line in program_lines {
        let call = num_string_to_array(line);
        registers = mapping.get(&call[0]).unwrap()(call[1], call[2], call[3], &registers);
        println!("regs: {}, {}, {}, {}", registers[0], registers[1], registers[2], registers[3]);
        println!("call: {}, {}, {}, {}", call[0], call[1], call[2], call[3]);
    }
    // 464 is too low
    println!("{}", registers[0]);
}
