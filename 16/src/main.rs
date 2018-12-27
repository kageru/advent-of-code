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

fn main() {
    let fns = [addi, addr, muli, mulr, seti, setr, bani, banr, bori, borr, gtir, gtri, gtrr, eqir, eqri, eqrr];
    // Apparently, this needs to be mutable for me to take elements inside the loop later
    let mut lines = include_str!("../input").lines().filter(|line| line != &"").peekable();
    let mut calls: Vec<Call> = Vec::new();

    while lines.peek() != None {
        let mut call = Call::new();

        scan!(lines.next().unwrap().bytes() => "Before: [{}, {}, {}, {}]", call.before[0], call.before[1], call.before[2], call.before[3]);
        scan!(lines.next().unwrap().bytes() => "{} {} {} {}", call.call[0], call.call[1], call.call[2], call.call[3]);
        scan!(lines.next().unwrap().bytes() => "After:  [{}, {}, {}, {}]", call.after[0], call.after[1], call.after[2], call.after[3]);

        calls.push(call);
    }

    // Part 1
    let mut more_than_three = 0;
    for call in &calls {
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

    // Part 2
    let mut mapping: HashMap<usize, fn(usize, usize, usize, &[usize; 4]) -> [usize; 4]> = HashMap::new();
    for i in 0..16 {
        let op_calls = calls.iter().filter(|&c| c.call[0] == i).collect::<Vec<_>>();
        let clone = fns.clone();
        println!("{}", op_calls.len());
        let funcs = clone.into_iter().filter(|&f| verify_fn(&f, &op_calls)).collect::<Vec<_>>();

        if funcs.len() == 1 {
            //let f = *funcs.get(0).unwrap();
            mapping.insert(i, *funcs.get(0).unwrap().clone());
        } else {
            println!("help: {}", funcs.len());
        }
    }
}
