#[macro_use] extern crate text_io;
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

    let mut more_than_three = 0;

    for call in calls {
        let mut correct = 0;
        for func in fns.iter() {
            if func(call.call[1], call.call[2], call.call[3], &call.before) == call.after {
                correct += 1;
            }
        }
        if correct >= 3 {
            more_than_three += 1;
        }
    }
    println!("{}", more_than_three);
}
