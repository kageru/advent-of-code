use std::io;
use std::io::BufRead;

pub fn main() {
    let input: Vec<usize> = io::stdin()
        .lock()
        .lines()
        .next()
        .unwrap()
        .unwrap()
        .split(',')
        .map(|n| n.parse().unwrap())
        .collect();
    println!("Part 1: {}", execute(&mut input.clone(), 12, 2));
}

fn execute(input: &mut Vec<usize>, p1: usize, p2: usize) -> usize {
    input[1] = p1;
    input[2] = p2;
    for i in (0..).step_by(4) {
        let opcode = input[i];
        let first = input[i+1];
        let second = input[i+2];
        let target = input[i+3];
        if target == 0 {
            dbg!("target is 0", target, opcode, first, second);
        }
        match opcode {
            1 => input[target] = input[first] + input[second],
            2 => input[target] = input[first] * input[second],
            99 => break,
            _ => unreachable!("Invalid opcode")
        }
    }
    dbg!(&input);
    input[0]
}
