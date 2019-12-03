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

    let part2_target = 19690720;
    let part2 = (1..99)
        .flat_map(|x| (1..99).map(move |y| (x, y)))
        .filter(|(x, y)| execute(&mut input.clone(), *x, *y) == part2_target)
        .map(|(x, y)| x * 100 + y)
        .next()
        .unwrap();
    println!("Part 2: {}", part2);
}

fn execute(input: &mut Vec<usize>, p1: usize, p2: usize) -> usize {
    input[1] = p1;
    input[2] = p2;
    for i in (0..).step_by(4) {
        let opcode = input[i];
        let first = input[i + 1];
        let second = input[i + 2];
        let target = input[i + 3];
        match opcode {
            1 => input[target] = input[first] + input[second],
            2 => input[target] = input[first] * input[second],
            99 => break,
            _ => unreachable!("Invalid opcode"),
        }
    }
    input[0]
}
