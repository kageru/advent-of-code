#![feature(test)]
#![allow(clippy::ptr_arg, clippy::upper_case_acronyms)]
extern crate test;
use aoc2020::common::*;

#[derive(Debug, PartialEq)]
enum Command {
    NOP(i32),
    ACC(i32),
    JMP(i32),
}

fn read_input() -> String {
    read_file(8)
}

fn parse_input(raw: &str) -> Vec<Command> {
    raw.lines()
        .map(|l| match l.split_once(' ') {
            Some(("nop", x)) => Command::NOP(x.parse().unwrap()),
            Some(("acc", x)) => Command::ACC(x.parse().unwrap()),
            Some(("jmp", x)) => Command::JMP(x.parse().unwrap()),
            _ => unreachable!(),
        })
        .collect()
}

fn main() {
    let commands = parse_input(&read_input());
    println!("Part 1: {}", part1(&commands));
    println!(
        "Part 2: {}",
        part2(&commands, &mut vec![false; commands.len()], 0, 0, false).unwrap()
    );
}

fn part1(commands: &Vec<Command>) -> i32 {
    let mut seen = vec![false; commands.len()];
    let mut index = 0i32;
    let mut acc = 0;
    loop {
        if seen[index as usize] {
            return acc;
        }
        seen[index as usize] = true;
        match commands[index as usize] {
            Command::NOP(_) => index += 1,
            Command::ACC(x) => {
                acc += x;
                index += 1
            }
            Command::JMP(x) => index += x,
        }
    }
}

fn part2(commands: &Vec<Command>, seen: &mut Vec<bool>, mut index: i32, mut acc: i32, changed: bool) -> Option<i32> {
    loop {
        if index as usize >= commands.len() {
            return Some(acc);
        }
        if changed && seen[index as usize] {
            return None;
        }
        seen[index as usize] = true;
        match commands[index as usize] {
            Command::NOP(x) => {
                if !changed && index > -x {
                    if let Some(n) = part2(commands, seen, index + x, acc, true) {
                        return Some(n);
                    }
                }
                index += 1;
            }
            Command::ACC(x) => {
                acc += x;
                index += 1
            }
            Command::JMP(x) => {
                // is there no way to have regular if and if let in one statement?
                if !changed {
                    if let Some(n) = part2(commands, seen, index + 1, acc, true) {
                        return Some(n);
                    }
                }
                index += x;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::black_box;
    use aoc2020::*;
    use paste::paste;

    const TEST_INPUT: &str = "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6";

    test!(part1() == 5);
    test!(part2(&mut vec![false; 9], 0, 0, false) == Some(8));
    bench!(part1() == 1317);
    bench!(part2(&mut vec![false; 626], 0, 0, false) == Some(1033));
    bench_input!(len == 626);
}
