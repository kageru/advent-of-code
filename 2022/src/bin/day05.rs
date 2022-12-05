#![feature(test)]
extern crate test;
use aoc2022::{boilerplate, common::*};

const DAY: usize = 5;
type Parsed = (Vec<Vec<u8>>, Vec<Move>);

#[derive(Debug)]
struct Move {
    n:   usize,
    src: usize,
    dst: usize,
}

fn parse_input(raw: &str) -> Parsed {
    let mut lines = raw.lines();
    let mut raw_stacks = Vec::new();
    let stack_numbers = loop {
        match lines.next() {
            Some(line) if line.contains('[') => raw_stacks.push(line),
            Some(line) => break line,
            None => unreachable!(),
        }
    };
    let num_stacks = (stack_numbers.trim().bytes().last().unwrap() - b'0') as usize;
    let mut stacks = vec![vec![]; num_stacks];
    for s in raw_stacks.iter().rev() {
        for n in 0..num_stacks {
            match s.as_bytes().get(1 + 4 * n) {
                Some(b' ') | None => (),
                Some(&c) => stacks[n].push(c),
            }
        }
    }
    let moves = lines
        .skip(1)
        .map(|l| {
            let (n, pos) = l[5..].split_once(" from ").unwrap();
            let (src, dst) = pos.split_once(" to ").unwrap();
            Move { n: parse_num(n), src: parse_num::<usize>(src) - 1, dst: parse_num::<usize>(dst) - 1 }
        })
        .collect();
    (stacks, moves)
}

fn part1((stacks, moves): &Parsed) -> String {
    let mut stacks = stacks.to_owned();
    for mov in moves {
        for _ in 0..(mov.n) {
            let e = stacks[mov.src].pop().unwrap();
            stacks[mov.dst].push(e);
        }
    }
    stacks.iter().filter_map(|s| s.last()).map(|&b| b as char).collect()
}

fn part2((stacks, moves): &Parsed) -> String {
    let mut stacks = stacks.to_owned();
    let mut temp = Vec::new();
    for mov in moves {
        // Sadly can’t drain from one into the other because the compiler doesn’t know
        // src and dst are always different :feelsBadMan:
        let start_index = stacks[mov.src].len() - mov.n;
        temp.extend(stacks[mov.src].drain(start_index..));
        for e in temp.drain(..) {
            stacks[mov.dst].push(e);
        }
    }
    stacks.iter().filter_map(|s| s.last()).map(|&b| b as char).collect()
}

boilerplate! {
    TEST_INPUT ==
"    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2",
    tests: {
        part1: { TEST_INPUT => "CMZ" },
        part2: { TEST_INPUT => "MCD" },
    },
    bench1 == "QMBMJDFTD",
    bench2 == "NBTVTJNFJ",
    bench_parse: check_input => 504,
}

#[cfg(test)]
fn check_input((_, moves): &Parsed) -> usize {
    moves.len()
}
