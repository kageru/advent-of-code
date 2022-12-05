#![feature(test, slice_take, get_many_mut)]
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
    for &s in raw_stacks.iter().rev() {
        for (n, stack) in stacks.iter_mut().enumerate() {
            match s.as_bytes().get(1 + 4 * n) {
                Some(b' ') | None => (),
                Some(&c) => stack.push(c),
            }
        }
    }
    let moves = lines
        .skip(1)
        .map(|l| match l.as_bytes() {
            [_, _, _, _, _, n, _, _, _, _, _, _, src, _, _, _, _, dst] => {
                Move { n: (n - b'0') as usize, src: (src - b'1') as _, dst: (dst - b'1') as _ }
            }
            [_, _, _, _, _, n1, n2, _, _, _, _, _, _, src, _, _, _, _, dst] => {
                Move { n: ((n1 - b'0') * 10 + n2 - b'0') as _, src: (src - b'1') as _, dst: (dst - b'1') as _ }
            }
            _ => unreachable!("Operations can’t be repeated more than 99 times"),
        })
        .collect();
    (stacks, moves)
}

fn part1(parsed: &Parsed) -> String {
    rearrange::<false>(parsed)
}

fn part2(parsed: &Parsed) -> String {
    rearrange::<true>(parsed)
}

fn rearrange<const GRAB_MULTIPLE: bool>((stacks, moves): &Parsed) -> String {
    let mut stacks = stacks.to_owned();
    for mov in moves {
        let start_index = stacks[mov.src].len() - mov.n;
        if let Ok([src, dst]) = stacks.get_many_mut([mov.src, mov.dst]) {
            if GRAB_MULTIPLE {
                dst.extend(src.drain(start_index..));
            } else {
                dst.extend(src.drain(start_index..).rev());
            }
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
