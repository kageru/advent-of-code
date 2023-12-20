#![feature(test)]
extern crate test;
use std::collections::VecDeque;

use self::{Node::*, Pulse::*};
use aoc2023::{boilerplate, common::*};
use fnv::FnvHashMap as Map;

const DAY: usize = 20;
type Parsed<'a> = Map<&'a str, (Node<'a>, Vec<&'a str>)>;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum Pulse {
    High,
    Low,
}

#[derive(Debug, Clone)]
enum Node<'a> {
    Broadcaster,
    FlipFlop(bool),
    Conjunction(Map<&'a str, Pulse>),
}

fn parse_input(raw: &str) -> Parsed {
    let mut parsed: Parsed = raw
        .lines()
        .map(|line| line.split_once(" -> ").unwrap())
        .map(|(node, outputs)| {
            let (node, name) = match node.as_bytes()[0] {
                b'%' => (FlipFlop(false), &node[1..]),
                b'&' => (Conjunction(Map::default()), &node[1..]),
                _ => (Broadcaster, node),
            };
            let outputs = outputs.split(", ").collect();
            (name, (node, outputs))
        })
        .collect();
    // Now the annoying part: set all inputs of all conjunctions to Low
    let mut con_inputs = Map::<&str, Vec<&str>>::default();
    for (input, (_, outputs)) in parsed.iter() {
        for output in outputs {
            if matches!(parsed.get(output), Some((Conjunction(_), _))) {
                match con_inputs.get_mut(output) {
                    Some(v) => v.push(input),
                    None => {
                        con_inputs.insert(output, vec![input]);
                    }
                };
            }
        }
    }
    for (con, inputs) in con_inputs {
        let Some((Conjunction(map), _)) = parsed.get_mut(con) else { unreachable!() };
        for i in inputs {
            map.insert(i, Low);
        }
    }
    parsed
}

const EMPTY: &Vec<&str> = &Vec::new();

fn process<'a, 'b>(map: &'a mut Parsed<'b>, name: &'b str, pulse: Pulse, src: &'b str) -> (&'b str, Pulse, &'a Vec<&'b str>) {
    // println!("{src} ({pulse:?}) -> {name}");
    // First modify as needed…
    match map.get_mut(name) {
        // nodes that don’t lead anywhere, e.g. `output` from the second example
        None => return ("", Low, EMPTY),
        Some((FlipFlop(_), _)) if pulse == High => return ("", Low, EMPTY),
        Some((FlipFlop(b), _)) => *b = !*b,
        Some((Conjunction(inputs), _)) => {
            inputs.insert(src, pulse);
        }
        Some(_) => (),
    };
    // …then match again on an immutable borrow from the map so we can return the output vector.
    let (pulse, out) = match (map.get(name).unwrap(), pulse) {
        ((Broadcaster, out), p) => (p, out),
        // Careful: we already flipped the flipflops above, so off gives low and on gives high.
        ((FlipFlop(false), out), Low) => (Low, out),
        ((FlipFlop(true), out), Low) => (High, out),
        ((FlipFlop(_), _), High) => (High, EMPTY),
        ((Conjunction(inputs), out), _) => {
            if inputs.values().all(|&v| v == High) {
                (Low, out)
            } else {
                (High, out)
            }
        }
    };
    (name, pulse, out)
}

fn push_button<'a, 'b>(map: &'a mut Parsed<'b>) -> (usize, usize) {
    let mut low_count = 1; // initial button
    let mut high_count = 0;
    let mut queue = VecDeque::new();
    queue.push_back(("button", Low, "broadcaster"));
    while let Some((src, pulse, output)) = queue.pop_front() {
        let (src, pulse, outputs) = process(map, output, pulse, src);
        for output in outputs {
            queue.push_back((src, pulse, output));
            match pulse {
                Low => low_count += 1,
                High => high_count += 1,
            }
        }
    }
    (low_count, high_count)
}

fn part1<'a>(map: &Parsed) -> usize {
    let mut map = map.to_owned();
    let (mut low, mut high) = (0, 0);
    for _ in 0..1000 {
        let (new_low, new_high) = push_button(&mut map);
        low += new_low;
        high += new_high;
    }
    low * high
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "\
broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a",
    TEST_INPUT_2 == "\
broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output"
    for tests: {
        part1: {
            TEST_INPUT => 32000000,
            TEST_INPUT_2 => 11687500,
        },
        part2: { TEST_INPUT => 0 },
    },
    bench1 == 929810733,
    bench2 == 0,
    bench_parse: Map::len => 58,
}
