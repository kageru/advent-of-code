#![feature(test)]
extern crate test;
use aoc2022::{boilerplate, common::*};

const DAY: usize = 11;
type Parsed = Vec<Monkey>;

#[derive(Clone, Debug)]
struct Monkey {
    inspection_count: usize,
    items:            Vec<usize>,
    op:               MonkeyOp,
    div_test:         usize,
    true_dst:         usize,
    false_dst:        usize,
}

#[derive(Copy, Clone, Debug)]
enum MonkeyOp {
    Add(Option<usize>),
    Mul(Option<usize>),
}

fn parse_input(raw: &str) -> Parsed {
    raw.split("\n\n")
        .map(|raw_monkey| {
            let mut lines = raw_monkey.lines();
            let [_, start, op, test, if_true, if_false] = std::array::from_fn(|_| lines.next().unwrap());
            let items = start[18..].split(", ").map(parse_num).collect();
            let div_test = parse_num(&test[21..]);
            let true_dst = parse_num(&if_true[29..]);
            let false_dst = parse_num(&if_false[30..]);
            let op = op.as_bytes();
            let op = match (op[23], op[25]) {
                (b'+', b'o') => MonkeyOp::Add(None),
                (b'+', x) => MonkeyOp::Add(Some((x - b'0') as _)),
                (b'*', b'o') => MonkeyOp::Mul(None),
                (b'*', x) => MonkeyOp::Mul(Some((x - b'0') as _)),
                _ => unreachable!(),
            };
            Monkey { inspection_count: 0, items, op, div_test, true_dst, false_dst }
        })
        .collect()
}

fn part1(parsed: &Parsed) -> usize {
    monkey_business::<20, 3>(parsed)
}

fn part2(parsed: &Parsed) -> usize {
    monkey_business::<10_000, 1>(parsed)
}

fn monkey_business<const ITERATIONS: usize, const STRESS_REDUCTION: usize>(parsed: &Parsed) -> usize {
    // The checks are all prime numbers, so the lcm is just their product.
    let lcm = parsed.iter().map(|m| m.div_test).product();
    let mut monkeys: Vec<Monkey> = parsed.clone();
    let mut moved = Vec::new();
    for _ in 0..ITERATIONS {
        let mut i = 0;
        while i < monkeys.len() {
            let monkey = monkeys.get_mut(i).unwrap();
            monkey.inspection_count += monkey.items.len();
            while !monkey.items.is_empty() {
                moved.extend(monkey.items.drain(..).map(|mut stress| {
                    stress = match monkey.op {
                        MonkeyOp::Add(x) => stress + x.unwrap_or(stress),
                        MonkeyOp::Mul(x) => stress * x.unwrap_or(stress),
                    } / STRESS_REDUCTION;
                    if stress > lcm {
                        stress %= lcm;
                    }
                    (stress, if stress % monkey.div_test == 0 { monkey.true_dst } else { monkey.false_dst })
                }));
            }
            drop(monkey);
            for (item, dst) in moved.drain(..) {
                monkeys.get_mut(dst).unwrap().items.push(item);
            }
            i += 1;
        }
    }
    monkeys.sort_by_key(|m| m.inspection_count);
    monkeys.into_iter().rev().map(|m| m.inspection_count).take(2).product()
}

boilerplate! {
    TEST_INPUT == "\
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1",
    tests: {
        part1: { TEST_INPUT => 10605 },
        part2: { TEST_INPUT => 2713310158 },
    },
    bench1 == 56595,
    bench2 == 15693274740,
    bench_parse: Vec::len => 8,
}
