#![feature(array_windows)]
#![feature(test)]
extern crate test;
use aoc2021::common::*;
use fnv::FnvHashMap;

const DAY: usize = 14;
type Parsed = (FnvHashMap<[u8; 2], u8>, String);

fn parse_input(raw: &str) -> Parsed {
    let (state, rules) = raw.split_once("\n\n").unwrap();
    let rules = rules.lines().map(|line| line.as_bytes()).map(|bytes| ([bytes[0], bytes[1]], bytes[bytes.len() - 1])).collect();
    (rules, state.to_owned())
}

fn grow_polys((rules, raw_state): &Parsed, generations: usize) -> usize {
    let mut state = FnvHashMap::default();
    for polymer in raw_state.as_bytes().array_windows() {
        *state.entry(*polymer).or_insert(0) += 1;
    }
    for _ in 0..generations {
        for ([p1, p2], quantity) in state.clone() {
            let output = rules[&[p1, p2]];
            *state.entry([p1, output]).or_insert(0) += quantity;
            *state.entry([output, p2]).or_insert(0) += quantity;
            *state.get_mut(&[p1, p2]).unwrap() -= quantity;
        }
    }
    let mut charcounts = FnvHashMap::default();
    for (p, q) in state.into_iter().flat_map(|([p1, p2], q)| [(p1, q), (p2, q)]) {
        *charcounts.entry(p).or_insert(0) += q;
    }
    for (&p, q) in charcounts.iter_mut() {
        // This implementation counts each element except the very first and very last twice.
        // We add 1 for those and then divide by 2.
        *q += (p == raw_state.bytes().next().unwrap()) as usize;
        *q += (p == raw_state.bytes().last().unwrap()) as usize;
        *q /= 2;
    }
    charcounts.values().max().unwrap() - charcounts.values().filter(|&&q| q != 0).min().unwrap()
}

fn part1(parsed: &Parsed) -> usize {
    grow_polys(parsed, 10)
}

fn part2(parsed: &Parsed) -> usize {
    grow_polys(parsed, 40)
}

fn main() {
    let input = parse_input(&read_file(DAY));
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2021::*;

    const TEST_INPUT: &str = "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C";

    test!(part1() == 1588);
    test!(part2() == 2188189693529);
    bench!(part1() == 3587);
    bench!(part2() == 3906445077999);
    bench_input!(input_size => 120);

    fn input_size((m, s): &Parsed) -> usize {
        m.len() + s.len()
    }
}
