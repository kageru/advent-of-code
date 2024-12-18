#![feature(test)]
extern crate test;
use aoc2024::{boilerplate, common::*, position::Pos};
use fnv::FnvHashSet;
// Fuck you, I’m not writing another pathfinding algorithm from scratch.
use pathfinding::directed::astar::astar;

const DAY: usize = 18;
type P = Pos<usize, 2>;
type Parsed = Vec<P>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines().filter_map(|l| l.split_once(',')).map(|(x, y)| Pos([parse_num(x), parse_num(y)])).collect()
}

fn path_len(corrupted: &FnvHashSet<P>, size: usize) -> Option<usize> {
    astar(
        &Pos([0, 0]),
        |p| p.manhattan_neighbors_checked().into_iter().filter(|p| p[0] <= size && p[1] <= size && !corrupted.contains(p)).map(|p| (p, 1)),
        |Pos([x, y])| (size - x + size - y) / 3,
        |&Pos([x, y])| x == size && y == size,
    )
    .map(|(_, len)| len)
}

fn part1(parsed: &Parsed, size: usize, bytes: usize) -> usize {
    let corrupted: FnvHashSet<_> = parsed.iter().take(bytes).cloned().collect();
    path_len(&corrupted, size).unwrap()
}

// Returning a String because my macro uses the return value as part of the unit test function names,
// so x,y would be a syntax error.
fn part2(parsed: &Parsed, size: usize) -> String {
    let mut corrupted = FnvHashSet::default();
    for &byte in parsed {
        corrupted.insert(byte);
        if path_len(&corrupted, size).is_none() {
            return format!("{}_{}", byte[0], byte[1]);
        }
    }
    unreachable!()
}

boilerplate! {
    TEST_INPUT == "\
5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0"
    for tests: {
        part1: { TEST_INPUT, 6, 12 => 22 },
        part2: { TEST_INPUT, 6 => "6_1" },
    },
    bench1(70, 1024) == 320,
    bench2(70) == "34_40",
    bench_parse: Vec::len => 3450,
}
