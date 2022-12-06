#![feature(test, array_windows)]
extern crate test;
use std::collections::HashSet;

use aoc2022::{boilerplate, common::*};

const DAY: usize = 6;

fn parse_input(raw: &str) -> &str {
    raw
}

// Different implementation because manual comparisons are feasible for groups of 4 and
// significantly faster than sets.
fn part1(parsed: &str) -> usize {
    parsed
        .as_bytes()
        .array_windows()
        .zip(4..)
        .find_map(|([a, b, c, d], i)| (a != b && a != c && a != d && b != c && b != d && c != d).then_some(i))
        .unwrap()
}

fn part2(parsed: &str) -> usize {
    parsed
        .as_bytes()
        .array_windows::<14>()
        .zip(14..)
        .find_map(|(arr, i)| (arr.iter().collect::<HashSet<_>>().len() == 14).then_some(i))
        .unwrap()
}

boilerplate! {
    TEST_INPUT == "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
    tests: {
        part1: {
            TEST_INPUT => 7,
            "bvwbjplbgvbhsrlpgdmjqwftvncz" => 5,
            "nppdvjthqldpwncqszvftbrmjlhg" => 6,
            "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" => 10,
            "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" => 11,
        },
        part2: {
            TEST_INPUT => 19,
            "nppdvjthqldpwncqszvftbrmjlhg" => 23,
            "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" => 29,
            "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" => 26,
        },
    },
    bench1 == 1287,
    bench2 == 3716,
    bench_parse: str::len => 4096,
}
