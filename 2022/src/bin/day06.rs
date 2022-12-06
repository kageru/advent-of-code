#![feature(test, array_windows)]
extern crate test;

use aoc2022::{boilerplate, common::*};

const DAY: usize = 6;

fn parse_input(raw: &str) -> &str {
    raw
}

fn part1(parsed: &str) -> usize {
    solve::<4>(parsed)
}

fn part2(parsed: &str) -> usize {
    solve::<14>(parsed)
}

fn solve<const BS: usize>(s: &str) -> usize {
    s.as_bytes()
        .array_windows::<BS>()
        .zip(BS..)
        .find_map(|(arr, position)| {
            let bitset = arr.iter().fold(0u32, |bitset, n| bitset | 1 << (n & 0b1_1111));
            (bitset.count_ones() as usize == BS).then_some(position)
        })
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
