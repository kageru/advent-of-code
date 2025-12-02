#![feature(test)]
extern crate test;
use aoc2025::{boilerplate, common::*};
use itertools::Itertools;

const DAY: usize = 2;
type I = usize;
type Parsed = Vec<(I, I)>;

fn parse_input(raw: &str) -> Parsed {
    raw.trim().split(',').filter_map(|s| s.split_once('-')).map(|(start, end)| (parse_num(start), parse_num(end))).collect()
}

fn solve<F: Fn(I) -> bool>(parsed: &Parsed, f: F) -> usize {
    parsed.iter().map(|&(start, end)| (start..=end).filter(|&n| f(n)).sum::<I>()).sum()
}

fn part1(parsed: &Parsed) -> usize {
    solve(parsed, is_invalid)
}

fn part2(parsed: &Parsed) -> usize {
    solve(parsed, is_invalid_p2)
}

fn is_invalid(n: I) -> bool {
    let num_digits = n.ilog10() as usize + 1;
    if !num_digits.is_multiple_of(2) {
        return false;
    }
    let s = n.to_string();
    let (left, right) = s.as_bytes().split_at(num_digits / 2);
    left == right
}

fn is_invalid_p2(n: I) -> bool {
    let s = n.to_string();
    let b = s.as_bytes();
    (1..=(b.len() / 2))
        .filter(|&step_size| b.len().is_multiple_of(step_size))
        .any(|step_size| (0..step_size).all(|offset| b.iter().skip(offset).step_by(step_size).dedup().count() == 1))
}

boilerplate! {
    TEST_INPUT == "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
    for tests: {
        part1: { TEST_INPUT => 1227775554 },
        part2: {
            TEST_INPUT => 4174379265,
            "11-22" => 33,
            "95-115" => 210,
            "998-1012" => 2009,
            "1188511880-1188511890" => 1188511885,
            "222220-222224" => 222222,
            "1698522-1698528" => 0,
            "446443-446449" => 446446,
            "38593856-38593862" => 38593859,
            "824824821-824824827" => 824824824,
            "2121212118-2121212124" => 2121212121,
        },
    },
    unittests: {
        is_invalid_p2: {
            999 => true,
            1009 => false,
            1010 => true,
            222222 => true,
            446446 => true,
            38593859 => true,
        }
    },
    bench1 == 28846518423,
    bench2 == 31578210022,
    bench_parse: Vec::len => 38,
}
