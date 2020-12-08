#![feature(test)]
extern crate test;
use itertools::Itertools;
use std::{env, ops::RangeInclusive};
use text_io::scan;

#[derive(Debug)]
struct PasswordVerification {
    required_char:     char,
    // Had I known what part 2 was beforehand, I wouldn’t have used a range here. Oh well.
    required_quantity: RangeInclusive<usize>,
    password:          String,
}

impl From<&'_ str> for PasswordVerification {
    fn from(s: &'_ str) -> Self {
        let required_char: char;
        let lower: usize;
        let upper: usize;
        let password: String;
        scan!(s.bytes() => "{}-{} {}: {}\n", lower, upper, required_char, password);
        PasswordVerification {
            required_char,
            required_quantity: lower..=upper,
            password,
        }
    }
}

impl PasswordVerification {
    fn is_valid(&self) -> bool {
        self.required_quantity
            .contains(&self.password.chars().filter(|&c| c == self.required_char).count())
    }

    fn is_valid_part2(&self) -> bool {
        let pw = self.password.as_bytes();
        let (first, second) = (*self.required_quantity.start(), *self.required_quantity.end());
        // 1-based indexing :reeeeeee:
        (pw[first - 1] == self.required_char as u8) ^ (pw[second - 1] == self.required_char as u8)
    }
}

fn read_input() -> Vec<PasswordVerification> {
    std::fs::read_to_string(env::args().nth(1).filter(|n| n != "--bench").unwrap_or(String::from("inputs/day02")))
        .unwrap()
        .lines()
        .map_into()
        .collect()
}

fn main() {
    let input = read_input();
    let p1 = input.iter().filter(|pw| pw.is_valid()).count();
    println!("{}", p1);
    let p2 = input.iter().filter(|pw| pw.is_valid_part2()).count();
    println!("{}", p2);
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_INPUT: &str = "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc";

    #[test]
    fn test_part1() {
        let pws: Vec<PasswordVerification> = TEST_INPUT.lines().map_into().collect_vec();
        let pws = pws.into_iter().filter(|pw| !pw.is_valid()).map(|pw| pw.password).collect_vec();
        assert_eq!(pws, vec![String::from("cdefg")]);
    }

    #[test]
    fn test_part2() {
        let pws: Vec<PasswordVerification> = TEST_INPUT.lines().map_into().collect_vec();
        let pws = pws.into_iter().filter(|pw| pw.is_valid_part2()).map(|pw| pw.password).collect_vec();
        assert_eq!(pws, vec![String::from("abcde")]);
    }
}
