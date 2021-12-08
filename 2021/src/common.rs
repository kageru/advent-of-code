use std::env;

#[cfg(not(debug_assertions))]
use std::ops::{AddAssign, MulAssign};

#[cfg(debug_assertions)]
use std::{
    fmt::{Debug, Display},
    str::FromStr,
};

pub fn read_file(day: usize) -> String {
    std::fs::read_to_string(env::var("AOC_INPUT").unwrap_or_else(|_| format!("inputs/day{:0>2}", day))).unwrap()
}

pub fn parse_nums(l: &str) -> Vec<usize> {
    l.lines().map(parse_num).collect()
}

pub fn parse_nums_comma(l: &str) -> Vec<usize> {
    l.trim().split(',').map(parse_num).collect()
}

#[cfg(debug_assertions)]
pub fn parse_num<T: FromStr<Err: Debug> + Display>(s: &str) -> T {
    s.parse().unwrap_or_else(|e| panic!("Invalid number {s}: {e:?}"))
}

#[cfg(not(debug_assertions))]
pub fn parse_num<T: From<u8> + AddAssign<T> + MulAssign<T>>(s: &str) -> T {
    let mut digits = s.bytes().map(|b| T::from(b - b'0'));
    let mut n = digits.next().unwrap();
    for digit in digits {
        n *= T::from(10);
        n += digit;
    }
    n
}
