use std::{
    fmt::Display,
    iter::Step,
    ops::{Add, Mul},
    str::FromStr,
};

pub fn read_file(day: usize) -> String {
    std::fs::read_to_string(std::env::var("AOC_INPUT").unwrap_or_else(|_| format!("inputs/day{day:0>2}"))).unwrap()
}

pub trait ParseableNumber<I>: FromStr + Display + From<u8> + Add<I, Output = I> + Mul<I, Output = I> {}
macro_rules! parseable {
    ($($t: ty),*) => {
        $(impl ParseableNumber<$t> for $t {})*
    };
}
parseable! {usize, u32, u64, isize, i32, i64}

pub fn parse_nums<I: ParseableNumber<I>>(l: &str) -> Vec<I> {
    l.lines().map(parse_num::<I>).collect()
}

pub fn parse_nums_comma<I: ParseableNumber<I>>(l: &str) -> Vec<I> {
    l.trim().split(',').map(parse_num::<I>).collect()
}

pub trait Splitting {
    fn before<'a>(&'a self, sep: &str) -> &'a str;
    fn after<'a>(&'a self, sep: &str) -> &'a str;
}

impl Splitting for &str {
    fn after<'a>(&'a self, sep: &str) -> &'a str {
        self.split_once(sep).unwrap().1
    }

    fn before<'a>(&'a self, sep: &str) -> &'a str {
        self.split_once(sep).unwrap().0
    }
}

pub trait Inc: Default + Copy + Step {
    fn inc(self) -> Self;
    fn dec(self) -> Self;
}

impl<T: Step + Default + Copy> Inc for T {
    fn inc(self) -> Self {
        T::forward(self, 1)
    }

    fn dec(self) -> Self {
        T::backward(self, 1)
    }
}

#[cfg(debug_assertions)]
pub fn parse_num<I: ParseableNumber<I>>(s: &str) -> I {
    s.parse().unwrap_or_else(|_| panic!("Invalid number {s}"))
}

// For benchmarks.
// This function assumes that the input will always be valid numbers and is UB otherwise
#[cfg(not(debug_assertions))]
pub fn parse_num<I: ParseableNumber<I>>(s: &str) -> I {
    let mut digits = s.bytes().map(|b| I::from(b - b'0'));
    let start = unsafe { digits.next().unwrap_unchecked() };
    digits.fold(start, |acc, n| acc * I::from(10) + n)
}

fn gcd(mut x: usize, mut y: usize) -> usize {
    let mut remainder;
    while y != 0 {
        remainder = x % y;
        x = y;
        y = remainder;
    }
    x
}

pub fn lcm(x: usize, y: usize) -> usize {
    x * y / gcd(x, y)
}
