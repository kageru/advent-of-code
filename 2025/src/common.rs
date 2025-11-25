extern crate test;
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

pub fn parse_nums_separator<I: ParseableNumber<I>>(l: &str, s: char) -> Vec<I> {
    l.trim().split(s).map(parse_num::<I>).collect()
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
    fn inc_checked(self) -> Option<Self>;
    fn dec_checked(self) -> Option<Self>;
}

impl<T: Step + Default + Copy> Inc for T {
    fn inc(self) -> Self {
        T::forward(self, 1)
    }

    fn dec(self) -> Self {
        T::backward(self, 1)
    }

    fn inc_checked(self) -> Option<Self> {
        T::forward_checked(self, 1)
    }

    fn dec_checked(self) -> Option<Self> {
        T::backward_checked(self, 1)
    }
}

#[cfg(debug_assertions)]
pub fn parse_num<I: ParseableNumber<I>>(s: &str) -> I {
    s.parse().unwrap_or_else(|_| panic!("Invalid number “{s}”"))
}

// For benchmarks.
// This function assumes that the input will always be valid numbers and is UB otherwise
#[cfg(not(debug_assertions))]
pub fn parse_num<I: ParseableNumber<I>>(s: &str) -> I {
    s.bytes().fold(0.into(), |acc, n| acc * I::from(10) + (I::from(n - b'0')))
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

#[cfg(test)]
mod benches {
    use std::hint::black_box;

    use super::*;

    #[bench]
    fn bench_lcm(b: &mut test::Bencher) {
        b.iter(|| assert_eq!(lcm(black_box(20513), black_box(18827)), 1374371))
    }

    #[bench]
    fn bench_lcm_big(b: &mut test::Bencher) {
        b.iter(|| assert_eq!(lcm(black_box(169433831251), black_box(22199)), 13385272668829))
    }
}
