pub fn read_file(day: usize) -> String {
    std::fs::read_to_string(std::env::var("AOC_INPUT").unwrap_or_else(|_| format!("inputs/day{:0>2}", day))).unwrap()
}

pub fn parse_nums(l: &str) -> Vec<usize> {
    l.lines().map(parse_num).collect()
}

pub fn parse_nums_comma(l: &str) -> Vec<usize> {
    l.trim().split(',').map(parse_num).collect()
}

#[cfg(debug_assertions)]
pub fn parse_num<T: std::str::FromStr + std::fmt::Display>(s: &str) -> T {
    s.parse().unwrap_or_else(|_| panic!("Invalid number {s}"))
}

// For benchmarks.
// This function assumes that the input will always be valid numbers and is UB otherwise
#[cfg(not(debug_assertions))]
pub fn parse_num<T: From<u8> + std::ops::Add<T, Output = T> + std::ops::Mul<T, Output = T>>(s: &str) -> T {
    let mut digits = s.bytes().map(|b| T::from(b - b'0'));
    let start = unsafe { digits.next().unwrap_unchecked() };
    digits.fold(start, |acc, n| acc * T::from(10) + n)
}
