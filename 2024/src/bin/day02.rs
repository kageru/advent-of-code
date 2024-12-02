#![feature(test, array_windows)]
extern crate test;
use aoc2024::{boilerplate, common::*};

const DAY: usize = 2;
type I = u32;
type Parsed = Vec<Vec<I>>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines().map(|l| parse_nums_separator(l, ' ')).collect()
}

fn part1(parsed: &Parsed) -> usize {
    parsed
        .iter()
        .filter(|l| {
            l.array_windows().all(|&[a, b]| (1..=3).contains(&a.abs_diff(b))) && (l.iter().is_sorted() || l.iter().rev().is_sorted())
        })
        .count()
}

fn report_valid_inner<const DAMPENER: bool>(report: &Vec<u32>, asc: bool) -> bool {
    for (i, &[a, b]) in report.array_windows().enumerate() {
        if (asc && a > b) || (!asc && a < b) || !(1..=3).contains(&a.abs_diff(b)) {
            return DAMPENER
                // We don’t know if the current or next element has to be removed, so we just try both.
                && ({
                    let mut r1 = report.clone();
                    r1.remove(i);
                    report_valid_inner::<false>(&r1, asc)
                } || {
                    let mut r2 = report.clone();
                    r2.remove(i + 1);
                    report_valid_inner::<false>(&r2, asc)
                });
        }
    }
    true
}

fn report_valid(report: &&Vec<u32>) -> bool {
    // if 2/3 of these are ascending, we know the report is at least supposed to be ascending.
    let asc = [report[0] < report[1], report[1] < report[2], report[2] < report[3]].into_iter().filter(|&b| b).count() >= 2;
    report_valid_inner::<true>(report, asc)
}

fn part2(parsed: &Parsed) -> usize {
    parsed.iter().filter(report_valid).count()
}

boilerplate! {
    TEST_INPUT == "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"
    for tests: {
        part1: { TEST_INPUT => 2 },
        part2: { TEST_INPUT => 4, },
    },
    bench1 == 442,
    bench2 == 493,
    bench_parse: Vec::len => 1000,
}

#[cfg(test)]
mod tests2 {
    use super::*;

    #[test]
    fn report_valid_test() {
        assert!(report_valid(&&vec![83, 76, 75, 72, 69]), "1");
        assert!(report_valid(&&vec![76, 75, 72, 69, 60]), "2");
        assert!(report_valid(&&vec![74, 75, 72, 69]), "3");
        assert!(report_valid(&&vec![76, 72, 73, 70, 67]), "4");
    }
}
