#![feature(test)]
extern crate test;
use aoc2020::common::*;
use lazy_static::lazy_static;
use regex::Regex;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
struct Passport {
    byr: usize,
    iyr: usize,
    eyr: usize,
    hgt: String,
    hcl: String,
    ecl: String,
    pid: String, // can’t be usize because yaml doesn’t like 0-leading numbers
    cid: Option<usize>,
}

fn read_input() -> String {
    read_file(4)
}

/// When I first saw the input and puzzle, I thought
/// “well, this is stupid, so I’ll do something stupid and parse it as yaml with serde”.
/// That decision was objectively awful and not even worth it for the meme,
/// but I’m too stubborn to give up now.
/// Also, I didn’t know the yaml spec was *that* broken. Fucking hell.
fn parse_input(s: &str) -> Vec<Passport> {
    s.replace(' ', "\n")
        .replace(':', ": ")
        .replace('#', "§") // # is a comment in yaml rooDerp
        .split("\n\n")
        .map(serde_yaml::from_str)
        .filter_map(Result::ok)
        .collect()
}

fn validate_height(hgt: &str) -> bool {
    if let Some(cm) = hgt.strip_suffix("cm").and_then(|s| s.parse::<usize>().ok()) {
        (150..=193).contains(&cm)
    } else if let Some(inch) = hgt.strip_suffix("in").and_then(|s| s.parse::<usize>().ok()) {
        (59..=76).contains(&inch)
    } else {
        false
    }
}

lazy_static! {
    static ref HCL_REGEX: Regex = Regex::new("§[0-9a-f]{6}").unwrap();
    static ref ECL_REGEX: Regex = Regex::new("(amb|blu|brn|gry|grn|hzl|oth)").unwrap();
    static ref PID_REGEX: Regex = Regex::new(r"^\d{9}$").unwrap();
}

fn part2(ps: &[Passport]) -> usize {
    ps.iter()
        .filter(|p| {
            (1920..=2002).contains(&p.byr)
                && (2010..=2020).contains(&p.iyr)
                && (2020..=2030).contains(&p.eyr)
                && validate_height(&p.hgt)
                && HCL_REGEX.is_match(&p.hcl)
                && ECL_REGEX.is_match(&p.ecl)
                && PID_REGEX.is_match(&p.pid)
        })
        .count()
}

fn main() {
    let i = parse_input(&read_input());
    println!("Part 1: {}", i.len());
    println!("Part 2: {}", part2(&i));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2020::*;
    use paste::paste;
    use test::black_box;

    const TEST_INPUT: &str = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in";

    #[test]
    fn part1_test() {
        assert_eq!(parse_input(TEST_INPUT).len(), 2);
    }

    const P2_INVALID: &str = "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007";

    const P2_VALID: &str = "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719";

    #[test]
    fn part2_test() {
        let valid = parse_input(P2_VALID);
        assert_eq!(part2(&valid), valid.len());

        let invalid = parse_input(P2_INVALID);
        assert_eq!(part2(&invalid), 0);
    }

    bench_input!(len == 235);
    bench!(part2() == 194);
}
