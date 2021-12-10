#![feature(test)]
extern crate test;

use aoc2021::common::*;

const DAY: usize = 10;
type Parsed<'a> = Vec<&'a str>;

fn parse_input(raw: &str) -> Parsed {
    raw.lines().collect()
}

fn solve(parsed: &Parsed) -> (usize, usize) {
    let mut points = 0;
    let mut p2_points = Vec::new();
    for &line in parsed {
        let mut stack = Vec::new();
        let mut invalid = false;
        for c in line.bytes() {
            match c {
                b'(' | b'[' | b'<' | b'{' => stack.push(c),
                b')' if stack.last() == Some(&b'(') => {
                    stack.pop();
                }
                b']' if stack.last() == Some(&b'[') => {
                    stack.pop();
                }
                b'>' if stack.last() == Some(&b'<') => {
                    stack.pop();
                }
                b'}' if stack.last() == Some(&b'{') => {
                    stack.pop();
                }
                b')' => {
                    points += 3;
                    invalid = true;
                    break;
                }
                b']' => {
                    points += 57;
                    invalid = true;
                    break;
                }
                b'}' => {
                    points += 1197;
                    invalid = true;
                    break;
                }
                b'>' => {
                    points += 25137;
                    invalid = true;
                    break;
                }
                _ => unreachable!(),
            }
        }
        if invalid {
            continue;
        }
        p2_points.push(autocomplete_points(stack));
    }
    let p2_len = p2_points.len();
    (points, *p2_points.select_nth_unstable(p2_len / 2).1)
}

fn autocomplete_points(stack: Vec<u8>) -> usize {
    let mut points = 0;
    for p in stack.into_iter().rev() {
        points *= 5;
        match p {
            b'(' => points += 1,
            b'[' => points += 2,
            b'{' => points += 3,
            b'<' => points += 4,
            _ => unreachable!(),
        }
    }
    points
}

fn main() {
    let raw = read_file(DAY);
    let input = parse_input(&raw);
    let (p1, p2) = solve(&input);
    println!("Part 1: {p1}");
    println!("Part 2: {p2}");
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2021::*;

    const TEST_INPUT: &str = "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]";

    #[test]
    fn test_autocomplete_points() {
        assert_eq!(autocomplete_points(vec![b'<', b'{', b'(', b'[']), 294);
    }

    #[test]
    fn part1_test() {
        let input = parse_input(TEST_INPUT);
        assert_eq!(solve(&input).0, 26397);
    }

    #[test]
    fn part2_test() {
        let input = parse_input(TEST_INPUT);
        assert_eq!(solve(&input).1, 288957);
    }

    #[bench]
    fn bench_solution(b: &mut test::Bencher) {
        let raw = read_file(DAY);
        let input = parse_input(&raw);
        b.iter(|| assert_eq!(solve(test::black_box(&input)), (358737, 4329504793)))
    }

    bench_input!(Vec::len => 90);
}
