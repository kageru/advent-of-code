#![feature(test)]
extern crate test;
use aoc2021::common::*;

const DAY: usize = 10;

fn solve(input: &str) -> (usize, usize) {
    let mut p1_score = 0;
    let mut p2_scores = Vec::new();
    let mut stack = Vec::new();
    for line in input.lines() {
        match is_well_formed(line, &mut stack) {
            Ok(s) => p2_scores.push(autocomplete_points(s)), // <- clears the stack internally
            Err(p) => {
                p1_score += p;
                stack.clear();
            }
        }
    }
    let p2_len = p2_scores.len();
    (p1_score, *p2_scores.select_nth_unstable(p2_len / 2).1)
}

fn is_well_formed<'a>(line: &str, stack: &'a mut Vec<u8>) -> Result<&'a mut Vec<u8>, usize> {
    for c in line.bytes() {
        match c {
            b'(' | b'[' | b'<' | b'{' => stack.push(c),
            b']' | b'}' | b'>' if stack.last().unwrap() + 2 == c => {
                stack.pop();
            }
            b')' if stack.last().unwrap() == &b'(' => {
                stack.pop();
            }
            b')' => return Err(3),
            b']' => return Err(57),
            b'}' => return Err(1197),
            b'>' => return Err(25137),
            _ => unreachable!(),
        }
    }
    Ok(stack)
}

fn autocomplete_points(stack: &mut Vec<u8>) -> usize {
    let mut points = 0;
    for p in stack.drain(..).rev() {
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
    let (p1, p2) = solve(&raw);
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

    #[bench]
    fn bench_autocomplete_points(b: &mut test::Bencher) {
        let sample_stack = vec![b'<', b'{', b'(', b'['];
        b.iter(|| assert_eq!(autocomplete_points(test::black_box(&mut sample_stack.clone())), 294));
    }

    #[test]
    fn part1_test() {
        assert_eq!(solve(TEST_INPUT).0, 26397);
    }

    #[test]
    fn part2_test() {
        assert_eq!(solve(TEST_INPUT).1, 288957);
    }

    #[bench]
    fn bench_solution(b: &mut test::Bencher) {
        let raw = read_file(DAY);
        b.iter(|| assert_eq!(solve(test::black_box(&raw)), (358737, 4329504793)))
    }
}
