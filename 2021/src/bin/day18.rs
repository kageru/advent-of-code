#![feature(box_syntax)]
#![feature(test)]
extern crate test;
use aoc2021::common::*;
use std::{fmt, ops::Add};

const DAY: usize = 18;
type Parsed = Vec<Node>;

#[derive(Debug, PartialEq)]
enum Node {
    Number(usize),
    Pair(Box<(Node, Node)>),
}

impl Add for Node {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Node::Pair(box (self, rhs))
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Node::Number(n) => write!(f, "{n}"),
            Node::Pair(p) => write!(f, "[{},{}]", p.0, p.1),
        }
    }
}

fn parse_input(raw: &str) -> Parsed {
    raw.lines().map(|l| parse_node(l).0).collect()
}

fn parse_node(raw: &str) -> (Node, usize) {
    if let Some(inner) = raw.strip_prefix('[') {
        let (first, offset) = parse_node(inner);
        let (second, offset2) = parse_node(&inner[offset..]);
        (Node::Pair(box (first, second)), 1 /* the opening [ */ + offset + offset2 + 1 /* the comma */)
    } else {
        let n = raw.as_bytes()[0] - b'0';
        debug_assert!(n <= 9, "Number was {n}, raw was {raw}");
        (Node::Number(n as _), 2)
    }
}

#[derive(Debug, PartialEq)]
enum Reduction {
    None,
    Partial,
    Full,
}

impl Node {
    fn explode(&mut self) -> bool {
        self.explode_inner(0, &mut None, &mut None) != Reduction::None
    }

    fn explode_inner<'a, 'b>(
        &'a mut self,
        depth: usize,
        previous_number: &'b mut Option<&'a mut usize>,
        for_next: &'b mut Option<usize>,
    ) -> Reduction {
        match (self, &for_next) {
            (Node::Number(n), Some(x)) => {
                *n += x;
                *for_next = None;
                Reduction::Full
            }
            (Node::Number(n), None) => {
                *previous_number = Some(n);
                Reduction::None
            }
            (s @ Node::Pair(_), _) if depth == 4 => {
                let (left, right) = s.number_pair_or_panic();
                if let Some(prev) = previous_number {
                    **prev += left;
                }
                *for_next = Some(*right);
                *s = Node::Number(0);
                Reduction::Partial
            }
            (Node::Pair(p), _) => {
                let left = p.0.explode_inner(depth + 1, previous_number, for_next);
                if left != Reduction::Full {
                    p.1.explode_inner(depth + 1, previous_number, for_next)
                } else {
                    left
                }
            }
        }
    }

    fn number_pair_or_panic(&self) -> (&usize, &usize) {
        match &self {
            Node::Pair(p) => (p.0.number_or_panic(), p.1.number_or_panic()),
            _ => unreachable!(),
        }
    }

    fn number_or_panic(&self) -> &usize {
        match &self {
            Node::Number(n) => n,
            _ => unreachable!(),
        }
    }
}

fn part1(parsed: &Parsed) -> usize {
    unimplemented!()
}

fn part2(parsed: &Parsed) -> usize {
    unimplemented!()
}

fn main() {
    let input = parse_input(&read_file(DAY));
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    use aoc2021::*;
    use test_case::test_case;

    const TEST_INPUT_SINGLE_ADDITION: &str = "[[[[4,3],4],4],[7,[[8,4],9]]]
[1,1]";

    const TEST_INPUT: &str = "[1,2]
[[1,2],3]
[9,[8,7]]
[[1,9],[8,5]]
[[[[1,2],[3,4]],[[5,6],[7,8]]],9]
[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]";

    #[test]
    fn test_example_parsing() {
        let [first, second]: [Node; 2] = parse_input(TEST_INPUT_SINGLE_ADDITION).try_into().unwrap();
        assert_eq!(
            first,
            Node::Pair(box (
                Node::Pair(box (Node::Pair(box (Node::Pair(box (Node::Number(4), Node::Number(3))), Node::Number(4))), Node::Number(4))),
                Node::Pair(box (Node::Number(7), Node::Pair(box (Node::Pair(box (Node::Number(8), Node::Number(4),)), Node::Number(9),))))
            ))
        );
        assert_eq!(second, Node::Pair(box (Node::Number(1), Node::Number(1))));
    }

    #[test]
    fn test_node_display() {
        for (actual, expected) in parse_input(TEST_INPUT).into_iter().zip(TEST_INPUT.lines()) {
            assert_eq!(expected, actual.to_string());
        }
    }

    #[test_case("[[[[[9,8],1],2],3],4]" => "[[[[0,9],2],3],4]")]
    #[test_case("[7,[6,[5,[4,[3,2]]]]]" => "[7,[6,[5,[7,0]]]]")]
    #[test_case("[[6,[5,[4,[3,2]]]],1]" => "[[6,[5,[7,0]]],3]")]
    #[test_case("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" => "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")]
    #[test_case("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]" => "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")]
    fn test_single_explosion(raw: &str) -> String {
        let mut i = parse_node(raw).0;
        assert!(i.explode());
        i.to_string()
    }

    /*
    test!(part1() == 0);
    test!(part2() == 0);
    bench!(part1() == 0);
    bench!(part2() == 0);
    bench_input!(Vec::len => 0);
    */
}
