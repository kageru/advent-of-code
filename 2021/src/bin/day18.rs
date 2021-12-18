#![feature(test)]
extern crate test;
use aoc2021::common::*;
use itertools::Itertools;
use std::{fmt, ops::Add};

const DAY: usize = 18;
type Parsed = Vec<Node>;

impl Add for Node {
    type Output = Node;
    fn add(self, rhs: Self) -> Self::Output {
        Node::Pair(Box::new((self, rhs)))
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Node {
    Number(usize),
    Pair(Box<(Node, Node)>),
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
        (first + second, offset + offset2 + 2 /* 1 for the opening [ and 1 for the comma */)
    } else {
        let n = raw.as_bytes()[0] - b'0';
        debug_assert!(n <= 9);
        (Node::Number(n as _), 2)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Explosion {
    None,
    Partial,
    Full,
}

impl Node {
    fn reduce(mut self) -> Self {
        while self.explode() || self.split() {}
        self
    }

    fn explode(&mut self) -> bool {
        self.explode_inner(0, &mut None, &mut None, Explosion::None) != Explosion::None
    }

    fn split(&mut self) -> bool {
        match self {
            Node::Number(n) if *n >= 10 => {
                *self = Node::Number(*n / 2) + Node::Number(*n / 2 + (*n & 1));
                true
            }
            Node::Pair(p) => p.0.split() || p.1.split(),
            _ => false,
        }
    }

    fn magnitude(&self) -> usize {
        match self {
            Node::Number(n) => *n,
            Node::Pair(p) => 3 * p.0.magnitude() + 2 * p.1.magnitude(),
        }
    }

    fn explode_inner<'a, 'b>(
        &'a mut self,
        depth: usize,
        previous_number: &'b mut Option<&'a mut usize>,
        for_next: &'b mut Option<usize>,
        state: Explosion,
    ) -> Explosion {
        match (self, &for_next) {
            (Node::Number(n), Some(x)) => {
                *n += x;
                *for_next = None;
                Explosion::Full
            }
            (Node::Number(n), None) => {
                *previous_number = Some(n);
                state
            }
            (s @ Node::Pair(_), _) if depth == 4 && state == Explosion::None => {
                let (&left, &right) = s.number_pair_or_panic();
                if let Some(prev) = previous_number {
                    **prev += left;
                }
                *for_next = Some(right);
                *s = Node::Number(0);
                Explosion::Partial
            }
            (Node::Pair(p), _) => match p.0.explode_inner(depth + 1, previous_number, for_next, state) {
                f @ Explosion::Full => f,
                e => p.1.explode_inner(depth + 1, previous_number, for_next, e),
            },
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
    add_and_reduce(parsed.clone()).unwrap().magnitude()
}

fn add_and_reduce(parsed: Parsed) -> Option<Node> {
    parsed.into_iter().reduce(move |acc, new| (acc + new).reduce())
}

fn part2(parsed: &Parsed) -> usize {
    parsed.iter().permutations(2).map(|n| (n[0].clone() + n[1].clone()).reduce().magnitude()).max().unwrap()
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

    const OTHER_TEST_INPUT: &str = "[1,2]
[[1,2],3]
[9,[8,7]]
[[1,9],[8,5]]
[[[[1,2],[3,4]],[[5,6],[7,8]]],9]
[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]";

    const TEST_INPUT: &str = "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]";

    #[test]
    fn test_example_parsing() {
        let [first, second]: [Node; 2] = parse_input(TEST_INPUT_SINGLE_ADDITION).try_into().unwrap();
        assert_eq!(
            first,
            (((Node::Number(4) + Node::Number(3)) + Node::Number(4)) + Node::Number(4))
                + (Node::Number(7) + ((Node::Number(8) + Node::Number(4)) + Node::Number(9)))
        );
        assert_eq!(second, (Node::Number(1) + Node::Number(1)));
    }

    #[test]
    fn test_node_display() {
        for (actual, expected) in parse_input(OTHER_TEST_INPUT).into_iter().zip(OTHER_TEST_INPUT.lines()) {
            assert_eq!(expected, actual.to_string());
        }
    }

    #[test_case("[[[[[9,8],1],2],3],4]" => "[[[[0,9],2],3],4]")]
    #[test_case("[7,[6,[5,[4,[3,2]]]]]" => "[7,[6,[5,[7,0]]]]")]
    #[test_case("[[6,[5,[4,[3,2]]]],1]" => "[[6,[5,[7,0]]],3]")]
    #[test_case("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" => "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")]
    #[test_case("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]" => "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")]
    #[test_case("[[[[4,0],[5,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]" => "[[[[4,0],[5,4]],[[0,[7,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]")]
    fn test_single_explosion(raw: &str) -> String {
        let mut i = parse_node(raw).0;
        assert!(i.explode());
        i.to_string()
    }

    #[test_case("[[1,2],[[3,4],5]]" => 143)]
    #[test_case("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]" => 1384)]
    #[test_case("[[[[1,1],[2,2]],[3,3]],[4,4]]" => 445)]
    #[test_case("[[[[3,0],[5,3]],[4,4]],[5,5]]" => 791)]
    #[test_case("[[[[5,0],[7,4]],[5,5]],[6,6]]" => 1137)]
    #[test_case("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]" => 3488)]
    #[test_case("[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]" => 4140)]
    fn test_magnitude(raw: &str) -> usize {
        parse_node(raw).0.magnitude()
    }

    #[test]
    fn test_full_chain() {
        let lhs = parse_node("[[[[4,3],4],4],[7,[[8,4],9]]]").0;
        let rhs = parse_node("[1,1]").0;
        let mut res = lhs + rhs;
        assert_eq!(res.to_string(), "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]");

        let mut res2 = res.clone();

        res.explode();
        assert_eq!(res.to_string(), "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]");
        res.explode();
        assert_eq!(res.to_string(), "[[[[0,7],4],[15,[0,13]]],[1,1]]");
        res.split();
        assert_eq!(res.to_string(), "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]");
        res.split();
        assert_eq!(res.to_string(), "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]");
        res.explode();
        assert_eq!(res.to_string(), "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]");
        // should be done now
        res = res.reduce();
        assert_eq!(res.to_string(), "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]");

        // now again using .reduce() from the beginning
        res2 = res2.reduce();
        assert_eq!(res, res2);
    }

    #[test]
    fn test_single_reduction() {
        assert_eq!(
            parse_node("[[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]").0.reduce().to_string(),
            "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"
        );
    }

    #[test_case("[1,1]\n[2,2]\n[3,3]\n[4,4]" => "[[[[1,1],[2,2]],[3,3]],[4,4]]")]
    #[test_case("[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]" => "[[[[3,0],[5,3]],[4,4]],[5,5]]")]
    #[test_case("[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]\n[6,6]" => "[[[[5,0],[7,4]],[5,5]],[6,6]]")]
    #[test_case("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]" => "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")]
    fn test_list_reduction(raw: &str) -> String {
        let parsed = parse_input(raw);
        add_and_reduce(parsed).unwrap().to_string()
    }

    test!(part1() == 4140);
    test!(part2() == 3993);
    bench!(part1() == 4173);
    bench!(part2() == 4706);
    bench_input!(Vec::len => 100);
}
