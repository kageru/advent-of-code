#![feature(test)]
use std::collections::HashSet;
extern crate test;
use itertools::Itertools;

fn main() {
    let input = parse_input(&read_input());
    println!("Part 1: {}", part1(&input, "shiny gold", &mut HashSet::new()).len());
    println!("Part 2: {}", part2(&input, "shiny gold"));
}

#[derive(Debug)]
struct Bag {
    color:    String,
    contents: Vec<InnerBag>,
}

#[derive(Debug)]
struct InnerBag {
    qty:   usize,
    color: String,
}

// this is dumb
impl From<&str> for Bag {
    fn from(raw: &str) -> Self {
        let mut split = raw.split(", ");
        Self {
            color:    split.next().unwrap().to_string(),
            contents: split
                .filter_map(|s| {
                    if s.starts_with("no other") {
                        None
                    } else {
                        let mut split = s.splitn(2, ' ');
                        Some(InnerBag {
                            qty:   split.next().unwrap().parse().unwrap(),
                            color: split.next().unwrap().to_owned(),
                        })
                    }
                })
                .collect(),
        }
    }
}

fn read_input() -> String {
    std::fs::read_to_string("input").unwrap()
}

fn part1<'a>(bags: &[Bag], color: &str, seen: &'a mut HashSet<String>) -> &'a mut HashSet<String> {
    for bag in bags.iter().filter(|bag| bag.contents.iter().find(|b| b.color == color).is_some()) {
        seen.insert(bag.color.clone());
        part1(bags, &bag.color, seen);
    }
    seen
}

fn part2(bags: &[Bag], color: &str) -> usize {
    bags.iter()
        .filter(|bag| bag.color == color)
        .map(|bag| bag.contents.iter().map(|c| (part2(bags, &c.color) * c.qty) + c.qty).sum::<usize>())
        .sum()
}

fn parse_input(s: &str) -> Vec<Bag> {
    s.replace(" bags contain", ",")
        .replace(" bags", "")
        .replace(" bag", "")
        .replace('.', "")
        .lines()
        .map_into()
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::black_box;

    const TEST_INPUT: &str = "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.";

    #[test]
    fn part1_test() {
        let input = parse_input(TEST_INPUT);
        assert_eq!(part1(&input, "shiny gold", &mut HashSet::new()).len(), 4);
    }

    const TEST_INPUT_2: &str = "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.";

    #[test]
    fn part2_test() {
        let input = parse_input(TEST_INPUT);
        assert_eq!(part2(&input, "shiny gold"), 32);
        let input = parse_input(TEST_INPUT_2);
        assert_eq!(part2(&input, "shiny gold"), 126);
    }

    #[bench]
    fn bench_input_parsing(b: &mut test::Bencher) {
        let raw = read_input();
        b.iter(|| assert_eq!(parse_input(black_box(&raw)).len(), 594))
    }

    #[bench]
    fn bench_part1(b: &mut test::Bencher) {
        let bags = parse_input(&read_input());
        b.iter(|| assert_eq!(part1(black_box(&bags), "shiny gold", &mut HashSet::new()).len(), 226))
    }

    #[bench]
    fn bench_part2(b: &mut test::Bencher) {
        let bags = parse_input(&read_input());
        b.iter(|| assert_eq!(part2(black_box(&bags), "shiny gold"), 9569))
    }
}
