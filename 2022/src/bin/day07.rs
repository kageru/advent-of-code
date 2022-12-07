#![feature(test, if_let_guard, iter_collect_into)]
extern crate test;

use aoc2022::{boilerplate, common::*};

const DAY: usize = 7;

enum Node<'a> {
    File(&'a str, usize),
    Dir(&'a str, Vec<Node<'a>>, usize),
}

impl<'a> Node<'a> {
    /// Implementing this rather than IndexMut because that requires a mostly redundant
    /// implementation of Index which is more boilerplate.
    fn subdir_mut(&mut self, dir: &str) -> &mut Self {
        match self {
            Self::Dir(_, contents, _) => {
                contents.iter_mut().find(|d| matches!(**d, Self::Dir(name, _, _) if name == dir)).expect("File not found")
            }
            Self::File(name, _) => panic!("Can't index into a file ({name})"),
        }
    }
}

fn parse_input(raw: &str) -> Node<'_> {
    let mut pwd = Vec::<&str>::new();
    let mut fs = Node::Dir("/", Vec::new(), 0);
    for cmd in raw.trim_start_matches("$ cd /\n$ ").split("$ ") {
        let mut lines = cmd.lines();
        match lines.next().and_then(|s| s.split_once(' ')) {
            Some(("cd", "..")) => drop(pwd.pop()),
            Some(("cd", dir)) => pwd.push(dir),
            // ls
            _ if let Node::Dir(_, contents, _) = pwd.iter().fold(&mut fs, |cd, p| cd.subdir_mut(p)) => {
                lines
                    .filter_map(|l| l.split_once(' '))
                    .map(|line| match line {
                        ("dir", d) => Node::Dir(d, Vec::new(), 0),
                        (size, name) => Node::File(name, size.parse().unwrap())
                    })
                    .collect_into(contents);
            }
            _ => unreachable!()
        };
    }
    compute_dir_sizes(&mut fs);
    fs
}

fn compute_dir_sizes(node: &mut Node<'_>) -> usize {
    match node {
        Node::File(_, s) => *s,
        Node::Dir(_, c, size) => {
            *size = c.iter_mut().map(|d| compute_dir_sizes(d)).sum();
            *size
        }
    }
}

fn part1(parsed: &Node<'_>) -> usize {
    let mut sizes = Vec::new();
    dir_sizes(parsed, &mut sizes);
    sizes.into_iter().filter(|&s| s <= 100_000).sum()
}

fn dir_sizes(node: &Node<'_>, sizes: &mut Vec<usize>) {
    if let Node::Dir(_, contents, size) = node {
        sizes.push(*size);
        for c in contents {
            dir_sizes(c, sizes);
        }
    }
}

fn part2(parsed: &Node<'_>) -> usize {
    let needed_space = 30000000 - (70000000 - if let Node::Dir(_, _, s) = parsed { s } else { unreachable!() });
    let mut sizes = Vec::new();
    dir_sizes(parsed, &mut sizes);
    sizes.into_iter().filter(|&s| s >= needed_space).min().unwrap()
}

boilerplate! {
    TEST_INPUT == "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k",
    tests: {
        part1: { TEST_INPUT => 95437 },
        part2: { TEST_INPUT => 24933642 },
    },
    bench1 == 1667443,
    bench2 == 8998590,
    bench_parse: node_name => "/",
}

#[cfg(test)]
fn node_name<'a>(n: &Node<'a>) -> &'a str {
    match n {
        Node::Dir(name, _, _) => name,
        Node::File(name, _) => name,
    }
}
