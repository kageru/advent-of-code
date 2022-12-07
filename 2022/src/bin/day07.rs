#![feature(test)]
extern crate test;
use std::ops::{Index, IndexMut};

use aoc2022::{boilerplate, common::*};

const DAY: usize = 7;

enum Node<'a> {
    File(&'a str, usize),
    Dir(&'a str, Vec<Box<Node<'a>>>, usize),
}

impl<'a> IndexMut<&str> for Node<'a> {
    fn index_mut(&mut self, index: &str) -> &mut Node<'a> {
        match self {
            Self::Dir(_, contents, _) => {
                contents.iter_mut().find(|d| matches!(***d, Self::Dir(name, _, _) if name == index)).expect("File not found")
            }
            Self::File(name, _) => panic!("Can’t index into a file ({name})"),
        }
    }
}

impl<'a> Index<&str> for Node<'a> {
    type Output = Self;

    fn index(&self, index: &str) -> &Self::Output {
        match self {
            Self::Dir(_, contents, _) => {
                contents.iter().find(|&d| matches!(**d, Self::Dir(name, _, _) if name == index)).expect("File not found")
            }
            Self::File(name, _) => panic!("Can’t index into a file ({name})"),
        }
    }
}

fn parse_input(raw: &str) -> Node<'_> {
    let mut pwd = Vec::<&str>::new();
    let mut fs = Node::Dir("/", Vec::new(), 0);
    for cmd in raw.trim_start_matches("$ cd /\n$ ").split("$ ") {
        let mut lines = cmd.lines();
        match lines.next().and_then(|s| s.split_once(' ')) {
            Some(("cd", "..")) => {
                pwd.pop();
            }
            Some(("cd", dir)) => pwd.push(dir),
            // ls
            _ => {
                let mut cd = &mut fs;
                for p in pwd.iter() {
                    cd = &mut cd[p];
                }
                match cd {
                    Node::Dir(_, contents, _) => {
                        for line in lines {
                            match line.split_once(' ') {
                                Some(("dir", d)) => contents.push(Box::new(Node::Dir(d, Vec::new(), 0))),
                                Some((size, name)) => contents.push(Box::new(Node::File(name, size.parse().unwrap()))),
                                _ => unreachable!("Invalid directory contents: {line}"),
                            }
                        }
                    }
                    Node::File(_, _) => unreachable!("Can’t `ls` file contents"),
                }
            }
        };
    }
    compute_dir_size(&mut fs);
    fs
}

fn compute_dir_size(node: &mut Node<'_>) -> usize {
    match node {
        Node::File(_, s) => *s,
        Node::Dir(_, c, size) => {
            *size = c.iter_mut().map(|d| compute_dir_size(d)).sum();
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
    bench2 == 0,
    bench_parse: node_name => "/",
}

#[cfg(test)]
fn node_name<'a>(n: &Node<'a>) -> &'a str {
    match n {
        Node::Dir(name, _, _) => name,
        Node::File(name, _) => name,
    }
}
