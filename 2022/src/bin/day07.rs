#![feature(test)]
extern crate test;
use std::ops::{Index, IndexMut};

use aoc2022::{boilerplate, common::*};

const DAY: usize = 7;

enum Node<'a> {
    File(&'a str, usize),
    Dir(&'a str, Vec<Box<Node<'a>>>),
}

impl<'a> IndexMut<&str> for Node<'a> {
    fn index_mut(&mut self, index: &str) -> &mut Node<'a> {
        match self {
            Self::Dir(_, contents) => {
                contents.iter_mut().find(|d| matches!(***d, Self::Dir ( name,_) if name == index)).expect("File not found")
            }
            Self::File(name, _) => panic!("Can’t index into a file ({name})"),
        }
    }
}

impl<'a> Index<&str> for Node<'a> {
    type Output = Self;

    fn index(&self, index: &str) -> &Self::Output {
        match self {
            Self::Dir(_, contents) => {
                contents.iter().find(|&d| matches!(**d, Self::Dir ( name,_) if name == index)).expect("File not found")
            }
            Self::File(name, _) => panic!("Can’t index into a file ({name})"),
        }
    }
}

fn parse_input(raw: &str) -> Node<'_> {
    let mut pwd = Vec::<&str>::new();
    let mut fs = Node::Dir("/", Vec::new());
    for cmd in raw.trim_start_matches("$ cd /\n$ ").split("$ ") {
        // println!("Now at: {cmd}");
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
                    Node::Dir(_, contents) => {
                        for line in lines {
                            match line.split_once(' ') {
                                Some(("dir", d)) => contents.push(Box::new(Node::Dir(d, Vec::new()))),
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
    fs
}

fn part1(parsed: &Node<'_>) -> usize {
    let mut n = 0;
    node_size(parsed, &mut n);
    n
}

fn node_size(node: &Node<'_>, n: &mut usize) -> usize {
    match node {
        Node::File(_, s) => *s,
        Node::Dir(_, c) => {
            let dir_size = c.iter().map(|d| node_size(d, n)).sum();
            if dir_size <= 100_000 {
                *n += dir_size;
            }
            dir_size
        }
    }
}

fn part2(parsed: &Node<'_>) -> usize {
    unimplemented!()
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
        Node::Dir(name, _) => name,
        Node::File(name, _) => name,
    }
}
