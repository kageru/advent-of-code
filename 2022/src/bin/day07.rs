#![feature(test, if_let_guard)]
extern crate test;

use aoc2022::{boilerplate, common::*};

const DAY: usize = 7;

enum Node<'a> {
    File(usize),
    Dir(&'a str, Vec<Node<'a>>, usize),
}

impl<'a> Node<'a> {
    fn subdir_mut(&mut self, dir: &str) -> Option<&mut Self> {
        match self {
            Self::Dir(_, contents, _) => contents.iter_mut().find(|d| matches!(**d, Self::Dir(name, _, _) if name == dir)),
            _ => None,
        }
    }
}

fn parse_input(raw: &str) -> Node<'_> {
    let mut pwd = Vec::<&str>::new();
    let mut fs = Node::Dir("/", Vec::new(), 0);
    for cmd in raw.trim_start_matches("$ cd /\n$ ").split("\n$ ") {
        match cmd.bytes().next() {
            Some(b'c') => if cmd.ends_with('.') { pwd.pop(); } else { pwd.push(&cmd[3..]) },
            // ls
            _ if let Some(Node::Dir(_, contents, _)) = pwd.iter().try_fold(&mut fs, |cd, p| cd.subdir_mut(p)) => contents.extend(
                cmd.lines()
                    .filter_map(|l| l.split_once(' '))
                    .map(|line| match line {
                        ("dir", d) => Node::Dir(d, Vec::new(), 0),
                        (size, _) => Node::File(parse_num(size))
                    })
            ),
            _ => unreachable!()
        };
    }
    compute_dir_sizes(&mut fs);
    fs
}

fn compute_dir_sizes(node: &mut Node<'_>) -> usize {
    match node {
        Node::File(s) => *s,
        Node::Dir(_, c, size) => {
            *size = c.iter_mut().map(compute_dir_sizes).sum();
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
    TEST_INPUT == "\
$ cd /
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
    bench_parse: dir_name => "/",
}

#[cfg(test)]
fn dir_name<'a>(n: &Node<'a>) -> &'a str {
    match n {
        Node::Dir(name, _, _) => name,
        Node::File(_) => panic!("Not a directory"),
    }
}
