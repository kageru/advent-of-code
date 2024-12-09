#![feature(test, iter_intersperse)]
extern crate test;
use aoc2024::{boilerplate, common::*};
use std::iter;

const DAY: I = 9;
type I = usize;
type Parsed = Vec<I>;

fn parse_input(raw: &str) -> Parsed {
    raw.trim_end().bytes().map(|b| (b - b'0') as I).collect()
}

fn build_disk<T, F: Fn((I, &I)) -> Iter, Iter: IntoIterator<Item = T>>(parsed: &Parsed, f: F) -> Vec<T> {
    (0..).intersperse(0).zip(parsed).flat_map(f).collect()
}

fn part1(parsed: &Parsed) -> I {
    let mut disk = build_disk(parsed, |(id, &size)| iter::repeat_n(id, size));
    let mut from = disk.len() - 1;
    let mut to = parsed[0];
    while from > to {
        match (disk[from], disk[to]) {
            (1.., 0) => {
                disk.swap(to, from);
                from -= 1;
                to += 1;
            }
            (0, _) => from -= 1,
            (_, 1..) => to += 1,
        }
    }
    disk.into_iter().enumerate().map(|(i, id)| i * id).sum()
}

fn part2(parsed: &Parsed) -> I {
    let mut disk = build_disk(parsed, |(id, &size)| iter::once((id, size)));
    for src_index in (2..disk.len()).rev() {
        if let (1.., src_size) = disk[src_index] {
            for dst_index in 1..src_index {
                match disk[dst_index] {
                    (0, dst_size) if dst_size > src_size => {
                        disk[dst_index] = (0, dst_size - src_size);
                        disk.insert(dst_index, disk[src_index]);
                        disk[src_index + 1] = (0, src_size);
                    }
                    (0, dst_size) if dst_size == src_size => disk.swap(src_index, dst_index),
                    _ => continue,
                }
                break;
            }
        }
    }
    disk.into_iter().flat_map(|(id, size)| iter::repeat_n(id, size)).enumerate().map(|(i, n)| i * n).sum()
}

boilerplate! {
    TEST_INPUT == "2333133121414131402"
    for tests: {
        part1: { TEST_INPUT => 1928 },
        part2: { TEST_INPUT => 2858 },
    },
    bench1 == 6283170117911,
    bench2 == 6307653242596,
    bench_parse: Vec::len => 19999,
}
