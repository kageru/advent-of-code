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

fn build_disk<T, F: Fn((bool, I, I)) -> Iter, Iter: IntoIterator<Item = T>>(parsed: &Parsed, f: F) -> Vec<T> {
    parsed
        .iter()
        .zip(iter::successors(Some(true), |b| Some(!b))) // alternate between true and false
        .zip((0..).intersperse(0)) // block IDs
        .map(|((&size, is_file), id)| (is_file, id, size))
        .flat_map(f)
        .collect()
}

fn part1(parsed: &Parsed) -> I {
    let mut disk = build_disk(parsed, |(is_file, id, size)| iter::repeat_n(is_file.then_some(id), size));
    let mut from = disk.len() - 1;
    let mut to = 0;
    while from > to {
        match (disk[from], disk[to]) {
            (Some(_), None) => {
                disk.swap(to, from);
                from -= 1;
                to += 1;
            }
            (None, _) => from -= 1,
            (_, Some(_)) => to += 1,
        }
    }
    disk.into_iter().flatten().enumerate().map(|(i, id)| i * id).sum()
}

fn part2(parsed: &Parsed) -> I {
    let mut disk = build_disk(parsed, |(is_file, id, size)| iter::once((is_file.then_some(id), size)));
    for src_index in (2..disk.len()).rev() {
        match disk[src_index] {
            (None, _) => (),
            (Some(_), src_size) => {
                for dst_index in 1..src_index {
                    match disk[dst_index] {
                        (None, dst_size) if dst_size > src_size => {
                            disk[dst_index] = (None, dst_size - src_size);
                            disk.insert(dst_index, disk[src_index]);
                            disk[src_index + 1] = (None, src_size);
                        }
                        (None, dst_size) if dst_size == src_size => disk.swap(src_index, dst_index),
                        _ => continue,
                    }
                    break;
                }
            }
        }
    }
    disk.into_iter().flat_map(|(id, size)| iter::repeat_n(id.unwrap_or(0), size)).enumerate().map(|(i, n)| i * n).sum()
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
