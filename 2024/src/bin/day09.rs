#![feature(test)]
extern crate test;
use std::iter;

use aoc2024::{boilerplate, common::*};

const DAY: usize = 9;
type I = usize;
type Parsed = Vec<usize>;

fn parse_input(raw: &str) -> Parsed {
    raw.trim_end().bytes().map(|b| (b - b'0') as usize).collect()
}

fn build_disk(parsed: &Parsed) -> Vec<Option<I>> {
    let len = parsed.iter().sum();
    let mut disk = Vec::with_capacity(len);
    let mut gap = false;
    let mut id = 0;
    for &size in parsed {
        if gap {
            disk.extend(iter::repeat_n(None, size));
        } else {
            disk.extend(iter::repeat_n(Some(id), size));
            id += 1;
        }
        gap = !gap;
    }
    disk
}

fn part1(parsed: &Parsed) -> usize {
    let mut disk = build_disk(parsed);
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

fn part2(parsed: &Parsed) -> usize {
    let mut disk = Vec::new();
    let mut gap = false;
    let mut id = 0;
    for &size in parsed {
        if gap {
            disk.push((None, size));
        } else {
            disk.push((Some(id), size));
            id += 1;
        }
        gap = !gap;
    }
    let mut right_offset = 1;
    while right_offset < disk.len() - 1 {
        let src_index = disk.len() - right_offset;
        match disk[src_index] {
            (None, _) => right_offset += 1,
            (Some(_), src_size) => {
                for dst_index in 0..src_index {
                    match disk[dst_index] {
                        (None, dst_size) if dst_size > src_size => {
                            let remaining = dst_size - src_size;
                            disk[dst_index] = (None, remaining);
                            disk.insert(dst_index, disk[src_index]);
                            disk[src_index + 1] = (None, src_size);
                            break;
                        }
                        (None, s) if s == src_size => {
                            disk.swap(src_index, dst_index);
                            break;
                        }
                        _ => (),
                    }
                }
                right_offset += 1;
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
