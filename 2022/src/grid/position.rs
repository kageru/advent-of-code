extern crate test;
use super::direction::*;
use std::{
    convert::TryInto,
    hash::Hash,
    ops::{Add, Mul, Sub},
};

#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub struct PositionND<const DIMS: usize>(pub [i64; DIMS]);

pub type Position2D = PositionND<2>;

impl<I, const D: usize> From<[I; D]> for PositionND<D>
where I: TryInto<i64> + Copy
{
    fn from(s: [I; D]) -> Self {
        let mut points = [0; D];
        for i in 0..D {
            points[i] = s[i].try_into().unwrap_or_else(|_| panic!("number did not fit in target type"))
        }
        Self(points)
    }
}

pub const fn num_neighbors(d: usize) -> usize {
    3usize.pow(d as u32) - 1
}

impl<const DIMS: usize> PositionND<DIMS> {
    pub const fn zero() -> Self {
        PositionND([0; DIMS])
    }

    pub fn from_padded(slice: &[i64]) -> PositionND<DIMS> {
        let mut points = [0; DIMS];
        #[allow(clippy::manual_memcpy)]
        for i in 0..(DIMS.min(slice.len())) {
            points[i] = slice[i];
        }
        PositionND(points)
    }

    pub fn neighbors(&self) -> [PositionND<DIMS>; num_neighbors(DIMS)]
    where [PositionND<DIMS>; num_neighbors(DIMS) + 1]: Sized {
        let ns = neighbor_vectors::<DIMS>();
        let mut out = [*self; num_neighbors(DIMS)];
        for (out, dir) in out.iter_mut().zip(IntoIterator::into_iter(ns).filter(|n| n != &[0; DIMS])) {
            *out = *out + PositionND::from(dir);
        }
        out
    }
}

impl PositionND<2> {
    pub fn neighbors_no_diagonals_only_positive(&self) -> [PositionND<2>; 2] {
        let PositionND::<2>([x, y]) = *self;
        [[x + 1, y].into(), [x, y + 1].into()]
    }

    pub fn neighbors_no_diagonals(&self) -> [PositionND<2>; 4] {
        let PositionND::<2>([x, y]) = *self;
        [[x + 1, y].into(), [x, y + 1].into(), [x - 1, y].into(), [x, y - 1].into()]
    }
}

#[macro_export]
macro_rules! dim {
    ($d: expr) => {{
        let mut out = [[0; D]; num_neighbors(D) + 1];
        let mut i = 0;
        for offset in -1..=1 {
            for inner in neighbor_vectors::<$d>() {
                out[i][0] = offset;
                let mut j = 1;
                for e in inner {
                    out[i][j] = e;
                    j += 1;
                }
                i += 1;
            }
        }
        out
    }};
}

fn neighbor_vectors<const D: usize>() -> [[i64; D]; num_neighbors(D) + 1]
where
{
    // I would love to just call neighbor_vectors::<D-1>(), but it seems to be impossible to get the
    // correct constraints for that.
    match D {
        0 => unreachable!(),
        1 => {
            let mut out = [[0; D]; num_neighbors(D) + 1];
            out[0] = [-1; D];
            out[1] = [0; D];
            out[2] = [1; D];
            out
        }
        2 => dim!(1),
        3 => dim!(2),
        4 => dim!(3),
        5 => dim!(4),
        6 => dim!(5),
        7 => dim!(6),
        // Adding more causes a stackoverflow. How curious.
        _ => unimplemented!(),
    }
}

impl<const D: usize> Mul<i64> for PositionND<D> {
    type Output = PositionND<D>;

    fn mul(mut self, rhs: i64) -> Self::Output {
        for p in self.0.iter_mut() {
            *p *= rhs;
        }
        self
    }
}

impl<const D: usize> Add<PositionND<D>> for PositionND<D> {
    type Output = PositionND<D>;

    fn add(mut self, rhs: PositionND<D>) -> Self::Output {
        for (x, y) in self.0.iter_mut().zip(rhs.0) {
            *x += y;
        }
        self
    }
}

impl<const D: usize> Sub<PositionND<D>> for PositionND<D> {
    type Output = PositionND<D>;

    fn sub(mut self, rhs: PositionND<D>) -> Self::Output {
        for (x, y) in self.0.iter_mut().zip(rhs.0) {
            *x -= y;
        }
        self
    }
}

impl From<Direction> for PositionND<2> {
    fn from(d: Direction) -> Self {
        match d {
            Direction::Up => PositionND::from([0, 1]),
            Direction::Right => PositionND::from([1, 0]),
            Direction::Left => PositionND::from([-1, 0]),
            Direction::Down => PositionND::from([0, -1]),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_neighbors_2d() {
        let p = PositionND([0, 0]);
        let n = p.neighbors();
        assert_eq!(
            n,
            [
                PositionND([-1, -1]),
                PositionND([-1, 0]),
                PositionND([-1, 1]),
                PositionND([0, -1]),
                PositionND([0, 1]),
                PositionND([1, -1]),
                PositionND([1, 0]),
                PositionND([1, 1]),
            ]
        );

        let p = PositionND([1, 1]);
        let n = p.neighbors();
        assert_eq!(
            n,
            [
                PositionND([0, 0]),
                PositionND([0, 1]),
                PositionND([0, 2]),
                PositionND([1, 0]),
                PositionND([1, 2]),
                PositionND([2, 0]),
                PositionND([2, 1]),
                PositionND([2, 2]),
            ]
        )
    }

    #[test]
    fn test_neighbors_3d() {
        let p = PositionND([0, 0, 0]);
        let n = p.neighbors();
        assert_eq!(
            n,
            [
                PositionND([-1, -1, -1]),
                PositionND([-1, -1, 0]),
                PositionND([-1, -1, 1]),
                PositionND([-1, 0, -1]),
                PositionND([-1, 0, 0]),
                PositionND([-1, 0, 1]),
                PositionND([-1, 1, -1]),
                PositionND([-1, 1, 0]),
                PositionND([-1, 1, 1]),
                PositionND([0, -1, -1]),
                PositionND([0, -1, 0]),
                PositionND([0, -1, 1]),
                PositionND([0, 0, -1]),
                PositionND([0, 0, 1]),
                PositionND([0, 1, -1]),
                PositionND([0, 1, 0]),
                PositionND([0, 1, 1]),
                PositionND([1, -1, -1]),
                PositionND([1, -1, 0]),
                PositionND([1, -1, 1]),
                PositionND([1, 0, -1]),
                PositionND([1, 0, 0]),
                PositionND([1, 0, 1]),
                PositionND([1, 1, -1]),
                PositionND([1, 1, 0]),
                PositionND([1, 1, 1]),
            ]
        );
    }

    #[test]
    fn test_neighbor_vectors() {
        let n = neighbor_vectors::<2>();
        assert_eq!(n, [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 0], [0, 1], [1, -1], [1, 0], [1, 1],]);
    }

    #[bench]
    fn bench_neighbor_vectors_2d(b: &mut test::Bencher) {
        b.iter(|| test::black_box(neighbor_vectors::<2>()))
    }

    #[bench]
    fn bench_neighbor_vectors_3d(b: &mut test::Bencher) {
        b.iter(|| test::black_box(neighbor_vectors::<3>()))
    }

    #[bench]
    fn bench_neighbor_vectors_4d(b: &mut test::Bencher) {
        b.iter(|| test::black_box(neighbor_vectors::<4>()))
    }

    #[bench]
    fn bench_neighbor_vectors_5d(b: &mut test::Bencher) {
        b.iter(|| test::black_box(neighbor_vectors::<5>()))
    }
}
