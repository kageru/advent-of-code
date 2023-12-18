extern crate test;
use crate::common::Inc;
use std::{
    fmt::Debug,
    hash::Hash,
    iter::Step,
    ops::{Add, AddAssign, Index, IndexMut},
};

#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub struct PositionND<I, const DIMS: usize>(pub [I; DIMS]);

pub type Position2D<I> = PositionND<I, 2>;

pub const fn num_neighbors(d: usize) -> usize {
    3usize.pow(d as u32) - 1
}

impl<I: Inc + Add<I, Output = I> + AddAssign + Debug, const DIMS: usize> PositionND<I, DIMS> {
    pub fn zero() -> Self {
        PositionND([I::default(); DIMS])
    }

    pub fn from_padded(slice: &[I]) -> PositionND<I, DIMS> {
        let mut points = [I::default(); DIMS];
        #[allow(clippy::manual_memcpy)]
        for i in 0..(DIMS.min(slice.len())) {
            points[i] = slice[i];
        }
        PositionND(points)
    }

    pub fn neighbors(&self) -> [PositionND<I, DIMS>; num_neighbors(DIMS)]
    where [PositionND<I, DIMS>; num_neighbors(DIMS) + 1]: Sized {
        let ns = neighbor_vectors::<I, DIMS>();
        let mut out = [*self; num_neighbors(DIMS)];
        for (out, dir) in out.iter_mut().zip(IntoIterator::into_iter(ns).filter(|n| n != &[I::default(); DIMS])) {
            *out += PositionND(dir);
        }
        out
    }
}

impl<I, const D: usize> Add<PositionND<I, D>> for PositionND<I, D>
where I: AddAssign<I> + Copy
{
    type Output = PositionND<I, D>;

    fn add(mut self, rhs: PositionND<I, D>) -> Self::Output {
        for (x, y) in self.0.iter_mut().zip(rhs.0) {
            *x += y;
        }
        self
    }
}
impl<I, const D: usize> AddAssign<PositionND<I, D>> for PositionND<I, D>
where I: AddAssign<I> + Copy
{
    fn add_assign(&mut self, rhs: PositionND<I, D>) {
        for i in 0..D {
            self[i] += rhs[i];
        }
    }
}

impl<I, const D: usize> Index<usize> for PositionND<I, D> {
    type Output = I;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl<I, const D: usize> IndexMut<usize> for PositionND<I, D> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
    }
}

impl<I: Copy + Default + Step> PositionND<I, 2> {
    pub fn neighbors_no_diagonals(&self) -> [PositionND<I, 2>; 4] {
        let PositionND([x, y]) = *self;
        [PositionND([x.inc(), y]), PositionND([x, y.inc()]), PositionND([x.dec(), y]), PositionND([x, y.dec()])]
    }
}

macro_rules! dim {
    ($d: expr, $i:ty) => {{
        let zero: $i = Default::default();
        let mut out = [[zero; D]; num_neighbors(D) + 1];
        let mut i = 0;
        for offset in zero.dec()..=zero.inc() {
            for inner in neighbor_vectors::<$i, $d>() {
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

fn neighbor_vectors<I: Inc, const D: usize>() -> [[I; D]; num_neighbors(D) + 1] {
    // I would love to just call neighbor_vectors::<D-1>(), but it seems to be impossible to get the
    // correct constraints for that.
    match D {
        0 => unreachable!(),
        1 => {
            let zero = I::default();
            let mut out = [[zero; D]; num_neighbors(D) + 1];
            out[0] = [zero.dec(); D];
            out[1] = [zero; D];
            out[2] = [zero.inc(); D];
            out
        }
        2 => dim!(1, I),
        3 => dim!(2, I),
        4 => dim!(3, I),
        5 => dim!(4, I),
        6 => dim!(5, I),
        7 => dim!(6, I),
        // Adding more causes a stackoverflow. How curious.
        _ => unimplemented!(),
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
        let n = neighbor_vectors::<i32, 2>();
        assert_eq!(n, [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 0], [0, 1], [1, -1], [1, 0], [1, 1],]);
    }

    #[bench]
    fn bench_neighbor_vectors_2d(b: &mut test::Bencher) {
        b.iter(|| test::black_box(neighbor_vectors::<i32, 2>()))
    }

    #[bench]
    fn bench_neighbor_vectors_3d(b: &mut test::Bencher) {
        b.iter(|| test::black_box(neighbor_vectors::<i32, 3>()))
    }

    #[bench]
    fn bench_neighbor_vectors_4d(b: &mut test::Bencher) {
        b.iter(|| test::black_box(neighbor_vectors::<i32, 4>()))
    }

    #[bench]
    fn bench_neighbor_vectors_5d(b: &mut test::Bencher) {
        b.iter(|| test::black_box(neighbor_vectors::<i32, 5>()))
    }
}
