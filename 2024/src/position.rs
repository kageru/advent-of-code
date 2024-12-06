extern crate test;
use crate::common::Inc;
use std::{
    fmt::Debug,
    hash::Hash,
    iter::Step,
    ops::{Add, AddAssign, Index, IndexMut},
};

#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
#[repr(transparent)]
pub struct Pos<I, const DIMS: usize>(pub [I; DIMS]);

pub type Pos2D<I> = Pos<I, 2>;

pub const fn num_neighbors(d: usize) -> usize {
    3usize.pow(d as u32) - 1
}

impl<I: Inc + Add<I, Output = I> + AddAssign + Debug, const DIMS: usize> Pos<I, DIMS> {
    pub fn zero() -> Self {
        Pos([I::default(); DIMS])
    }

    pub fn from_padded(slice: &[I]) -> Pos<I, DIMS> {
        let mut points = [I::default(); DIMS];
        #[allow(clippy::manual_memcpy)]
        for i in 0..(DIMS.min(slice.len())) {
            points[i] = slice[i];
        }
        Pos(points)
    }

    pub fn neighbors(&self) -> [Pos<I, DIMS>; num_neighbors(DIMS)]
    where [Pos<I, DIMS>; num_neighbors(DIMS) + 1]: Sized {
        let ns = neighbor_vectors::<I, DIMS>();
        let mut out = [*self; num_neighbors(DIMS)];
        for (out, dir) in out.iter_mut().zip(IntoIterator::into_iter(ns).filter(|n| n != &[I::default(); DIMS])) {
            *out += Pos(dir);
        }
        out
    }
}

impl<I, const D: usize> Add<Pos<I, D>> for Pos<I, D>
where I: AddAssign<I> + Copy
{
    type Output = Pos<I, D>;

    fn add(mut self, rhs: Pos<I, D>) -> Self::Output {
        for (x, y) in self.0.iter_mut().zip(rhs.0) {
            *x += y;
        }
        self
    }
}
impl<I, const D: usize> AddAssign<Pos<I, D>> for Pos<I, D>
where I: AddAssign<I> + Copy
{
    fn add_assign(&mut self, rhs: Pos<I, D>) {
        for i in 0..D {
            self[i] += rhs[i];
        }
    }
}

impl<I, const D: usize> Index<usize> for Pos<I, D> {
    type Output = I;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl<I, const D: usize> IndexMut<usize> for Pos<I, D> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
    }
}

impl<I: Copy + Default + Step> Pos<I, 2> {
    pub fn neighbors_no_diagonals(&self) -> [Pos<I, 2>; 4] {
        let Pos([x, y]) = *self;
        [Pos([x.inc(), y]), Pos([x, y.inc()]), Pos([x.dec(), y]), Pos([x, y.dec()])]
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
        let p = Pos([0, 0]);
        let n = p.neighbors();
        assert_eq!(n, [
            Pos([-1, -1]),
            Pos([-1, 0]),
            Pos([-1, 1]),
            Pos([0, -1]),
            Pos([0, 1]),
            Pos([1, -1]),
            Pos([1, 0]),
            Pos([1, 1]),
        ]);

        let p = Pos([1, 1]);
        let n = p.neighbors();
        assert_eq!(n, [
            Pos([0, 0]),
            Pos([0, 1]),
            Pos([0, 2]),
            Pos([1, 0]),
            Pos([1, 2]),
            Pos([2, 0]),
            Pos([2, 1]),
            Pos([2, 2]),
        ])
    }

    #[test]
    fn test_neighbors_3d() {
        let p = Pos([0, 0, 0]);
        let n = p.neighbors();
        assert_eq!(n, [
            Pos([-1, -1, -1]),
            Pos([-1, -1, 0]),
            Pos([-1, -1, 1]),
            Pos([-1, 0, -1]),
            Pos([-1, 0, 0]),
            Pos([-1, 0, 1]),
            Pos([-1, 1, -1]),
            Pos([-1, 1, 0]),
            Pos([-1, 1, 1]),
            Pos([0, -1, -1]),
            Pos([0, -1, 0]),
            Pos([0, -1, 1]),
            Pos([0, 0, -1]),
            Pos([0, 0, 1]),
            Pos([0, 1, -1]),
            Pos([0, 1, 0]),
            Pos([0, 1, 1]),
            Pos([1, -1, -1]),
            Pos([1, -1, 0]),
            Pos([1, -1, 1]),
            Pos([1, 0, -1]),
            Pos([1, 0, 0]),
            Pos([1, 0, 1]),
            Pos([1, 1, -1]),
            Pos([1, 1, 0]),
            Pos([1, 1, 1]),
        ]);
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
