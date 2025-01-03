extern crate test;
use super::direction::*;
use lazy_static::lazy_static;
use std::{
    convert::TryInto, hash::Hash, ops::{Add, Mul, Sub}
};

pub trait Position
where Self: Sized + Hash + PartialEq + Eq + Clone + Copy
{
    fn neighbors(&self) -> Vec<Self>;
}

#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub struct PositionND<const DIMS: usize> {
    pub points: [i64; DIMS],
}

impl<const D: usize, I> From<[I; D]> for PositionND<D>
where I: TryInto<i64> + Copy
{
    fn from(s: [I; D]) -> Self {
        let mut points = [0; D];
        for i in 0..D {
            points[i] = unwrap_number_result(s[i]);
        }
        Self { points }
    }
}

// because calling .unwrap() on a TryInto result isn’t possible without trait bounds on the
// associated Error type.
fn unwrap_number_result<I: TryInto<i64>>(i: I) -> i64 {
    match i.try_into() {
        Ok(i) => i,
        _ => panic!("Bad coordinate"),
    }
}

pub const fn num_neighbors(d: usize) -> usize {
    3usize.pow(d as u32) - 1
}

impl<const DIMS: usize> PositionND<DIMS> {
    pub const fn zero() -> Self {
        PositionND { points: [0; DIMS] }
    }

    pub fn from_padded(slice: &[i64]) -> PositionND<DIMS> {
        let mut points = [0; DIMS];
        for i in 0..(DIMS.min(slice.len())) {
            points[i] = slice[i];
        }
        PositionND { points }
    }

    pub fn neighbors(&self) -> [PositionND<DIMS>; num_neighbors(DIMS)]
    where [PositionND<DIMS>; num_neighbors(DIMS) + 1]: Sized {
        // Day 17 gets 25% faster if we cheat by using these cached vectors
        if DIMS < 5 {
            return match DIMS {
                1 => {
                    let mut out = [*self; num_neighbors(DIMS)];
                    for (out, dir) in out.iter_mut().zip(NEIGHBOR_VECTORS_1D.iter()) {
                        *out = *out + PositionND::from_padded(dir);
                    }
                    out
                }
                2 => {
                    let mut out = [*self; num_neighbors(DIMS)];
                    for (out, dir) in out.iter_mut().zip(NEIGHBOR_VECTORS_2D.iter()) {
                        *out = *out + PositionND::from_padded(dir);
                    }
                    out
                }
                3 => {
                    let mut out = [*self; num_neighbors(DIMS)];
                    for (out, dir) in out.iter_mut().zip(NEIGHBOR_VECTORS_3D.iter()) {
                        *out = *out + PositionND::from_padded(dir);
                    }
                    out
                }
                4 => {
                    let mut out = [*self; num_neighbors(DIMS)];
                    for (out, dir) in out.iter_mut().zip(NEIGHBOR_VECTORS_4D.iter()) {
                        *out = *out + PositionND::from_padded(dir);
                    }
                    out
                }
                _ => unreachable!(),
            };
        }
        let ns = neighbor_vectors::<DIMS>();
        let mut out = [*self; num_neighbors(DIMS)];
        for (out, dir) in out.iter_mut().zip(IntoIterator::into_iter(ns).filter(|n| n != &[0; DIMS])) {
            *out = *out + PositionND::from(dir);
        }
        out
    }
}

fn build_neighbor_cache<const D: usize>() -> Vec<[i64; D]>
where [(); num_neighbors(D) + 1]:  {
    IntoIterator::into_iter(neighbor_vectors::<D>()).filter(|n| n != &[0; D]).collect()
}

lazy_static! {
    static ref NEIGHBOR_VECTORS_1D: Vec<[i64; 1]> = build_neighbor_cache::<1>();
    static ref NEIGHBOR_VECTORS_2D: Vec<[i64; 2]> = build_neighbor_cache::<2>();
    static ref NEIGHBOR_VECTORS_3D: Vec<[i64; 3]> = build_neighbor_cache::<3>();
    static ref NEIGHBOR_VECTORS_4D: Vec<[i64; 4]> = build_neighbor_cache::<4>();
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
        for p in self.points.iter_mut() {
            *p *= rhs;
        }
        self
    }
}

impl<const D: usize> Add<PositionND<D>> for PositionND<D> {
    type Output = PositionND<D>;

    fn add(mut self, rhs: PositionND<D>) -> Self::Output {
        for (x, y) in self.points.iter_mut().zip(rhs.points) {
            *x += y;
        }
        self
    }
}

impl<const D: usize> Sub<PositionND<D>> for PositionND<D> {
    type Output = PositionND<D>;

    fn sub(mut self, rhs: PositionND<D>) -> Self::Output {
        for (x, y) in self.points.iter_mut().zip(rhs.points) {
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
        let p = PositionND { points: [0, 0] };
        let n = p.neighbors();
        assert_eq!(
            n,
            [
                PositionND { points: [-1, -1] },
                PositionND { points: [-1, 0] },
                PositionND { points: [-1, 1] },
                PositionND { points: [0, -1] },
                PositionND { points: [0, 1] },
                PositionND { points: [1, -1] },
                PositionND { points: [1, 0] },
                PositionND { points: [1, 1] },
            ]
        );

        let p = PositionND { points: [1, 1] };
        let n = p.neighbors();
        assert_eq!(
            n,
            [
                PositionND { points: [0, 0] },
                PositionND { points: [0, 1] },
                PositionND { points: [0, 2] },
                PositionND { points: [1, 0] },
                PositionND { points: [1, 2] },
                PositionND { points: [2, 0] },
                PositionND { points: [2, 1] },
                PositionND { points: [2, 2] },
            ]
        )
    }

    #[test]
    fn test_neighbors_3d() {
        let p = PositionND { points: [0, 0, 0] };
        let n = p.neighbors();
        assert_eq!(
            n,
            [
                PositionND { points: [-1, -1, -1] },
                PositionND { points: [-1, -1, 0] },
                PositionND { points: [-1, -1, 1] },
                PositionND { points: [-1, 0, -1] },
                PositionND { points: [-1, 0, 0] },
                PositionND { points: [-1, 0, 1] },
                PositionND { points: [-1, 1, -1] },
                PositionND { points: [-1, 1, 0] },
                PositionND { points: [-1, 1, 1] },
                PositionND { points: [0, -1, -1] },
                PositionND { points: [0, -1, 0] },
                PositionND { points: [0, -1, 1] },
                PositionND { points: [0, 0, -1] },
                PositionND { points: [0, 0, 1] },
                PositionND { points: [0, 1, -1] },
                PositionND { points: [0, 1, 0] },
                PositionND { points: [0, 1, 1] },
                PositionND { points: [1, -1, -1] },
                PositionND { points: [1, -1, 0] },
                PositionND { points: [1, -1, 1] },
                PositionND { points: [1, 0, -1] },
                PositionND { points: [1, 0, 0] },
                PositionND { points: [1, 0, 1] },
                PositionND { points: [1, 1, -1] },
                PositionND { points: [1, 1, 0] },
                PositionND { points: [1, 1, 1] },
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
