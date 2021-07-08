extern crate test;
use super::direction::*;
use itertools::iproduct;
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

    // until I can figure out how to properly do that, here’s a “good enough” solution :^)
    pub fn neighbors(&self) -> [PositionND<DIMS>; num_neighbors(DIMS)]
    where
        [PositionND<DIMS>; num_neighbors(DIMS)]: Sized,
        [(); num_neighbors(DIMS)]: Sized,
    {
        let mut out = [PositionND::zero(); num_neighbors(DIMS)];
        match DIMS {
            2 => {
                for (i, n) in iproduct!((-1..=1), (-1..=1))
                    .filter(|t| t != &(0, 0))
                    .map(|(x, y)| PositionND::<DIMS>::from_padded(&[self.points[0] + x, self.points[1] + y]))
                    .enumerate()
                {
                    out[i] = n;
                }
            }
            3 => {
                for (i, n) in iproduct!((-1..=1), (-1..=1), (-1..=1))
                    .filter(|t| t != &(0, 0, 0))
                    .map(|(x, y, z)| PositionND::<DIMS>::from_padded(&[self.points[0] + x, self.points[1] + y, self.points[2] + z]))
                    .enumerate()
                {
                    out[i] = n;
                }
            }
            4 => {
                for (i, n) in iproduct!((-1..=1), (-1..=1), (-1..=1), (-1..=1))
                    .filter(|t| t != &(0, 0, 0, 0))
                    .map(|(x, y, z, w)| {
                        PositionND::<DIMS>::from_padded(&[self.points[0] + x, self.points[1] + y, self.points[2] + z, self.points[3] + w])
                    })
                    .enumerate()
                {
                    out[i] = n;
                }
            }
            _ => unimplemented!(),
        }
        out
    }

    // Maybe one day :(
    /*
    fn neighbors_inner<const D: usize>(existing: [i64; DIMS]) -> [[i64; DIMS]; (DIMS - D).pow(3)] {
        let out = [[0; DIMS]; (DIMS - D).pow(3)];
        let mut index = 0;
        for i in -1..=1 {
            existing[D] = i;
            // I guess that means no recursion with const generics?
            for xs in neighbors_inner(existing.clone()) {
                out[index] = xs;
                index += 1;
            }
        }
        out
    }
    */
}

impl<const D: usize> Mul<i64> for PositionND<D> {
    type Output = PositionND<D>;

    fn mul(self, rhs: i64) -> Self::Output {
        let mut points = [0; D];
        for i in 0..D {
            points[i] = self.points[i] * rhs;
        }
        PositionND { points }
    }
}

impl<const D: usize> Add<PositionND<D>> for PositionND<D> {
    type Output = PositionND<D>;

    fn add(self, rhs: PositionND<D>) -> Self::Output {
        let mut points = [0; D];
        for i in 0..D {
            points[i] = self.points[i] + rhs.points[i];
        }
        PositionND { points }
    }
}

impl<const D: usize> Sub<PositionND<D>> for PositionND<D> {
    type Output = PositionND<D>;

    fn sub(self, rhs: PositionND<D>) -> Self::Output {
        let mut points = [0; D];
        for i in 0..D {
            points[i] = self.points[i] - rhs.points[i];
        }
        PositionND { points }
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
}
