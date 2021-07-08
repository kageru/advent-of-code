extern crate test;
use super::direction::*;
use impl_ops::*;
use itertools::iproduct;
use std::{
    convert::TryInto, hash::Hash, ops::{self, Add, AddAssign, Mul, Sub}
};

pub trait Position
where Self: Sized + Hash + PartialEq + Eq + Clone + Copy
{
    fn neighbors(&self) -> Vec<Self>;
}

#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub struct Position2D {
    pub x: i64,
    pub y: i64,
}

#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub struct Position3D {
    pub x: i64,
    pub y: i64,
    pub z: i64,
}

#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub struct Position4D {
    pub x: i64,
    pub y: i64,
    pub z: i64,
    pub w: i64,
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

mod p2d {
    use super::*;

    impl From<Direction> for Position2D {
        fn from(d: Direction) -> Self {
            match d {
                Direction::Up => Position2D::from((0, 1)),
                Direction::Right => Position2D::from((1, 0)),
                Direction::Left => Position2D::from((-1, 0)),
                Direction::Down => Position2D::from((0, -1)),
            }
        }
    }

    impl Position for Position2D {
        fn neighbors(&self) -> Vec<Position2D> {
            vec![
                *self + Direction::Up + Direction::Left,
                *self + Direction::Up,
                *self + Direction::Up + Direction::Right,
                *self + Direction::Left,
                *self + Direction::Right,
                *self + Direction::Down + Direction::Left,
                *self + Direction::Down,
                *self + Direction::Down + Direction::Right,
            ]
        }
    }

    impl<I: Into<i64>> From<(I, I)> for Position2D {
        fn from((x, y): (I, I)) -> Position2D {
            Position2D { x: x.into(), y: y.into() }
        }
    }

    impl_op!(+|a: Position2D, b: Direction| -> Position2D { a + Position2D::from(b) });

    impl_op!(-|a: Position2D, b: Direction| -> Position2D { a - Position2D::from(b) });

    impl_op!(*|a: Position2D, b: i64| -> Position2D { Position2D { x: a.x * b, y: a.y * b } });

    impl_op!(+|a: Position2D, b: Position2D| -> Position2D {
        Position2D {
            x: a.x + b.x,
            y: a.y + b.y
        }
    });

    impl_op!(-|a: Position2D, b: Position2D| -> Position2D {
        Position2D {
            x: a.x - b.x,
            y: a.y - b.y,
        }
    });

    impl AddAssign<Direction> for Position2D {
        fn add_assign(&mut self, rhs: Direction) {
            *self = *self + rhs;
        }
    }

    impl AddAssign for Position2D {
        fn add_assign(&mut self, rhs: Position2D) {
            *self = *self + rhs;
        }
    }
}

mod p3d {
    use super::*;

    impl Position for Position3D {
        fn neighbors(&self) -> Vec<Position3D> {
            iproduct!((-1..=1), (-1..=1), (-1..=1))
                .filter(|t| t != &(0, 0, 0))
                .map(|(x, y, z)| *self + Position3D::from((x, y, z)))
                .collect()
        }
    }

    impl<I> From<(I, I, I)> for Position3D
    where I: TryInto<i64>
    {
        fn from((x, y, z): (I, I, I)) -> Position3D {
            Position3D {
                x: unwrap_number_result(x),
                y: unwrap_number_result(y),
                z: unwrap_number_result(z),
            }
        }
    }

    impl_op!(-|a: Position3D, b: Position3D| -> Position3D {
        Position3D {
            x: a.x - b.x,
            y: a.y - b.y,
            z: a.z - b.z,
        }
    });

    impl_op!(+|a: Position3D, b: Position3D| -> Position3D {
        Position3D {
            x: a.x + b.x,
            y: a.y + b.y,
            z: a.z + b.z,
        }
    });
}

mod p4d {
    use super::*;

    impl Position for Position4D {
        fn neighbors(&self) -> Vec<Position4D> {
            iproduct!((-1..=1), (-1..=1), (-1..=1), (-1..=1))
                .filter(|t| t != &(0, 0, 0, 0))
                .map(|(x, y, z, w)| *self + Position4D::from((x, y, z, w)))
                .collect()
        }
    }

    impl<I> From<(I, I, I, I)> for Position4D
    where I: TryInto<i64>
    {
        fn from((x, y, z, w): (I, I, I, I)) -> Position4D {
            Position4D {
                x: unwrap_number_result(x),
                y: unwrap_number_result(y),
                z: unwrap_number_result(z),
                w: unwrap_number_result(w),
            }
        }
    }

    impl_op!(-|a: Position4D, b: Position4D| -> Position4D {
        Position4D {
            x: a.x - b.x,
            y: a.y - b.y,
            z: a.z - b.z,
            w: a.w - b.w,
        }
    });

    impl_op!(+|a: Position4D, b: Position4D| -> Position4D {
        Position4D {
            x: a.x + b.x,
            y: a.y + b.y,
            z: a.z + b.z,
            w: a.w + b.w,
        }
    });
}

// because calling .unwrap() on a TryInto result isn’t possible without trait bounds on the
// associated Error type.
fn unwrap_number_result<I: TryInto<i64>>(i: I) -> i64 {
    match i.try_into() {
        Ok(i) => i,
        _ => panic!("Bad coordinate"),
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
