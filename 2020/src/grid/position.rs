use super::direction::*;
use impl_ops::*;
use itertools::iproduct;
use std::{convert::TryInto, hash::Hash, ops, ops::AddAssign};

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
