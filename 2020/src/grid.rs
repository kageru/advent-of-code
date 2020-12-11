use impl_ops::*;
use itertools::join;
use std::{collections::HashMap, fmt::Display, hash::BuildHasher, ops, ops::AddAssign};

#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub struct Position2D {
    pub x: i64,
    pub y: i64,
}

#[derive(Clone, Copy, Debug)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

pub const ALL_DIRECTIONS: [Direction; 4] = [Direction::Up, Direction::Down, Direction::Left, Direction::Right];

struct Boundaries {
    x_min: i64,
    x_max: i64,
    y_min: i64,
    y_max: i64,
}

#[rustfmt::skip]
fn get_boundaries(input: &[&Position2D]) -> Boundaries {
    let x_min = input.iter().min_by_key(|k| k.x).map(|p| p.x).unwrap_or(0);
    let x_max = input.iter().max_by_key(|k| k.x).map(|p| p.x).unwrap_or(0);
    let y_min = input.iter().min_by_key(|k| k.y).map(|p| p.y).unwrap_or(0);
    let y_max = input.iter().max_by_key(|k| k.y).map(|p| p.y).unwrap_or(0);
    Boundaries { x_min, x_max, y_min, y_max }
}

pub fn draw_ascii<T: Display, S: BuildHasher>(coordinates: &HashMap<Position2D, T, S>, default: T) -> String {
    let b = get_boundaries(&coordinates.keys().collect::<Vec<_>>());
    join(
        (b.y_min..=b.y_max).rev().map(|y| {
            (b.x_min..=b.x_max)
                .map(|x| coordinates.get(&(x, y).into()).unwrap_or(&default).to_string())
                .collect::<String>()
        }),
        "\n",
    )
}

impl Position2D {
    pub fn neighbors(&self) -> [(Direction, Position2D); 4] {
        [
            (Direction::Up, *self + Direction::Up),
            (Direction::Down, *self + Direction::Down),
            (Direction::Right, *self + Direction::Right),
            (Direction::Left, *self + Direction::Left),
        ]
    }

    pub fn moore(&self) -> [Position2D; 8] {
        [
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

impl Direction {
    pub fn turn(&mut self, turn_value: i64) {
        *self += turn_value as i8;
    }
}

impl_op!(+ |a: Direction, b: i8| -> Direction {
        match b {
            -1 => match a {
                Direction::Up => Direction::Left,
                Direction::Right => Direction::Up,
                Direction::Down => Direction::Right,
                Direction::Left => Direction::Down,
            },
            1 => match a {
                Direction::Up => Direction::Right,
                Direction::Right => Direction::Down,
                Direction::Down => Direction::Left,
                Direction::Left => Direction::Up,
            },
            0 => a,
            n => unreachable!(format!("Illegal turn value: {}", n)),
        }
});

impl_op!(+ |a: Position2D, b: Position2D| -> Position2D {
    Position2D {
        x: a.x + b.x,
        y: a.y + b.y }
});

impl_op!(-|a: Position2D, b: Position2D| -> Position2D {
    Position2D {
        x: a.x - b.x,
        y: a.y - b.y,
    }
});

impl_op!(+ |a: Position2D, b: Direction| -> Position2D { a + match b {
            Direction::Up => Position2D::from((0, 1)),
            Direction::Right => Position2D::from((1, 0)),
            Direction::Left => Position2D::from((-1, 0)),
            Direction::Down => Position2D::from((0, -1)),
        }
});

impl AddAssign<i8> for Direction {
    fn add_assign(&mut self, rhs: i8) {
        *self = *self + rhs;
    }
}

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

impl From<(usize, usize)> for Position2D {
    fn from(tuple: (usize, usize)) -> Position2D {
        Position2D {
            x: tuple.0 as i64,
            y: tuple.1 as i64,
        }
    }
}

impl From<(i32, i32)> for Position2D {
    fn from(tuple: (i32, i32)) -> Position2D {
        Position2D {
            x: tuple.0 as i64,
            y: tuple.1 as i64,
        }
    }
}

impl From<(i64, i64)> for Position2D {
    fn from(tuple: (i64, i64)) -> Position2D {
        Position2D { x: tuple.0, y: tuple.1 }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_add() {
        assert_eq!(Position2D { x: 0, y: 2 } + Position2D { x: -1, y: 0 }, (-1, 2).into());
        assert_eq!(Position2D { x: 0, y: -1 } + Direction::Up, (0, 0).into());
    }
}
