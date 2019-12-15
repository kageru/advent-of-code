use itertools::join;
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::BuildHasher;
use std::ops::{Add, AddAssign};

#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub struct Position2D {
    pub x: i64,
    pub y: i64,
}

#[derive(Clone, Copy)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

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

pub fn draw_ascii<T: Display, S: BuildHasher>(
    coordinates: &HashMap<Position2D, T, S>,
    default: T,
) -> String {
    let b = get_boundaries(&coordinates.keys().collect::<Vec<_>>());
    join(
        (b.y_min..=b.y_max).rev().map(|y| {
            (b.x_min..=b.x_max)
                .map(|x| {
                    coordinates
                        .get(&(x, y).into())
                        .unwrap_or(&default)
                        .to_string()
                })
                .collect::<String>()
        }),
        "\n",
    )
}

impl Direction {
    pub fn turn(&mut self, turn_value: i64) {
        *self = match turn_value {
            -1 => match self {
                Direction::Up => Direction::Left,
                Direction::Right => Direction::Up,
                Direction::Down => Direction::Right,
                Direction::Left => Direction::Down,
            },
            1 => match self {
                Direction::Up => Direction::Right,
                Direction::Right => Direction::Down,
                Direction::Down => Direction::Left,
                Direction::Left => Direction::Up,
            },
            0 => *self,
            n => unreachable!(format!("Illegal turn value: {}", n)),
        }
    }
}

impl Position2D {
    pub fn mov(&mut self, dir: &Direction) {
        *self = *self
            + match dir {
                Direction::Up => (0, 1).into(),
                Direction::Right => (1, 0).into(),
                Direction::Left => (-1, 0).into(),
                Direction::Down => (0, -1).into(),
            }
    }
}

impl AddAssign for Position2D {
    fn add_assign(&mut self, rhs: Position2D) {
        *self = *self + rhs;
    }
}

impl Add for Position2D {
    type Output = Position2D;

    fn add(self, rhs: Position2D) -> Position2D {
        Position2D {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl From<(i64, i64)> for Position2D {
    fn from(tuple: (i64, i64)) -> Position2D {
        Position2D {
            x: tuple.0,
            y: tuple.1,
        }
    }
}
