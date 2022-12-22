use impl_ops::*;
use std::{ops, ops::AddAssign};

pub const ALL_DIRECTIONS: [Direction; 4] = [Direction::Up, Direction::Down, Direction::Left, Direction::Right];

#[derive(Clone, Copy, Debug)]
pub enum Direction {
    Right = 0,
    Down = 1,
    Left = 2,
    Up = 3,
}

impl AddAssign<i8> for Direction {
    fn add_assign(&mut self, rhs: i8) {
        *self = *self + rhs;
    }
}

impl_op!(+ |a: Direction, b: i8| -> Direction {
        match b {
            -1 | 3 => match a {
                Direction::Up => Direction::Left,
                Direction::Right => Direction::Up,
                Direction::Down => Direction::Right,
                Direction::Left => Direction::Down,
            },
            1 | -3 => match a {
                Direction::Up => Direction::Right,
                Direction::Right => Direction::Down,
                Direction::Down => Direction::Left,
                Direction::Left => Direction::Up,
            },
            0 | 4 | -4 => a,
            2 | -2 => match a {
                Direction::Up => Direction::Down,
                Direction::Right => Direction::Left,
                Direction::Down => Direction::Up,
                Direction::Left => Direction::Right,
            },
            n => unreachable!("Illegal turn value: {n}"),
        }
});

impl Direction {
    pub fn turn(&mut self, turn_value: i64) {
        *self += turn_value as i8;
    }
}
