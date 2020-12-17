use impl_ops::*;
use itertools::{iproduct, join, Itertools};
use std::{
    collections::HashMap, convert::TryInto, fmt::{self, Display, Formatter}, hash::{BuildHasher, Hash}, ops, ops::AddAssign
};

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

impl Position3D {
    pub fn neighbors(&self) -> Vec<Position3D> {
        iproduct!((-1..=1), (-1..=1), (-1..=1))
            .filter(|t| t != &(0, 0, 0))
            .map(|(x, y, z)| *self + Position3D::from((x, y, z)))
            .collect()
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub enum Cell {
    Alive,
    Dead,
}

impl From<u8> for Cell {
    fn from(b: u8) -> Self {
        match b {
            b'.' => Cell::Dead,
            b'#' => Cell::Alive,
            _ => unreachable!(),
        }
    }
}

impl Display for Cell {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Cell::Alive => ".",
            Cell::Dead => "#",
        })
    }
}

impl Default for Cell {
    fn default() -> Self {
        Cell::Dead
    }
}

pub trait Position {}
impl Position for Position2D {}
impl Position for Position3D {}

#[derive(Debug, Clone)]
pub struct Grid<P: Position, T: Display + Default> {
    pub fields: HashMap<P, T>,
}

impl<P: Position + Eq + Hash, T: Display + Default + Copy> Grid<P, T> {
    pub fn get<Pos: Into<P>>(&self, pos: Pos) -> T {
        self.fields.get(&pos.into()).copied().unwrap_or_else(|| T::default())
    }

    pub fn insert<Pos: Into<P>>(&mut self, pos: Pos, t: T) {
        self.fields.insert(pos.into(), t);
    }
}

impl<P: Position + Hash + Eq, T: Display + Default> std::iter::FromIterator<(P, T)> for Grid<P, T> {
    fn from_iter<I: IntoIterator<Item = (P, T)>>(iter: I) -> Self {
        Grid {
            fields: iter.into_iter().collect(),
        }
    }
}

impl<T: Display + Default + Copy> Grid<Position2D, T> {
    fn draw_ascii(&self) -> String {
        draw_ascii(&self.fields)
    }
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

pub fn draw_ascii<T: Display + Default, S: BuildHasher>(coordinates: &HashMap<Position2D, T, S>) -> String {
    let b = get_boundaries(&coordinates.keys().collect::<Vec<_>>());
    join(
        (b.y_min..=b.y_max).rev().map(|y| {
            (b.x_min..=b.x_max)
                .map(|x| coordinates.get(&(x, y).into()).unwrap_or(&T::default()).to_string())
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
            n => unreachable!(format!("Illegal turn value: {}", n)),
        }
});

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

impl_op!(+|a: Position2D, b: Direction| -> Position2D {
    a + Position2D::from(b)
});

impl_op!(-|a: Position2D, b: Direction| -> Position2D { a - Position2D::from(b) });

impl_op!(*|a: Position2D, b: i64| -> Position2D { Position2D { x: a.x * b, y: a.y * b } });

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

// because calling .unwrap() on a TryInto result isn’t possible without trait bounds on the
// associated Error type.
fn unwrap_number_result<I: TryInto<i64>>(i: I) -> i64 {
    match i.try_into() {
        Ok(i) => return i,
        _ => panic!("Bad coordinate"),
    }
}

impl<I: Into<i64>> From<(I, I)> for Position2D {
    fn from((x, y): (I, I)) -> Position2D {
        Position2D { x: x.into(), y: y.into() }
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
