pub mod cell;
pub mod direction;
pub mod position;
pub use direction::*;
pub use position::*;

use itertools::join;
use std::{collections::HashMap, fmt::Display, hash::BuildHasher};

#[derive(Debug, Clone, PartialEq)]
pub struct Grid<T: Default, const D: usize> {
    pub fields: HashMap<PositionND<D>, T>,
}

impl<T: Default + Copy, const D: usize> Grid<T, D> {
    pub fn get(&self, pos: &PositionND<D>) -> T {
        self.fields.get(pos).copied().unwrap_or_else(T::default)
    }

    pub fn insert<Pos: Into<PositionND<D>>>(&mut self, pos: Pos, t: T) {
        self.fields.insert(pos.into(), t);
    }

    pub fn from_bytes_2d<F: FnMut(u8) -> T + Copy>(raw: &str, mut f: F) -> Grid<T, 2> {
        raw.lines()
            .enumerate()
            .flat_map(move |(y, l)| l.bytes().enumerate().map(move |(x, c)| (PositionND { points: [x as i64, y as i64] }, f(c))))
            .collect()
    }

    pub fn len(&self) -> usize {
        self.fields.len()
    }
}

impl<T: Default, const D: usize> std::iter::FromIterator<(PositionND<D>, T)> for Grid<T, D> {
    fn from_iter<I: IntoIterator<Item = (PositionND<D>, T)>>(iter: I) -> Self {
        Grid { fields: iter.into_iter().collect() }
    }
}

struct Boundaries {
    x_min: i64,
    x_max: i64,
    y_min: i64,
    y_max: i64,
}

#[rustfmt::skip]
fn get_boundaries(input: &[&PositionND<2>]) -> Boundaries {
    let x_min = input.iter().min_by_key(|k| k.points[0]).map(|p| p.points[0]).unwrap_or(0);
    let x_max = input.iter().max_by_key(|k| k.points[0]).map(|p| p.points[0]).unwrap_or(0);
    let y_min = input.iter().min_by_key(|k| k.points[1]).map(|p| p.points[1]).unwrap_or(0);
    let y_max = input.iter().max_by_key(|k| k.points[1]).map(|p| p.points[1]).unwrap_or(0);
    Boundaries { x_min, x_max, y_min, y_max }
}

pub fn draw_ascii<T: Display + Default, S: BuildHasher>(coordinates: &HashMap<PositionND<2>, T, S>) -> String {
    let b = get_boundaries(&coordinates.keys().collect::<Vec<_>>());
    join(
        (b.y_min..=b.y_max).rev().map(|y| {
            (b.x_min..=b.x_max)
                .map(|x| coordinates.get(&PositionND { points: [x, y] }).unwrap_or(&T::default()).to_string())
                .collect::<String>()
        }),
        "\n",
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(PositionND::from([0, 2]) + PositionND::from([-1, 0]), [-1, 2].into());
        assert_eq!(PositionND::from([0, -1]) + PositionND::from(Direction::Up), [0, 0].into());
    }

    #[test]
    fn test_sub() {
        assert_eq!(PositionND::from([0, 2]) - PositionND::from([-1, 0]), [1, 2].into());
        assert_eq!(PositionND::from([0, -1]) - PositionND::from([0, -1]), [0, 0].into());
    }

    #[test]
    fn test_mul() {
        assert_eq!(PositionND::from([0, 2]) * 5, [0, 10].into());
        assert_eq!(PositionND::from([0, -1]) * -2, [0, 2].into());
    }
}
