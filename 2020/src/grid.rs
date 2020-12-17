pub mod cell;
pub mod direction;
pub mod position;
pub use direction::*;
pub use position::*;

use itertools::join;
use std::{collections::HashMap, fmt::Display, hash::BuildHasher};

#[derive(Debug, Clone)]
pub struct Grid<P: Position, T: Display + Default> {
    pub fields: HashMap<P, T>,
}

impl<P: Position, T: Display + Default + Copy> Grid<P, T> {
    pub fn get<Pos: Into<P>>(&self, pos: Pos) -> T {
        self.fields.get(&pos.into()).copied().unwrap_or_else(|| T::default())
    }

    pub fn insert<Pos: Into<P>>(&mut self, pos: Pos, t: T) {
        self.fields.insert(pos.into(), t);
    }
}

impl<P: Position, T: Display + Default> std::iter::FromIterator<(P, T)> for Grid<P, T> {
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

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_add() {
        assert_eq!(Position2D { x: 0, y: 2 } + Position2D { x: -1, y: 0 }, (-1, 2).into());
        assert_eq!(Position2D { x: 0, y: -1 } + Direction::Up, (0, 0).into());
    }
}
