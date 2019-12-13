use itertools::join;
use std::collections::HashMap;

#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub struct Position2D {
    x: i64,
    y: i64,
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

pub fn draw_ascii<T: std::fmt::Display>(coordinates: &HashMap<Position2D, T>, default: T) -> String {
    let b = get_boundaries(&coordinates.keys().collect::<Vec<_>>());
    join(
        (b.y_min..=b.y_max).rev().map(|y| {
            (b.x_min..b.x_max)
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

impl std::ops::Add for Position2D {
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

#[cfg(test)]
mod tests {}
