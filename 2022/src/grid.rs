pub mod direction;
pub mod position;
pub use direction::*;
use fnv::FnvHashMap as HashMap;
use itertools::{join, Itertools, MinMaxResult};
pub use position::*;
use std::{
    fmt::Display,
    ops::{Index, IndexMut},
};

#[allow(clippy::len_without_is_empty)] // I mainly have this for assertions in benchmarks
pub trait Grid<T, const D: usize>: Index<PositionND<D>, Output = T> + IndexMut<PositionND<D>> {
    fn get(&self, pos: &PositionND<D>) -> Option<&T>;

    fn insert<Pos: Into<PositionND<D>>>(&mut self, pos: Pos, element: T);

    fn len(&self) -> usize;
}

#[derive(Debug, Clone, PartialEq)]
pub struct HashGrid<T: Default, const D: usize> {
    pub fields: HashMap<PositionND<D>, T>,
}

impl<T: Default, const D: usize> Index<PositionND<D>> for HashGrid<T, D> {
    type Output = T;
    fn index(&self, index: PositionND<D>) -> &Self::Output {
        &self.fields[&index]
    }
}

impl<T: Default, const D: usize> IndexMut<PositionND<D>> for HashGrid<T, D> {
    fn index_mut(&mut self, index: PositionND<D>) -> &mut Self::Output {
        self.fields.get_mut(&index).expect("Key not in map")
    }
}

impl<T: Default, const D: usize> Grid<T, D> for HashGrid<T, D> {
    fn get(&self, pos: &PositionND<D>) -> Option<&T> {
        self.fields.get(pos)
    }

    fn insert<Pos: Into<PositionND<D>>>(&mut self, pos: Pos, t: T) {
        self.fields.insert(pos.into(), t);
    }

    fn len(&self) -> usize {
        self.fields.len()
    }
}

impl<T: Default + Copy> HashGrid<T, 2> {
    pub fn from_bytes_2d<F: FnMut(u8) -> T + Copy>(raw: &str, mut f: F) -> HashGrid<T, 2> {
        raw.lines()
            .enumerate()
            .flat_map(move |(y, l)| l.bytes().enumerate().map(move |(x, c)| (PositionND([x as i64, y as i64]), f(c))))
            .collect()
    }
}

impl<T: Default, const D: usize> std::iter::FromIterator<(PositionND<D>, T)> for HashGrid<T, D> {
    fn from_iter<I: IntoIterator<Item = (PositionND<D>, T)>>(iter: I) -> Self {
        HashGrid { fields: iter.into_iter().collect() }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VecGrid<T> {
    pub fields: Vec<Vec<T>>,
}

impl<T> Grid<T, 2> for VecGrid<T> {
    fn get(&self, pos: &PositionND<2>) -> Option<&T> {
        self.fields.get(pos.0[0] as usize)?.get(pos.0[1] as usize)
    }

    fn insert<Pos: Into<PositionND<2>>>(&mut self, pos: Pos, element: T) {
        let PositionND([x, y]) = pos.into();
        self.fields[x as usize][y as usize] = element;
    }

    fn len(&self) -> usize {
        self.fields.len()
    }
}

impl<T> Index<Position2D> for VecGrid<T> {
    type Output = T;
    fn index(&self, index: Position2D) -> &Self::Output {
        &self.fields[index.0[0] as usize][index.0[1] as usize]
    }
}

impl<T> IndexMut<Position2D> for VecGrid<T> {
    fn index_mut(&mut self, index: Position2D) -> &mut Self::Output {
        &mut self.fields[index.0[0] as usize][index.0[1] as usize]
    }
}

impl<T: Copy> VecGrid<T> {
    pub fn from_bytes_2d<F: FnMut(u8) -> T + Copy>(raw: &str, f: F) -> VecGrid<T> {
        VecGrid { fields: raw.lines().map(|l| l.bytes().map(f).collect()).collect() }
    }
}

pub struct Boundaries {
    pub x_min: i64,
    pub x_max: i64,
    pub y_min: i64,
    pub y_max: i64,
}

pub fn get_boundaries(input: &[&PositionND<2>]) -> Boundaries {
    let (x_min, x_max) = match input.iter().map(|p| p.0[0]).minmax() {
        MinMaxResult::NoElements => (0, 0),
        MinMaxResult::MinMax(min, max) => (min, max),
        MinMaxResult::OneElement(x) => (x, x),
    };
    let (y_min, y_max) = match input.iter().map(|p| p.0[1]).minmax() {
        MinMaxResult::NoElements => (0, 0),
        MinMaxResult::MinMax(min, max) => (min, max),
        MinMaxResult::OneElement(x) => (x, x),
    };
    Boundaries { x_min, x_max, y_min, y_max }
}

pub fn draw_ascii<T: Display + Default>(coordinates: &HashMap<PositionND<2>, T>) -> String {
    let b = get_boundaries(&coordinates.keys().collect::<Vec<_>>());
    join(
        (b.y_min..=b.y_max).rev().map(|y| {
            (b.x_min..=b.x_max).map(|x| coordinates.get(&PositionND([x, y])).unwrap_or(&T::default()).to_string()).collect::<String>()
        }),
        "\n",
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(PositionND([0, 2]) + PositionND([-1, 0]), [-1, 2].into());
        assert_eq!(PositionND([0, -1]) + PositionND::from(Direction::Up), [0, 0].into());
    }

    #[test]
    fn test_sub() {
        assert_eq!(PositionND([0, 2]) - PositionND([-1, 0]), [1, 2].into());
        assert_eq!(PositionND([0, -1]) - PositionND([0, -1]), [0, 0].into());
    }

    #[test]
    fn test_mul() {
        assert_eq!(PositionND([0, 2]) * 5, [0, 10].into());
        assert_eq!(PositionND([0, -1]) * -2, [0, 2].into());
    }
}
