use crate::position::Pos;
use itertools::{Itertools, MinMaxResult, join};
use std::{
    collections::HashMap,
    fmt::Display,
    hash::BuildHasher,
    ops::{Index, IndexMut},
};

pub trait Grid<T, I> {
    fn get_mut(&mut self, pos: &Pos<I>) -> Option<&mut T>;

    fn get(&self, pos: &Pos<I>) -> Option<&T>;

    fn insert(&mut self, pos: Pos<I>, element: T);

    fn len(&self) -> usize;

    fn is_empty(&self) -> bool {
        self.len() != 0
    }

    fn indices(&self) -> impl Iterator<Item = Pos<I>>;

    fn find(&self, needle: &T) -> Option<Pos<I>>;
}

#[derive(Debug, Clone, PartialEq)]
pub struct HashGrid<T: Default> {
    pub fields: HashMap<Pos<i64>, T>,
}

impl<T: PartialEq> IndexMut<Pos<usize>> for VecGrid<T> {
    fn index_mut(&mut self, Pos(y, x): Pos<usize>) -> &mut Self::Output {
        &mut self.0[y][x]
    }
}

impl<T: PartialEq> Index<Pos<usize>> for VecGrid<T> {
    type Output = T;

    fn index(&self, Pos(y, x): Pos<usize>) -> &Self::Output {
        &self.0[y][x]
    }
}

impl<T: Default + PartialEq> Grid<T, i64> for HashGrid<T> {
    fn get_mut(&mut self, pos: &Pos<i64>) -> Option<&mut T> {
        self.fields.get_mut(pos)
    }

    fn get(&self, pos: &Pos<i64>) -> Option<&T> {
        self.fields.get(pos)
    }

    fn insert(&mut self, pos: Pos<i64>, t: T) {
        self.fields.insert(pos, t);
    }

    fn len(&self) -> usize {
        self.fields.len()
    }

    fn is_empty(&self) -> bool {
        self.len() != 0
    }

    fn indices(&self) -> impl Iterator<Item = Pos<i64>> {
        self.fields.keys().cloned()
    }

    fn find(&self, needle: &T) -> Option<Pos<i64>> {
        self.fields.iter().find_map(|(p, v)| (v == needle).then_some(*p))
    }
}

impl<T: Default + Copy> HashGrid<T> {
    pub fn from_bytes_2d<F: FnMut(u8) -> T + Copy>(raw: &str, mut f: F) -> HashGrid<T> {
        raw.lines().enumerate().flat_map(move |(y, l)| l.bytes().enumerate().map(move |(x, c)| (Pos(x as i64, y as i64), f(c)))).collect()
    }
}

impl<T: Default> std::iter::FromIterator<(Pos<i64>, T)> for HashGrid<T> {
    fn from_iter<I: IntoIterator<Item = (Pos<i64>, T)>>(iter: I) -> Self {
        HashGrid { fields: iter.into_iter().collect() }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VecGrid<T: PartialEq>(pub Vec<Vec<T>>);

impl<T: PartialEq> Grid<T, usize> for VecGrid<T> {
    fn get(&self, pos: &Pos<usize>) -> Option<&T> {
        self.0.get(pos.0)?.get(pos.1)
    }

    fn get_mut(&mut self, pos: &Pos<usize>) -> Option<&mut T> {
        self.0.get_mut(pos.0)?.get_mut(pos.1)
    }

    fn insert(&mut self, p: Pos<usize>, element: T) {
        self[p] = element;
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    fn indices(&self) -> impl Iterator<Item = Pos<usize>> {
        (0..self.len()).flat_map(|y| (0..self.0[0].len()).map(move |x| Pos(y, x)))
    }

    fn find(&self, needle: &T) -> Option<Pos<usize>> {
        self.indices().find(|p| self.get(p) == Some(needle))
    }
}

impl<T: Copy + PartialEq> VecGrid<T> {
    pub fn from_bytes_2d<F: FnMut(u8) -> T + Copy>(raw: &str, f: F) -> VecGrid<T> {
        // reverse here so the bottom line is at y=0
        VecGrid(raw.lines().rev().map(|l| l.bytes().map(f).collect()).collect())
    }

    pub fn transmute_from_lines(raw: &str) -> VecGrid<T> {
        VecGrid(raw.lines().rev().map(|l| unsafe { std::mem::transmute::<&[u8], &[T]>(l.as_bytes()) }.to_vec()).collect())
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.0.iter().flatten()
    }
}

impl<T: Default + PartialEq> std::iter::FromIterator<Vec<T>> for VecGrid<T> {
    fn from_iter<I: IntoIterator<Item = Vec<T>>>(iter: I) -> Self {
        VecGrid(iter.into_iter().collect())
    }
}

pub struct Boundaries<I> {
    pub x_min: I,
    pub x_max: I,
    pub y_min: I,
    pub y_max: I,
}

pub fn get_boundaries<I: Ord + Default + Copy>(input: &[&Pos<I>]) -> Boundaries<I> {
    let (x_min, x_max) = match input.iter().map(|p| p.0).minmax() {
        MinMaxResult::NoElements => (I::default(), I::default()),
        MinMaxResult::MinMax(min, max) => (min, max),
        MinMaxResult::OneElement(x) => (x, x),
    };
    let (y_min, y_max) = match input.iter().map(|p| p.1).minmax() {
        MinMaxResult::NoElements => (I::default(), I::default()),
        MinMaxResult::MinMax(min, max) => (min, max),
        MinMaxResult::OneElement(x) => (x, x),
    };
    Boundaries { x_min, x_max, y_min, y_max }
}

pub fn draw_ascii<T: Display + Default, S: BuildHasher>(coordinates: &HashMap<Pos<i64>, T, S>) -> String {
    let b = get_boundaries(&coordinates.keys().collect::<Vec<_>>());
    join(
        (b.y_min..=b.y_max)
            .rev()
            .map(|y| (b.x_min..=b.x_max).map(|x| coordinates.get(&Pos(x, y)).unwrap_or(&T::default()).to_string()).collect::<String>()),
        "\n",
    )
}

pub fn draw<T: Copy + PartialEq, F: Fn(T) -> char>(grid: &VecGrid<T>, draw: F) -> String {
    join((0..grid.len()).rev().map(|y| (0..(grid.0[0].len())).map(|x| draw(grid.0[y][x])).collect::<String>()), "\n")
}
