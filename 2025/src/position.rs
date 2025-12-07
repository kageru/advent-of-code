extern crate test;
use crate::{
    common::Inc,
    direction::{ALL_DIRECTIONS, Direction},
};
use std::{
    fmt::Debug,
    hash::Hash,
    iter::Step,
    ops::{Add, AddAssign, Sub},
};

#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy, PartialOrd, Ord)]
pub struct Pos<I>(pub I, pub I);

impl<I: Default> Default for Pos<I> {
    fn default() -> Self {
        Pos(I::default(), I::default())
    }
}

impl<I: Inc + Add<I, Output = I> + AddAssign + Sub<I, Output = I> + Ord> Pos<I> {
    pub fn manhattan_distance(&self, other: &Self) -> I {
        abs_diff(self.0, other.0) + abs_diff(self.1, other.1)
    }
}

fn abs_diff<T: Sub<T, Output = T> + Ord>(a: T, b: T) -> T {
    if a > b { a - b } else { b - a }
}

impl<I> Add<Pos<I>> for Pos<I>
where I: Add<I, Output = I> + Copy
{
    type Output = Pos<I>;

    fn add(self, rhs: Pos<I>) -> Self::Output {
        Pos(self.0 + rhs.0, self.1 + rhs.1)
    }
}

impl<I> AddAssign<Pos<I>> for Pos<I>
where I: AddAssign<I> + Copy
{
    fn add_assign(&mut self, rhs: Pos<I>) {
        self.0 += rhs.0;
        self.1 += rhs.1;
    }
}

impl<I> Add<Direction> for Pos<I>
where I: Inc
{
    type Output = Self;

    fn add(self, rhs: Direction) -> Self::Output {
        let Pos(y, x) = self;
        match rhs {
            Direction::Up => Pos(y.inc(), x),
            Direction::Down => Pos(y.dec(), x),
            Direction::Right => Pos(y, x.inc()),
            Direction::Left => Pos(y, x.dec()),
        }
    }
}

impl Pos<usize> {
    pub fn checked_add(self, dir: Direction) -> Option<Self> {
        let Pos(y, x) = self;
        Some(match dir {
            Direction::Up => Pos(y + 1, x),
            Direction::Down => Pos(y.checked_sub(1)?, x),
            Direction::Right => Pos(y, x + 1),
            Direction::Left => Pos(y, x.checked_sub(1)?),
        })
    }
}

impl<I: Copy + Default + Step> Pos<I> {
    pub fn manhattan_neighbors_checked(&self) -> Vec<Pos<I>> {
        let Pos(x, y) = *self;
        let mut v = Vec::with_capacity(4);
        v.push(Pos(x.inc(), y));
        v.push(Pos(x, y.inc()));
        if let Some(x2) = x.dec_checked() {
            v.push(Pos(x2, y));
        }
        if let Some(y2) = y.dec_checked() {
            v.push(Pos(x, y2));
        }
        v
    }

    pub fn manhattan_neighbors(&self) -> [Pos<I>; 4] {
        ALL_DIRECTIONS.map(|d| *self + d)
    }
}

pub trait Neighbors: Sized {
    fn neighbors(&self) -> [Self; 8];
}

pub trait NeighborsChecked: Sized {
    fn neighbors_checked(&self) -> Vec<Self>;
}

impl NeighborsChecked for Pos<usize> {
    fn neighbors_checked(&self) -> Vec<Self> {
        let &Pos(x, y) = self;
        [
            try { Pos(x.checked_sub(1)?, y.checked_sub(1)?) },
            try { Pos(x.checked_sub(1)?, y) },
            try { Pos(x.checked_sub(1)?, y + 1) },
            try { Pos(x, y.checked_sub(1)?) },
            Some(Pos(x, y + 1)),
            try { Pos(x, y.checked_sub(1)?) },
            Some(Pos(x + 1, y)),
            Some(Pos(x + 1, y + 1)),
        ]
        .into_iter()
        .flatten()
        .collect()
    }
}

macro_rules! neighbors_signed {
    ($($i: ty),*) => {
        $(impl Neighbors for Pos<$i> {
            fn neighbors(&self) -> [Self; 8] {
                let &Pos(x, y) = self;
                [
                    Pos(x - 1, y - 1),
                    Pos(x - 1, y),
                    Pos(x - 1, y + 1),
                    Pos(x, y - 1),
                    Pos(x, y + 1),
                    Pos(x + 1, y - 1),
                    Pos(x + 1, y),
                    Pos(x + 1, y + 1),
                ]
            }
        })*
    };
}

neighbors_signed!(i32, i64, isize);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_neighbors() {
        let p = Pos(0, 0);
        let n = p.neighbors();
        assert_eq!(n, [Pos(-1, -1), Pos(-1, 0), Pos(-1, 1), Pos(0, -1), Pos(0, 1), Pos(1, -1), Pos(1, 0), Pos(1, 1)]);

        let p = Pos(1, 1);
        let n = p.neighbors();
        assert_eq!(n, [Pos(0, 0), Pos(0, 1), Pos(0, 2), Pos(1, 0), Pos(1, 2), Pos(2, 0), Pos(2, 1), Pos(2, 2)])
    }

    #[test]
    fn test_checked_neighbors() {
        let p = Pos(0usize, 0);
        assert_eq!(p.neighbors_checked(), &[Pos(0, 1), Pos(1, 0), Pos(1, 1)])
    }

    #[test]
    fn test_manhattan_distance() {
        assert_eq!(Pos(5, 10).manhattan_distance(&Pos(15, 20)), 20);
        assert_eq!(Pos(15, 10).manhattan_distance(&Pos(5, 20)), 20);
        assert_eq!(Pos(10, 10).manhattan_distance(&Pos(10, 10)), 0);
        assert_eq!(Pos(0, 0).manhattan_distance(&Pos(1, 1)), 2);
        assert_eq!(Pos(2, 0).manhattan_distance(&Pos(0, -5)), 7);
    }
}
