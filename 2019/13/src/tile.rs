use std::fmt::{Display, Error, Formatter};
pub enum Tile {
    Empty,
    Wall,
    Block,
    Paddle,
    Ball,
}

impl Display for Tile {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        let c: char = self.into();
        write!(fmt, "{}", c)
    }
}

impl From<i64> for Tile {
    fn from(num: i64) -> Self {
        match num {
            0 => Tile::Empty,
            1 => Tile::Wall,
            2 => Tile::Block,
            3 => Tile::Paddle,
            4 => Tile::Ball,
            _ => unreachable!("Illegal tile ID"),
        }
    }
}

impl Into<char> for &Tile {
    fn into(self) -> char {
        match self {
            Tile::Empty => ' ',
            Tile::Wall => '█',
            Tile::Block => '⌂',
            Tile::Paddle => '_',
            Tile::Ball => '•',
        }
    }
}
