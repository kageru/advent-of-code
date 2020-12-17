use std::{
    fmt::{self, Display, Formatter}, hash::Hash
};

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
