use grid::*;
use std::collections::HashMap;
use std::fmt;
use std::io::{self, BufRead};

const FIELD_SIZE: usize = 5;

#[derive(Copy, Debug, Clone, PartialEq, Eq)]
enum State {
    Alive,
    Dead,
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                State::Alive => '#',
                State::Dead => '.',
            }
        )
    }
}

impl From<char> for State {
    fn from(c: char) -> Self {
        match c {
            '.' => Self::Dead,
            '#' => Self::Alive,
            _ => unreachable!(),
        }
    }
}

fn state_for_live_neighbors(state: &State, n: usize) -> State {
    match (state, n) {
        (_, 1) => State::Alive,
        (State::Dead, 2) => State::Alive,
        _ => State::Dead,
    }
}

fn main() {
    let mut fields: HashMap<Position2D, State> = io::stdin()
        .lock()
        .lines()
        .map(|l| l.unwrap())
        .enumerate()
        .flat_map(move |(y, l)| {
            l.chars()
                .enumerate()
                .map(move |(x, c)| ((x, y).into(), c.into()))
                .collect::<Vec<_>>()
        })
        .collect();
    loop {
        println!("{}", draw_ascii(&fields, State::Dead));
        fields = (0..FIELD_SIZE)
            .flat_map(|y| {
                (0..FIELD_SIZE)
                    .map(|x| {
                        let p = Position2D::from((x, y));
                        let state = state_for_live_neighbors(
                            fields.get(&p).unwrap(),
                            p.moore()
                                .iter()
                                .map(|p| fields.get(&p).unwrap_or(&State::Dead))
                                .filter(|&&s| s == State::Alive)
                                .count(),
                        );
                        (Position2D::from((x, y)), state)
                    })
                    // .map(|alive_neighbors| (Position2D::from((x, y)), *state))
                    .collect::<Vec<_>>()
            })
            .collect();
    }
}
