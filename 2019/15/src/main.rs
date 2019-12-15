use grid::*;
use intcode::*;
use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};
use std::sync::Mutex;
#[macro_use]
extern crate lazy_static;

#[derive(Clone, Copy, PartialEq, Debug)]
enum Tile {
    Wall,
    Empty,
    Oxygen,
    O2,
}

#[derive(Clone)]
struct Robot {
    pos: Position2D,
    dir: Direction,
    ic: IntComputer,
    steps: usize,
}

impl Robot {
    fn step(&mut self, dir: Direction) -> Tile {
        self.dir = dir;
        self.ic.params.push(int(dir));
        match self.ic.run() {
            IntComputerResult::Output(0) => {
                save(self.pos + self.dir, Tile::Wall);
                Tile::Wall
            }
            IntComputerResult::Output(1) => {
                self.pos += self.dir;
                save(self.pos, Tile::Empty);
                self.steps += 1;
                Tile::Empty
            }
            IntComputerResult::Output(2) => {
                self.pos += self.dir;
                self.steps += 1;
                save(self.pos, Tile::Oxygen);
                OXYGEN_BOT.lock().unwrap().push(self.clone());
                Tile::Oxygen
            }
            _ => unreachable!("This roboter shouldn’t halt"),
        }
    }
}

impl From<i64> for Tile {
    fn from(i: i64) -> Self {
        match i {
            0 => Tile::Wall,
            1 => Tile::Empty,
            2 => Tile::Oxygen,
            _ => unreachable!("Illegal tile"),
        }
    }
}

impl Display for Tile {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        let c = match self {
            Tile::Wall => '█',
            Tile::Empty => ' ',
            Tile::Oxygen => 'X',
            Tile::O2 => 'O',
        };
        write!(f, "{}", c)
    }
}

fn int(dir: Direction) -> i64 {
    match dir {
        Direction::Up => 1,
        Direction::Down => 2,
        Direction::Left => 3,
        Direction::Right => 4,
    }
}

lazy_static! {
    static ref FIELD: Mutex<HashMap<Position2D, Tile>> = Mutex::new(HashMap::new());
    static ref OXYGEN_BOT: Mutex<Vec<Robot>> = Mutex::new(Vec::with_capacity(1));
}

#[inline]
fn get(p: &Position2D) -> Option<Tile> {
    FIELD.lock().unwrap().get(p).copied()
}

#[inline]
fn save(p: Position2D, t: Tile) {
    FIELD.lock().unwrap().insert(p, t);
}

fn print_field() {
    if ENABLE_MAP_PRINT {
        std::thread::sleep(std::time::Duration::from_millis(16));
        println!("\n{}\n", draw_ascii(&FIELD.lock().unwrap(), Tile::Wall));
    }
}

#[rustfmt::skip]
fn is_dead_end(pos: &Position2D) -> bool {
    pos.neighbors().iter().filter(|(_, p)| get(p).unwrap() == Tile::Empty).count() == 0
}

fn fill(bot: Robot) -> usize {
    if is_dead_end(&bot.pos) {
        return bot.steps;
    }
    bot.pos
        .neighbors()
        .iter()
        .map(|(dir, pos)| {
            if get(pos).unwrap() == Tile::Empty {
                let mut clone = bot.clone();
                clone.pos += *dir;
                save(clone.pos, Tile::O2);
                clone.steps += 1;
                print_field();
                fill(clone)
            } else {
                0
            }
        })
        .max()
        .unwrap_or(0)
}

fn explore(bot: Robot) {
    let nbs = bot.pos.neighbors();
    for (dir, pos) in nbs.iter() {
        if get(pos).is_none() {
            let mut clone = bot.clone();
            if clone.step(*dir) == Tile::Empty {
                print_field();
                explore(clone);
            }
        }
    }
}

#[rustfmt::skip]
fn get_bot_at_generator() -> Robot {
    OXYGEN_BOT.lock().unwrap().pop().expect("No oxygen found in Part 1")
}

// Pretty visuals :wow:
const ENABLE_MAP_PRINT: bool = true;

fn main() {
    let mut bot = Robot {
        pos: (0, 0).into(),
        dir: Direction::Up,
        ic: IntComputer::new(read_input(), 0, vec![]),
        steps: 0,
    };
    save(bot.pos, Tile::Empty);
    explore(bot);
    bot = get_bot_at_generator();
    println!("Part 1: {}", bot.steps);
    bot.steps = 0;
    println!("Part 2: {}", fill(bot));
}
