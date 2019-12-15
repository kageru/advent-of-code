use grid::*;
use intcode::*;
use rand::random;
use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};
use std::sync::Mutex;
#[macro_use]
extern crate lazy_static;

#[derive(Clone, Copy, PartialEq, Debug)]
enum Tile {
    Unknown,
    Wall,
    Empty,
    Oxygen,
    Bot,
}

#[derive(Clone)]
struct Robot {
    pos: Position2D,
    dir: Direction,
    ic: IntComputer,
    steps: usize,
}

impl Robot {
    fn turn(&mut self) {
        self.dir.turn((random::<bool>() as i64) * 2 - 1);
        for d in &ALL_DIRECTIONS {
            if get(&(self.pos + *d)).is_none() {
                self.dir = *d;
            }
        }
    }

    fn step(&mut self, dir: &Direction) -> Tile {
        self.dir = *dir;
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
                let mut r = OXYGEN_BOT.lock().unwrap();
                *r = Some(self.clone());
                println!("Part 1: {}", self.steps);
                Tile::Oxygen
            }
            _ => unreachable!("This roboter shouldn’t halt"),
        }
    }
    /*
    fn step(&mut self) {
        match self.ic.run() {
            IntComputerResult::Output(0) => {
                save(self.pos + self.dir, Tile::Wall);
                self.turn();
                self.ic.params.push(int(&self.dir));
            }
            IntComputerResult::Output(1) => {
                self.pos += self.dir;
                save(self.pos, Tile::Empty);
                self.turn();
                self.ic.params.push(int(&self.dir));
            }
            IntComputerResult::Output(2) => {
                self.pos += self.dir;
                save(self.pos, Tile::Empty);
                self.turn();
                self.ic.params.push(int(&self.dir));
            }
            _ => unreachable!("This roboter shouldn’t halt"),
        };
    }
    */
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
            Tile::Unknown => '•',
            Tile::Wall => '█',
            Tile::Empty => ' ',
            Tile::Oxygen => 'X',
            Tile::Bot => 'O',
        };
        write!(f, "{}", c)
    }
}

fn int(dir: &Direction) -> i64 {
    match dir {
        Direction::Up => 1,
        Direction::Down => 2,
        Direction::Left => 3,
        Direction::Right => 4,
    }
}

lazy_static! {
    static ref FIELD: Mutex<HashMap<Position2D, Tile>> = Mutex::new(HashMap::new());
    static ref OXYGEN_BOT: Mutex<Option<Robot>> = Mutex::new(None);
}

#[inline]
fn get(p: &Position2D) -> Option<Tile> {
    FIELD.lock().unwrap().get(p).map(|o| *o)
}

#[inline]
fn save(p: Position2D, t: Tile) {
    FIELD.lock().unwrap().insert(p, t);
}

fn print_field() {
    println!(
        "\n{}\n",
        draw_ascii(&FIELD.lock().unwrap(), Tile::Wall)
    );
}

fn explore(bot: Robot) {
    let nbs = bot.pos.neighbors();
    for (dir, pos) in nbs.iter() {
        if get(pos).is_none() {
            let mut clone = bot.clone();
            if clone.step(dir) == Tile::Empty {
                // Pretty visuals :wow:
                // std::thread::sleep(std::time::Duration::from_millis(33));
                // print_field();
                explore(clone);
            }

        }
    }
}

fn main() {
    let bot = Robot {
        pos: (0, 0).into(),
        dir: Direction::Up,
        ic: IntComputer::new(read_input(), 0, vec![]),
        steps: 0,
    };
    save(bot.pos, Tile::Unknown);
    explore(bot);
    //print_field();
}
