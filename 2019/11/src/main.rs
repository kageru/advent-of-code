use intcode::*;
use itertools::join;
use std::collections::HashMap;

enum Direction {
    Up,
    Down,
    Left,
    Right,
}

struct Robot {
    direction: Direction,
    position: (i64, i64),
    visited: HashMap<(i64, i64), i64>,
}

impl Robot {
    fn turn(&mut self, turn_value: i64) {
        self.direction = match turn_value {
            0 => match self.direction {
                Direction::Up => Direction::Left,
                Direction::Right => Direction::Up,
                Direction::Down => Direction::Right,
                Direction::Left => Direction::Down,
            },
            1 => match self.direction {
                Direction::Up => Direction::Right,
                Direction::Right => Direction::Down,
                Direction::Down => Direction::Left,
                Direction::Left => Direction::Up,
            },
            _ => unreachable!("Illegal turn value"),
        }
    }

    fn mv(&mut self) {
        let pos = self.position;
        self.position = match self.direction {
            Direction::Up => (pos.0, pos.1 + 1),
            Direction::Right => (pos.0 + 1, pos.1),
            Direction::Left => (pos.0 - 1, pos.1),
            Direction::Down => (pos.0, pos.1 - 1),
        }
    }

    fn paint(&mut self, color: i64) {
        self.visited.insert(self.position, color);
    }

    fn current_color(&self) -> i64 {
        *self.visited.get(&self.position).unwrap_or(&0)
    }
}

#[rustfmt::skip]
fn get_boundaries(positions: &HashMap<(i64, i64), i64>) -> (i64, i64, i64, i64) {
    let keys = positions.keys();
    let x_max = keys.clone().into_iter().max_by_key(|k| k.0).unwrap() .0;
    let y_max = keys.clone().into_iter().max_by_key(|k| k.1).unwrap() .1;
    let x_min = keys.clone().into_iter().min_by_key(|k| k.0).unwrap() .0;
    let y_min = keys.clone().into_iter().min_by_key(|k| k.1).unwrap() .1;
    (x_min, x_max, y_min, y_max)
}

fn start_with_input(input: Vec<i64>, color: i64) -> Robot {
    let mut robot = Robot {
        position: (0, 0),
        visited: HashMap::new(),
        direction: Direction::Up,
    };
    let mut pc = IntComputer::new(input, 0, vec![color]);
    while let IntComputerResult::Output(o) = pc.run() {
        robot.paint(o);
        let turn_int = pc.run().unwrap();
        robot.turn(turn_int);
        robot.mv();
        pc.params.push(robot.current_color());
    }
    robot
}

fn draw_ascii_text(coordinates: &HashMap<(i64, i64), i64>) -> String {
    let (x_min, x_max, y_min, y_max) = get_boundaries(&coordinates);
    join(
        (y_min..y_max + 1).rev().map(|y| {
            (x_min..x_max)
                .map(|x| coordinates.get(&(x, y)).unwrap_or(&0).to_string())
                .collect::<String>()
                .replace('0', " ")
                .replace('1', "█")
        }),
        "\n",
    )
}

fn main() {
    let input = read_input();
    let part1_robot = start_with_input(input.clone(), 0);
    println!("Part 1: {}", part1_robot.visited.len());

    let part2_robot = start_with_input(input.clone(), 1);
    println!("Part 2:\n{}", draw_ascii_text(&part2_robot.visited));
}
