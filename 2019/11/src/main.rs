use intcode::*;
use grid::*;
use std::collections::HashMap;

enum Direction {
    Up,
    Down,
    Left,
    Right,
}

struct Robot {
    direction: Direction,
    position: Position2D,
    visited: HashMap<Position2D, i64>,
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
            Direction::Up => pos + (0, 1).into(),
            Direction::Right => pos + (1, 0).into(),
            Direction::Left => pos + (-1, 0).into(),
            Direction::Down => pos + (0, -1).into(),
        }
    }

    fn paint(&mut self, color: i64) {
        self.visited.insert(self.position, color);
    }

    fn current_color(&self) -> i64 {
        *self.visited.get(&self.position).unwrap_or(&0)
    }
}

fn start_with_input(input: Vec<i64>, color: i64) -> Robot {
    let mut robot = Robot {
        position: (0, 0).into(),
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

fn main() {
    let input = read_input();
    let part1_robot = start_with_input(input.clone(), 0);
    println!("Part 1: {}", part1_robot.visited.len());

    let part2_robot = start_with_input(input.clone(), 1);
    println!("Part 2:\n{}", draw_ascii(&part2_robot.visited, 0).replace('0', " ").replace('1', "•"));
}
