use grid::*;
use intcode::*;
use std::collections::HashMap;

struct Robot {
    direction: Direction,
    position: Position2D,
    visited: HashMap<Position2D, i64>,
}

impl Robot {
    fn mov(&mut self) {
        self.position.mov(&self.direction);
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
        robot.direction.turn(turn_int * 2 - 1);
        robot.mov();
        pc.params.push(robot.current_color());
    }
    robot
}

fn main() {
    let input = read_input();
    let part1_robot = start_with_input(input.clone(), 0);
    println!("Part 1: {}", part1_robot.visited.len());

    let part2_robot = start_with_input(input, 1);
    println!(
        "Part 2:\n{}",
        draw_ascii(&part2_robot.visited, 0)
            .replace('0', " ")
            .replace('1', "•")
    );
}
