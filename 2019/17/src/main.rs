use grid::*;
use intcode::*;
use std::char;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, PartialEq)]
struct Movement {
    rotation: i8,
    distance: u8,
}

impl fmt::Display for Movement {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let dir_char = if self.rotation == 1 { 'R' } else { 'L' };
        write!(f, "{}{}", dir_char, self.distance)
    }
}

#[rustfmt::skip]
fn find_commands(field: &HashMap<Position2D, char>) -> Vec<Movement> {
    let mut robot_position = field.iter().find(|(_, c)| *c == &'^').unwrap().0.to_owned();
    let mut robot_direction = Direction::Up;
    let mut commands = Vec::new();
    loop {
        let mut steps = 0;
        let turn = if field.get(&(robot_position + (robot_direction + 1))) == Some(&'#') {
            1
        } else {
            -1
        };
        robot_direction += turn;
        while field.get(&(robot_position + robot_direction)) == Some(&'#') {
            robot_position += robot_direction;
            steps += 1;
        }
        commands.push(Movement {
            distance: steps,
            rotation: turn,
        });
        if robot_position.neighbors().iter().filter(|(_, p)| field.get(p) == Some(&'#')).count() == 1 {
            break;
        }
    }
    commands
}

fn test_input() -> HashMap<Position2D, char> {
    "#######...#####
#.....#...#...#
#.....#...#...#
......#...#...#
......#...###.#
......#.....#.#
^########...#.#
......#.#...#.#
......#########
........#...#..
....#########..
....#...#......
....#...#......
....#...#......
....#####......"
        .lines()
        .enumerate()
        .flat_map(move |(y, s)| s.chars().enumerate().map(move |(x, c)| ((x, y).into(), c)))
        .collect()
}

fn main() {
    // The main reason I use a hashmap here (instead of a 2D vector) is that my abstractions for
    // ascii stuff all use maps ヽ( ﾟヮ・)ノ
    let field: HashMap<Position2D, char> = IntComputer::without_params(read_input())
        .get_all_outputs()
        .iter()
        .map(|n| char::from_u32(*n as u32).unwrap())
        .collect::<String>()
        .lines()
        .enumerate()
        .flat_map(move |(y, s)| s.chars().enumerate().map(move |(x, c)| ((x, y).into(), c)))
        .collect();
    // let field = test_input();
    let p1 = field
        .iter()
        .filter(|(pos, obj)| {
            *obj == &'#'
                && pos
                    .neighbors()
                    .iter()
                    .all(|(_, p)| field.get(&p) == Some(&'#'))
        })
        .fold(0, |acc, (pos, _)| acc + pos.x * pos.y);
    println!("Part 1: {}", p1);
    println!("{}", draw_ascii(&field, '.'));

    let commands = find_commands(&field);
    for c in &commands {
        print!("{}", c);
    }
}
