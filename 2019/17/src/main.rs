use grid::*;
use intcode::*;
use itertools::Itertools;
use std::char;
use std::collections::{HashMap, HashSet};
use std::fmt;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct Movement {
    rotation: i8,
    distance: u8,
}

impl fmt::Display for Movement {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let dir_char = if self.rotation == 1 { 'R' } else { 'L' };
        write!(f, "{},{}", dir_char, self.distance)
    }
}

#[rustfmt::skip]
fn find_commands(field: &HashMap<Position2D, char>) -> Vec<Movement> {
    let mut robot_position = field.iter().find(|(_, c)| *c == &'^').unwrap().0.to_owned();
    let mut robot_direction = Direction::Up;
    let mut commands = Vec::new();
    loop {
        let mut steps = 0;
        let turn = ((field.get(&(robot_position + (robot_direction + 1))) == Some(&'#')) as i8) * 2 - 1;
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

fn main() {
    // The main reason I use a hashmap here (instead of a 2D vector) is that my abstractions for
    // ascii stuff all use maps ヽ( ﾟヮ・)ノ
    let mut input = read_input();
    let field: HashMap<Position2D, char> = IntComputer::without_params(input.clone())
        .get_all_outputs()
        .iter()
        .map(|n| char::from_u32(*n as u32).unwrap())
        .collect::<String>()
        .lines()
        // this rev breaks part 1 but is necessary for part 2. remove it to get the part 1 solution
        .rev()
        .enumerate()
        .flat_map(move |(y, s)| s.chars().enumerate().map(move |(x, c)| ((x, y).into(), c)))
        .collect();
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

    let commands = find_commands(&field);
    let mut pos = 0;
    let mut segments = Vec::new();
    while pos < commands.len() - 4 {
        if let Some((n, mov)) = (2..=4)
            .filter_map(|i| {
                let reference = commands[pos..].windows(i).next();
                if segments.contains(&reference.unwrap()) {
                    Some(((i, 99), reference))
                } else {
                    let dupes = commands[pos..]
                        .windows(i)
                        .filter(|&w| Some(w) == reference)
                        .count();
                    if dupes > 0 {
                        Some(((i, dupes), reference))
                    } else {
                        None
                    }
                }
            })
            .max_by_key(|((x, y), _)| x * y)
        {
            pos += n.0;
            segments.push(mov.unwrap());
        }
    }
    let filtered: HashSet<_> = segments.clone().into_iter().collect();
    let mut filtered: Vec<_> = filtered.into_iter().zip(['A', 'B', 'C'].iter()).collect();
    filtered.sort_by_key(|(_, c)| c.to_owned());
    let mut instructions = Vec::new();
    let mut pos = 0;
    while pos < commands.len() {
        for i in 1.. {
            if filtered.iter().any(|(c, _)| c == &&commands[pos..pos + i]) {
                instructions.push(&commands[pos..pos + i]);
                pos += i;
                break;
            }
        }
    }
    input[0] = 2;
    let path: Vec<i64> = (instructions
        .iter()
        .map(|i| filtered.iter().find(|(f, _)| i == f).unwrap().1)
        .join(",")
        + "\n"
        + &filtered
            .into_iter()
            .map(|(f, _)| f.iter().map(|m| m.to_string()).join(","))
            .join("\n")
        + "\nn\n")
        .chars()
        .map(|c| c as i64)
        .rev()
        .collect();
    println!(
        "Part 2: {:?}",
        IntComputer::new(input, 0, path).get_all_outputs().pop().unwrap()
    );
}
