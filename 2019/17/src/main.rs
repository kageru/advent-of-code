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

/// The main reason I use a hashmap here (instead of a 2D vector)
/// is that my abstractions for ascii stuff all use maps ヽ( ﾟヮ・)ノ
fn build_field(input: &[i64]) -> HashMap<Position2D, char> {
    IntComputer::without_params(input.to_vec())
        .get_all_outputs()
        .iter()
        .map(|n| char::from_u32(*n as u32).unwrap())
        .collect::<String>()
        .lines()
        .rev()
        .enumerate()
        .flat_map(move |(y, s)| s.chars().enumerate().map(move |(x, c)| ((x, y).into(), c)))
        .collect()
}

fn part1(field: &HashMap<Position2D, char>) -> i64 {
    // For some reason, the math for part 1 is upside down. This compensates for that. ¯\_(ツ)_/¯
    let max_y = field.keys().max_by_key(|p| p.y).unwrap().y;
    field
        .iter()
        .filter(|(pos, obj)| {
            *obj == &'#'
                && pos
                    .neighbors()
                    .iter()
                    .all(|(_, p)| field.get(&p) == Some(&'#'))
        })
        .fold(0, |acc, (pos, _)| acc + pos.x * (max_y - pos.y))
}

fn part2(field: &HashMap<Position2D, char>) -> Vec<i64> {
    let movements = find_all_movements(&field);
    let mut functions: Vec<_> = split_into_functions(&movements)
        .into_iter()
        // To remove duplicates
        .collect::<HashSet<_>>()
        .into_iter()
        .zip(['A', 'B', 'C'].iter())
        .collect();
    // Get them in order A, B, C. Makes the output easier later.
    functions.sort_by_key(|(_, c)| c.to_owned());
    let function_calls = get_function_calls(&movements, &functions);
    (function_calls.iter().join(",")
        + "\n"
        + &functions
            .into_iter()
            .map(|(f, _)| function_to_string(f))
            .join("\n")
        + "\nn\n")
        .chars()
        .map(|c| c as i64)
        .rev()
        .collect()
}

#[rustfmt::skip]
fn find_all_movements(field: &HashMap<Position2D, char>) -> Vec<Movement> {
    let mut robot_position = field.iter().find(|(_, c)| *c == &'^').unwrap().0.to_owned();
    let mut robot_direction = Direction::Up;
    let mut commands = Vec::new();
    loop {
        let mut steps = 0;
        // Check if the next valid tile is to the left or the right
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
        // hit the dead end -> end of scaffolding
        if robot_position.neighbors().iter().filter(|(_, p)| field.get(p) == Some(&'#')).count() == 1 {
            break;
        }
    }
    commands
}

fn function_to_string(function: &[Movement]) -> String {
    function.iter().map(|m| m.to_string()).join(",")
}

fn find_reoccuring_segments<'a>(
    commands: &'a [Movement],
    previous_segments: &[&'a [Movement]],
    starting_pos: &mut usize,
    length: usize,
) -> Option<(usize, usize, &'a [Movement])> {
    let reference = commands[*starting_pos..].windows(length).next().unwrap();
    // If the segment we’re currently looking at (reference) is already in our function list,
    // add it with the highest priority. (We’ll deduplicate later)
    if previous_segments.contains(&reference) {
        return Some((length, 99, reference));
    }
    let dupes = commands[*starting_pos..]
        .windows(length)
        .filter(|&w| w == reference)
        .count();
    if dupes > 1 {
        Some((length, dupes, reference))
    } else {
        None
    }
}

/**
 * From each position, try to find the slice of the next 2-4 (inclusive)
 * elements that reoccur as often as possible in the output.
 */
fn split_into_functions<'a>(commands: &'a [Movement]) -> Vec<&'a [Movement]> {
    let mut pos = 0;
    let mut segments = Vec::new();
    while pos < commands.len() - 4 {
        if let Some((segment_length, _, mov)) = (2..=4)
            .filter_map(|i| find_reoccuring_segments(commands, &segments, &mut pos, i))
            // Segments are rated by length and number of occurences
            .max_by_key(|(x, y, _)| x * y)
        {
            pos += segment_length;
            segments.push(mov);
        }
    }
    segments
}

#[rustfmt::skip]
/// Find matching function calls for all movement sequences.
fn get_function_calls<'a>(
    movements: &'a[Movement],
    functions: &[(&'a [Movement], &'a char)],
) -> Vec<char> {
    let mut function_calls = Vec::new();
    let mut pos = 0;
    while pos < movements.len() {
        for i in 1.. {
            if let Some((_, chr)) = functions.iter().find(|(m, _)| m == &&movements[pos..pos + i]) {
                function_calls.push(chr.to_owned().to_owned());
                pos += i;
                break;
            }
        }
    }
    function_calls
}

fn main() {
    let mut input = read_input();
    let field = build_field(&input);
    let p1 = part1(&field);
    println!("Part 1: {}", p1);

    input[0] = 2;
    let program = part2(&field);
    println!(
        "Part 2: {:?}",
        IntComputer::new(input, 0, program)
            .get_all_outputs()
            .pop()
            .unwrap()
    );
}
