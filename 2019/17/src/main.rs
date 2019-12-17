use grid::*;
use intcode::*;
use std::char;
use std::collections::{HashMap, HashSet};
use std::fmt;
use itertools::Itertools;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
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
        .rev()
        .enumerate()
        .flat_map(move |(y, s)| s.chars().enumerate().map(move |(x, c)| ((x, y).into(), c)))
        .collect()
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
        // this rev breaks part 1 but is necessary for part 2
        .rev()
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
    println!("Commands: {}", commands.len());
    for c in commands.iter() {
        println!("{}", c);
    }
    let mut pos = 0;
    let mut segments = Vec::new();
    /*let segments = vec![
        vec![
        Movement{ rotation: -1, distance: 8 },
        Movement{ rotation: 1, distance: 12 },
        Movement{ rotation: 1, distance: 12 },
        Movement{ rotation: 1, distance: 10 },
        ],
        vec![
        Movement{ rotation: 1, distance: 10 },
        Movement{ rotation: 1, distance: 12 },
        Movement{ rotation: 1, distance: 10 },
        ],
        vec![
        Movement{ rotation: -1, distance: 10 },
        Movement{ rotation: 1, distance: 10 },
        Movement{ rotation: -1, distance: 6 },
        ]
    ];*/
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
            .inspect(|_| println!("hit for {}", pos))
            .max_by_key(|((x, y), _)| x * y)
        {
            pos += n.0;
            segments.push(mov.unwrap());
        } else {
            //panic!("nothing found at position {}", pos);
        }
    }
    let filtered: HashSet<_> = segments
        .clone()
        .into_iter()
        /*
        .map(|mut curr| {
            for s in &segments {
                if curr.len() <= s.len() {
                    continue;
                }
                let index_after = curr.len() - s.len();
                if s == &&curr[index_after..] {
                    curr = &curr[..index_after];
                }
                if s == &&curr[..index_after] {
                    curr = &curr[index_after..];
                }
            }
            curr
        })
        .filter(|s| !s.is_empty())
        */
        .collect();
    let mut filtered: Vec<_> = filtered
        .into_iter()
        .zip(['A', 'B', 'C'].iter())
        .collect();
    filtered.sort_by_key(|(_, c)| c.to_owned());
    dbg!(&segments, segments.len());
    dbg!(filtered.len());
    println!(
        "{}",
        filtered
            .iter()
            .map(|s| s.0.iter().map(|m| m.to_string()).collect::<String>() + "\n")
            .collect::<String>()
    );
    //let filtered = segments;
    let mut instructions = Vec::new();
    let mut pos = 0;
    while pos < commands.len() {
        println!("searching");
        for i in 1.. {
            if filtered.iter().any(|(c, _)| c == &&commands[pos..pos + i]) {
                instructions.push(&commands[pos..pos + i]);
                pos += i;
                println!("match for {}..{}", pos, i);
                break;
            } else {
                println!("no match for {}..{}", pos, i);
            }
        }
    }
    fn dir(rotation: i8) -> char {
        if rotation == 1 { 'R' } else { 'L' }
    }
    input[0] = 2;
    let raw_path = (instructions.iter().map(|i| filtered.iter().find(|(f, c)| i == f).unwrap().1).join(",")
        + "\n"
        + &filtered.into_iter().map(|(f, _)| f.iter().map(|m| format!("{},{}", dir(m.rotation), m.distance)).join(",")).join("\n")
        + "\nn\n");
    let path: Vec<i64> = raw_path
            .chars()
            .map(|c| c as i64)
            .rev()
            .collect();
    println!(
        "Part 2: {:?}",
        IntComputer::new(input, 0, path).get_all_outputs().pop()
    );
}
