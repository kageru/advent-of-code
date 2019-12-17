use grid::*;
use intcode::*;
use std::char;
use std::collections::{HashMap,HashSet};
use std::fmt;

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
    //let field = test_input();
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
    for c in commands.iter() { println!("{}", c); }
    let mut pos = 0;
    //let mut segments = Vec::new();
    let segments = vec![
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
    ];
    /*
    while pos < commands.len() {
        if let Some((n, mov)) = (1..=5)
            .filter_map(|i| {
                let reference = commands[pos..].windows(i).next();
                let dupes = commands[pos..]
                    .windows(i)
                    .filter(|&w| Some(w) == reference)
                    .count();
                if dupes > 0 {
                    Some(((i, dupes), reference))
                } else {
                    None
                }
            })
            .max_by_key(|((x, y), _)| x + y*2)
        {
            pos += n.0;
            segments.push(mov.unwrap());
        }
    }
    let filtered: HashSet<_> = segments.clone().into_iter().map(|mut curr| {
        for s in &segments {
            if curr.len() <= s.len() {
                continue
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
    .collect();
    //dbg!(&segments, segments.len());
    dbg!(filtered.len());
    println!(
        "{}",
        filtered
            .iter()
            .map(|s| s.iter().map(|m| m.to_string()).collect::<String>() + "\n")
            .collect::<String>()
    );
    */
    let filtered = segments;
    let mut instructions = Vec::new();
    let mut pos = 0;
    while pos < commands.len() {
        for i in 1.. {
            if filtered.contains(&commands[pos..pos+i].to_vec()) {
                instructions.push(&commands[pos..pos+i]);
                pos += i;
                println!("match for {}..{}", pos, i);
                break;
            } else {
                println!("no match for {}..{}", pos, i);
            }
        }
    }
    for i in instructions {
        println!("{}", i.iter().map(|m| m.to_string()).collect::<String>());
    }
    
    // it’s surprisingly easy to do this manually with enough debug prints above.
    // proper solution once the headache is gone. fml
    input[0] = 2;
    let mut path: Vec<i64> = "A,B,A,B,C,C,B,A,B,C\nL,8,R,12,R,12,R,10\nR,10,R,12,R,10\nL,10,R,10,L,6\nn\n".chars().map(|c| c as i64).collect();
    path.reverse();
    println!("Part 2: {:?}", IntComputer::new(input, 0, path).get_all_outputs());
}
