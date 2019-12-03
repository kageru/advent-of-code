use std::collections::{HashMap, HashSet};
use std::io::{self, BufRead};

struct Movement {
    pub direction: char,
    pub distance: u16,
}

impl Movement {
    pub fn new(raw: &str) -> Self {
        Self {
            direction: raw.chars().nth(0).unwrap(),
            distance: raw.get(1..).unwrap().parse().unwrap(),
        }
    }
}

fn make_step(x: &mut i32, y: &mut i32, dir: char) {
    match dir {
        'U' => *y += 1,
        'D' => *y -= 1,
        'L' => *x += 1,
        _ => *x -= 1,
    };
}

pub fn main() {
    let stdin = io::stdin();
    let mut input = stdin.lock().lines().map(|s| {
        s.unwrap()
            .split(',')
            .map(|m| Movement::new(m))
            .collect::<Vec<_>>()
    });
    let (first_wire, second_wire) = (input.next().unwrap(), input.next().unwrap());
    let mut first_wire_positions = HashSet::new();
    let (mut x, mut y) = (0, 0);
    for m in first_wire {
        for _ in 0..m.distance {
            make_step(&mut x, &mut y, m.direction);
            first_wire_positions.insert((x, y));
        }
    }
    x = 0;
    y = 0;
    let mut intersections = HashMap::new();
    for m in second_wire {
        for _ in 0..m.distance {
            make_step(&mut x, &mut y, m.direction);
            if first_wire_positions.contains(&(x, y)) {
                intersections.insert((x, y), 0);
            }
        }
    }
    println!("Part 1: {}", intersections.keys().map(|(x, y)| x.abs() + y.abs()).min().unwrap());
}
