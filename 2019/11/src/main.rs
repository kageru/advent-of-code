use intcode::*;
use std::collections::HashMap;

enum Direction {
    Up,
    Down,
    Left,
    Right,
}

fn turn(dir: Direction, turn_value: i64) -> Direction {
    match turn_value {
        0 => match dir {
            Direction::Up => Direction::Left,
            Direction::Right => Direction::Up,
            Direction::Down => Direction::Right,
            Direction::Left => Direction::Down,
        },
        1 => match dir {
            Direction::Up => Direction::Right,
            Direction::Right => Direction::Down,
            Direction::Down => Direction::Left,
            Direction::Left => Direction::Up,
        },
        _ => unreachable!("Illegal turn value"),
    }
}

fn mv(pos: (i64, i64), dir: &Direction) -> (i64, i64) {
    match dir {
        Direction::Up => (pos.0, pos.1 + 1),
        Direction::Right => (pos.0 + 1, pos.1),
        Direction::Left => (pos.0 - 1, pos.1),
        Direction::Down => (pos.0, pos.1 - 1),
    }
}

fn main() {
    let mut positions = HashMap::new();
    let mut pc = IntComputer::new(read_input(), 0, vec![1]);
    let mut pos = (0, 0);
    let mut direction = Direction::Up;
    while let IntComputerResult::Output(o) = pc.run() {
        positions.insert(pos, o);
        let turn_int = pc.run().unwrap();
        direction = turn(direction, turn_int);
        pos = mv(pos, &direction);
        pc.params.push(*positions.get(&pos).unwrap_or(&0));
    }
    // 
    println!("Part 1: {}", positions.len());
    let x_max = positions
        .clone()
        .keys()
        .into_iter()
        .max_by_key(|k| k.0)
        .unwrap()
        .0;
    let y_max = positions
        .clone()
        .keys()
        .into_iter()
        .max_by_key(|k| k.1)
        .unwrap()
        .1;
    let x_min = positions
        .clone()
        .keys()
        .into_iter()
        .min_by_key(|k| k.0)
        .unwrap()
        .0;
    let y_min = positions
        .clone()
        .keys()
        .into_iter()
        .min_by_key(|k| k.1)
        .unwrap()
        .1;
    for y in (y_min..y_max+1).rev() {
        println!(
            "{}",
            (x_min..x_max)
                .map(|x| positions.get(&(x, y)).unwrap_or(&0).to_string())
                .collect::<String>()
                .replace('0', " ")
                .replace('1', "█")
        );
    }
}
