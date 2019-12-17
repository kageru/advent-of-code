use grid::*;
use intcode::*;
use std::char;
use std::collections::HashMap;

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
    let p1 = field
        .keys()
        .filter(|pos| field.get(&pos) == Some(&'#') && pos.neighbors().iter().all(|(_, p)| field.get(&p) == Some(&'#')))
        .fold(0, |acc, pos| acc + pos.x * pos.y);
    println!("Part 1: {}", p1);
    // println!("{}", draw_ascii(&field, '.'));
}
