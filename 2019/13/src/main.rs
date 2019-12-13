use grid::*;
use intcode::*;
use std::collections::HashMap;
mod tile;
use std::cmp::Ordering;
use tile::Tile;

fn int(ord: Ordering) -> i64 {
    match ord {
        Ordering::Less => -1,
        Ordering::Equal => 0,
        Ordering::Greater => 1,
    }
}

fn main() {
    let mut input = read_input();
    let part1 = IntComputer::new(input.clone(), 0, vec![])
        .get_all_outputs()
        .into_iter()
        .skip(2)
        .step_by(3)
        .filter(|s| s == &2)
        .count();
    println!("Part 1: {}", part1);

    input[0] = 2;
    let (mut paddle_pos, mut ball_pos) = (0, 0);
    let mut ic = IntComputer::new(input, 0, vec![]);
    let mut outputs = Vec::with_capacity(3);
    let mut field: HashMap<Position2D, Tile> = HashMap::new();
    let mut score = 0;
    loop {
        match ic.step() {
            IntComputerResult::Output(o) => outputs.push(o),
            IntComputerResult::Halt => break,
            IntComputerResult::Continue => (),
        };
        if outputs.len() == 3 {
            let pos: Position2D = (outputs[0], outputs[1]).into();
            if pos.x == -1 {
                score = outputs[2];
                outputs.clear();
                continue;
            }
            let tile = outputs[2].into();
            match tile {
                Tile::Ball => ball_pos = pos.x,
                Tile::Paddle => paddle_pos = pos.x,
                _ => (),
            };
            field.insert(pos, tile);
            outputs.clear();
            ic.params = vec![int(ball_pos.cmp(&paddle_pos))];

            //println!("{}", draw_ascii(&field, Tile::Empty));
        }
    }
    println!("Part 2: {}", score);
}
