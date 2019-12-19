use intcode::*;
use grid::*;
use std::collections::HashMap;

fn tractor_at_position(input: &Vec<i64>, x: i64, y: i64) -> bool {
    IntComputer::new(input.clone(), 0, vec![x, y]).get_all_outputs()[0] == 1
}

const ZONE_SIZE: i64 = 100;

fn main() {
    let input = read_input();
    let mut s = 0;
    for x in 0..50 {
        for y in 0..50 {
            if tractor_at_position(&input, x, y) {
                s += 1;
            }
        }
    }
    println!("Part 1: {}", s);

    let mut beam = HashMap::new();
    for x in 0..900 {
        for y in 0..1100 {
            let here = tractor_at_position(&input, x, y);
            beam.insert(Position2D {x, y}, if here { '#'}  else { '.' });
            if here 
            && tractor_at_position(&input, x+ZONE_SIZE, y)
            && tractor_at_position(&input, x, y+ZONE_SIZE)
            && tractor_at_position(&input, x+ZONE_SIZE, y+ZONE_SIZE) {
                beam.insert(Position2D {x, y}, 'O');
                println!("Part 2: {}, {}, {}", x, y, x*10_000 + y);
            }
        }
        if x%100 == 0 {
            println!("Outer loop {}", x);
        }
    }
    println!("{}", draw_ascii(&beam, '.'));
}
