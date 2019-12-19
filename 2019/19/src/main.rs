use intcode::*;

fn tractor_at_position(input: &[i64], x: i64, y: i64) -> bool {
    IntComputer::new(input.to_vec(), 0, vec![x, y]).get_all_outputs()[0] == 1
}

const ZONE_SIZE: i64 = 99;

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

    for y in 0..1000 {
        for x in 0..1000 {
            if tractor_at_position(&input, x, y)
                && tractor_at_position(&input, x + ZONE_SIZE, y)
                && tractor_at_position(&input, x, y + ZONE_SIZE)
            {
                println!("Part 2: {}", y * 10_000 + x);
                return;
            }
        }
        if y % 100 == 0 {
            println!("Outer loop {}", y);
        }
    }
}
