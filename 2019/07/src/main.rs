use intcode::*;

pub fn main() {
    let input = read_input();
    println!("Part 1: {}", find_max(0..5, &input).unwrap());
    println!("Part 2: {}", find_max(5..10, &input).unwrap());
}
