use intcode::*;

pub fn main() {
    let input = read_input();
    println!("Part 1: {}", run_for_input(&input, &mut 0, vec![1]));
    println!("Part 2: {}", run_for_input(&input, &mut 0, vec![5]));
}
