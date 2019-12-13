use intcode::*;

fn main() {
    let part1 = IntComputer::new(read_input(), 0, vec![])
        .get_all_outputs()
        .into_iter()
        .skip(2)
        .step_by(3)
        .filter(|s| s == &2)
        .count();
    println!("Part 1: {}", part1);
}
