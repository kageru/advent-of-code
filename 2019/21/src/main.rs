use intcode::*;

fn main() {
    let program = "NOT A J
NOT B T
OR T J
NOT C T
OR T J
AND D J
WALK
"
    .chars()
    .map(|c| c as i64)
    .rev()
    .collect();
    let part1 = IntComputer::new(read_input(), 0, program)
        .get_all_outputs()
        .pop()
        .unwrap();
    println!("{}", part1);
}
