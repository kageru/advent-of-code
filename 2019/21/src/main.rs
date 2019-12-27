use intcode::*;
use itertools::Itertools;

// If any of the next 3 are a gap, jump
const PART1_PROGRAM: &str = "NOT A J
NOT B T
OR T J
NOT C T
OR T J
// unless 4 is also a gap (otherwise we’d jump right into the hole)
AND D J
WALK
";

const PART2_PROGRAM: &str = "NOT T T
// if any of [A, B, C] are false, T will be false as well
AND A T
AND B T
AND C T
// jump if that is the case
NOT T J
// don’t jump if D (where we would land) is false
AND D J
// don’t jump if H is false (because that would prevent us from jumping again after we land)
OR H T
// unless E is true (which gives us the option to walk another step after landing)
OR E T
AND T J
RUN
";

/// Remove comments and convert the source code to ASCII for the springbot.
fn compile(program: &str) -> Vec<i64> {
    (program.lines().filter(|l| !l.starts_with("//")).join("\n") + "\n")
        .chars()
        .map(|c| c as i64)
        .rev()
        .collect()
}

fn run(input: &Vec<i64>, program: &str) -> i64 {
    IntComputer::new(input.clone(), 0, compile(program))
        .get_all_outputs()
        .pop()
        .unwrap()
}

fn main() {
    let input = read_input();
    println!("Part 1: {}", run(&input, PART1_PROGRAM));
    println!("Part 2: {}", run(&input, PART2_PROGRAM));
}
