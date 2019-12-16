use std::io::{stdin, BufRead};
use std::iter::*;

#[rustfmt::skip]
fn read_input() -> Vec<i32> {
    stdin().lock().lines().next().unwrap().unwrap().chars().map(|c| c.to_string().parse().unwrap()).collect()
}

#[rustfmt::skip]
fn part1(mut last_phase: Vec<i32>) -> String {
    for _ in 0..100 {
        last_phase = (1..=last_phase.len()).map(|i| {
            let mut pattern = [0i32, 1, 0, -1].iter().flat_map(|x| repeat(x).take(i)).cycle().skip(1);
            last_phase.iter().map(|x| x*pattern.next().unwrap()).sum::<i32>().abs() % 10
        }).collect();
    }
    last_phase.iter().take(8).map(|n| n.to_string()).collect::<String>()
}

/**
 * The outputs after each phase are related in a way that I can’t really explain.
 * They are essentially a summed-area table, but built starting with the last element.
 * However, this only works for the second half of the output. The rest seem to be random?
 * 
 * The examples show this quite clearly:
 * Input signal:  12345678 (let’s call this `input`)
 * After 1 phase: 48226158 (`output`)
 * We can build the state after 1 phase right to left.
 * ```
 * output[7] = input[7..8].iter().sum() % 10; // 8
 * output[6] = input[6..8].iter().sum() % 10; // 15 % 10 == 5
 * output[5] = input[5..8].iter().sum() % 10; // 21 % 10 == 1
 * output[4] = input[4..8].iter().sum() % 10; // 26 % 10 == 6
 * ```
 * Which is exactly the output sequence.
 * This pattern only holds true for the second half of the array,
 * but the offset always seems to be high enough for that.
 *
 * Because all input elements only affect outputs with lower indices,
 * we can also drop all elements before the output starts.
 */
#[rustfmt::skip]
fn part2(input: Vec<i32>) -> String {
    let offset: usize = input.iter().take(7).map(|n| n.to_string()).collect::<String>().parse().unwrap();
    let mut p2 = input.repeat(10_000).split_off(offset);
    p2.reverse();
    for _ in 0..100 {
        p2 = p2.iter().scan(0, |acc, n| {
            *acc += n;
            Some(*acc%10)
        }).collect();
    }
    p2.iter().rev().take(8).map(|n| n.to_string()).collect::<String>()
}

fn main() {
    let input = read_input();
    println!("Part 1: {}", part1(input.clone()));
    println!("Part 2: {}", part2(input));
}
