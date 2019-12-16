use std::io::{stdin, BufRead};
use std::iter::*;

#[rustfmt::skip]
fn read_input() -> Vec<i16> {
    stdin().lock().lines().next().unwrap().unwrap().chars().map(|c| c.to_string().parse().unwrap()).collect()
}

#[rustfmt::skip]
fn main() {
    let mut last_phase = read_input();
    //let mut last_phase: Vec<i16> = "80871224585914546619083218645595".chars().map(|c| c.to_string().parse().unwrap()).collect();
    for _ in 0..100 {
        last_phase = (1..=last_phase.len()).map(|i| {
            let mut pattern = [0i16, 1, 0, -1].iter().flat_map(|x| repeat(x).take(i)).cycle().skip(1);
            last_phase.iter().map(|x| x*pattern.next().unwrap()).sum::<i16>().abs() % 10
        }).collect();
    }
    println!("Part 1: {}", last_phase.iter().take(8).map(|n| n.to_string()).collect::<String>());
}
