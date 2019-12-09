use itertools::Itertools;
use std::convert::TryFrom;
use std::io::{self, BufRead};
use std::ops::Range;
use amplifier::*;
mod amplifier;

/**
 * Construct amplifiers and run all of them for a single given input tape
 * and a vector of amplifier configurations.
 *
 * Returns the return value of the first halted amplifier.
 */
pub fn run_for_input(input: &Vec<i64>, acc: &mut i64, amp_phases: Vec<i64>) -> i64 {
    let mut amplifiers: Vec<_> = amp_phases
        .into_iter()
        .map(|amp| Amplifier {
            tape: input.clone(),
            pos: 0,
            params: vec![amp],
        })
        .collect();
    for state in (0..amplifiers.len()).cycle() {
        let amplifier = amplifiers.get_mut(state).unwrap();
        amplifier.params.insert(0, *acc);
        match amplifier.run() {
            Err(output) => *acc = output,
            Ok(_) => break,
        }
    }
    *acc
}

/**
 * Convenience method for day 7.
 * Will try all combinations of parameters in the given range and return the maximum result.
 */
pub fn find_max(range: Range<i64>, input: &Vec<i64>) -> Option<i64> {
    usize::try_from(range.end - range.start)
        .ok()
        .and_then(|len| {
            range
                .permutations(len)
                .scan(0, |&mut mut acc, amps| {
                    Some(run_for_input(input, &mut acc, amps))
                })
                .max()
        })
}

#[rustfmt::skip]
pub fn read_input() -> Vec<i64> {
    io::stdin().lock().lines().next().unwrap().unwrap().split(',').map(|n| n.parse().unwrap()).collect()
}

#[cfg(test)]
mod tests;
