use std::collections::HashSet;
use std::io::{self, BufRead};
mod tests;

#[derive(Clone, Copy)]
enum ShuffleOperation {
    CutLeft(usize),
    CutRight(usize),
    DealWithIncrement(usize),
    Reverse,
}

impl<'a> From<&'a str> for ShuffleOperation {
    fn from(raw: &'a str) -> ShuffleOperation {
        let words: Vec<_> = raw.split(' ').collect();
        match words[0] {
            "cut" => {
                let offset: isize = words[1].parse().expect("Not a valid number for cutting");
                if offset < 0 {
                    ShuffleOperation::CutRight(offset.abs() as usize)
                } else {
                    ShuffleOperation::CutLeft(offset as usize)
                }
            }
            "deal" => match words[1] {
                // with increment
                "with" => ShuffleOperation::DealWithIncrement(words[3].parse().unwrap()),
                // into new stack
                "into" => ShuffleOperation::Reverse,
                _ => unreachable!("Unknown deal command"),
            },
            _ => unreachable!("Unknown shuffle command"),
        }
    }
}

fn apply(deck: &mut Vec<usize>, operation: ShuffleOperation) {
    match operation {
        ShuffleOperation::Reverse => deck.reverse(),
        ShuffleOperation::CutLeft(offset) => {
            let clone = deck.clone();
            let (left, right) = clone.split_at(offset as usize);
            *deck = right.to_vec();
            deck.append(&mut left.to_vec());
        }
        ShuffleOperation::CutRight(offset) => {
            let offset = deck.len() - offset;
            let cut: Vec<_> = deck.drain(0..offset).collect();
            cut.into_iter().for_each(|n| deck.push(n));
        }
        ShuffleOperation::DealWithIncrement(increment) => {
            let length = deck.len();
            let old_deck = deck.clone();
            for i in 0..length {
                deck[(i * increment) % length] = old_deck[i];
            }
        }
    };
}

fn get_previous_index(current: usize, operation: ShuffleOperation, deck_size: usize) -> usize {
    match operation {
        ShuffleOperation::Reverse => deck_size - current - 1,
        ShuffleOperation::CutLeft(offset) => (current + offset) % deck_size,
        ShuffleOperation::CutRight(offset) => (current + deck_size - offset) % deck_size,
        ShuffleOperation::DealWithIncrement(increment) => (0..)
            .filter_map(|x| {
                let old_position = x * deck_size + current;
                if old_position % increment == 0 {
                    Some(old_position / increment)
                } else {
                    None
                }
            })
            .next()
            .unwrap(),
    }
}

fn create_deck(len: usize) -> Vec<usize> {
    let mut deck = Vec::with_capacity(len);
    (0..len).for_each(|n| deck.push(n));
    deck
}

fn get_original_index(
    initial: usize,
    iterations: i128,
    deck_size: usize,
    operations: &[ShuffleOperation],
) -> i128 {
    let mut last = initial;
    for operation in operations.iter().rev() {
        last = get_previous_index(last, *operation, deck_size);
    }
    let initial = initial as i128;
    let deck_size = deck_size as i128;
    let drift = last as i128 - initial;
    dbg!(drift);
    let new_index = initial + drift * iterations;
    if new_index >= 0 {
        new_index % deck_size
    } else {
        (new_index + deck_size * ((new_index.abs() / deck_size) + 1)) % deck_size
    }
}

fn main() {
    let input: Vec<ShuffleOperation> = io::stdin()
        .lock()
        .lines()
        .map(|l| ShuffleOperation::from(l.unwrap().as_ref()))
        .collect();
    const DECK_SIZE: usize = 10007;
    let mut deck = create_deck(DECK_SIZE);
    for operation in &input {
        apply(&mut deck, *operation);
    }
    println!("Part 1: {}", deck.iter().position(|&x| x == 2019).unwrap());

    const P2_DECK_SIZE: usize = 119315717514047;
    const P2_ITERATIONS: i128 = 101_741_582_076_661;
    const P2_INDEX: usize = 2020;
    let mut last = P2_INDEX;
    println!("Part 2: {}", last);
}
