use std::io::{self, BufRead};

fn apply(deck: &mut Vec<usize>, operation: &str) {
    let words: Vec<_> = operation.split(' ').collect();
    match words[0] {
        "cut" => {
            let offset: isize = words[1].parse().expect("Not a valid number for cutting");
            if offset < 0 {
                let offset = deck.len() - (offset.abs()) as usize;
                let cut: Vec<_> = deck.drain(0..offset).collect();
                cut.into_iter().for_each(|n| deck.push(n));
            } else {
                let clone = deck.clone();
                let (left, right) = clone.split_at(offset as usize);
                *deck = right.to_vec();
                deck.append(&mut left.to_vec());
            }
        }
        // deal
        "deal" => match words[1] {
            // with increment
            "with" => {
                let length = deck.len();
                let increment: usize = words[3].parse().unwrap();
                let old_deck = deck.clone();
                for i in 0..length {
                    deck[(i * increment) % length] = old_deck[i];
                }
            }
            // into new stack
            "into" => deck.reverse(),
            _ => unreachable!("Unknown deal command"),
        },
        _ => unreachable!("Unknown shuffle command"),
    };
}

const DECK_SIZE: usize = 10007;

fn main() {
    let input: Vec<_> = io::stdin().lock().lines().map(|l| l.unwrap()).collect();
    let mut deck = Vec::with_capacity(DECK_SIZE);
    (0..DECK_SIZE).for_each(|n| deck.push(n));
    for line in input {
        apply(&mut deck, &line);
    }
    println!("{}", deck.iter().position(|&x| x == 2019).unwrap());
    assert_eq!(deck.capacity(), 10007);
}

mod tests {
    use super::*;

    #[test]
    fn deal_test() {
        let mut deck: Vec<usize> = (0..10).collect();
        let instructions = "deal with increment 7
deal into new stack
deal into new stack";
        for line in instructions.lines() {
            apply(&mut deck, line);
        }
        assert_eq!(deck, &[0, 3, 6, 9, 2, 5, 8, 1, 4, 7]);
    }

    #[test]
    fn cut_deal_test() {
        let mut deck: Vec<usize> = (0..10).collect();
        let instructions = "cut 6
deal with increment 7
deal into new stack";
        for line in instructions.lines() {
            apply(&mut deck, line);
        }
        assert_eq!(deck, &[3, 0, 7, 4, 1, 8, 5, 2, 9, 6]);
    }

    #[test]
    fn test_all() {
        let mut deck: Vec<usize> = (0..10).collect();
        let instructions = "deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal with increment 9
deal with increment 3
cut -1";
        for line in instructions.lines() {
            apply(&mut deck, line);
        }
        assert_eq!(deck, &[9, 2, 5, 8, 1, 4, 7, 0, 3, 6]);
    }
}
