use std::io::{self, BufRead};

enum ShuffleOperation {
    CutLeft(usize),
    CutRight(usize),
    DealWithIncrement(usize),
    Reverse,
}

fn parse_operation(operation: &str) -> ShuffleOperation {
    let words: Vec<_> = operation.split(' ').collect();
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

fn apply(deck: &mut Vec<usize>, operation: &str) {
    match parse_operation(operation) {
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

fn get_previous_index(current: usize, operation: &str, deck_size: usize) -> usize {
    match parse_operation(operation) {
        ShuffleOperation::Reverse => deck_size - current - 1,
        ShuffleOperation::CutLeft(offset) => {
            if current >= deck_size - offset {
                current - (deck_size - offset)
            } else {
                current + offset
            }
        }
        ShuffleOperation::CutRight(offset) => {
            if current < offset {
                deck_size - offset + current
            } else {
                current - offset
            }
        }
        ShuffleOperation::DealWithIncrement(increment) => (0..)
            .filter_map(|x| {
                if (x * deck_size + current) % increment == 0 {
                    Some((x * deck_size + current) / increment)
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
    iterations: u128,
    deck_size: usize,
    operations: &[String],
) -> u128 {
    let mut last = initial;
    for operation in operations.iter().rev() {
        last = get_previous_index(last, operation, deck_size);
    }
    let drift = (last - initial) as u128;
    (P2_INDEX as u128 + drift * iterations) % deck_size as u128
}

const DECK_SIZE: usize = 10007;
const P2_DECK_SIZE: usize = 119315717514047;
const P2_ITERATIONS: u128 = 101741582076661;
const P2_INDEX: usize = 2020;

fn main() {
    let input: Vec<_> = io::stdin().lock().lines().map(|l| l.unwrap()).collect();
    let mut deck = create_deck(DECK_SIZE);
    for operation in &input {
        apply(&mut deck, operation);
    }
    println!("Part 1: {}", deck.iter().position(|&x| x == 2019).unwrap());
    let p2 = get_original_index(P2_INDEX, P2_ITERATIONS, P2_DECK_SIZE, &input);
    println!("Part 2: {}", p2);
}

mod tests {
    use super::*;

    const EXAMPLE_1: &str = "deal with increment 7
deal into new stack
deal into new stack";
    const EXAMPLE_2: &str = "cut 6
deal with increment 7
deal into new stack";
    const EXAMPLE_3: &str = "deal with increment 7
deal with increment 9
cut -2";
    const EXAMPLE_4: &str = "deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal with increment 9
deal with increment 3
cut -1";
    /// Example 4 but with an even number of reversals
    const EXAMPLE_5: &str = "deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal into new stack
deal with increment 9
deal with increment 3
cut -1";

    #[test]
    fn deal_test() {
        let mut deck = create_deck(10);
        for line in EXAMPLE_1.lines() {
            apply(&mut deck, line);
        }
        assert_eq!(deck, &[0, 3, 6, 9, 2, 5, 8, 1, 4, 7]);
    }

    #[test]
    fn cut_deal_test() {
        let mut deck = create_deck(10);
        for line in EXAMPLE_2.lines() {
            apply(&mut deck, line);
        }
        assert_eq!(deck, &[3, 0, 7, 4, 1, 8, 5, 2, 9, 6]);
    }

    #[test]
    fn test_all() {
        let mut deck = create_deck(10);
        for line in EXAMPLE_4.lines() {
            apply(&mut deck, line);
        }
        assert_eq!(deck, &[9, 2, 5, 8, 1, 4, 7, 0, 3, 6]);
    }

    // tests for part 2
    #[test]
    fn reverse_stack_deal_test() {
        assert_eq!(
            reverse_step("deal into new stack", 10),
            [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
        );
    }

    #[test]
    fn reverse_cut_test() {
        assert_eq!(reverse_step("cut 3", 10), [3, 4, 5, 6, 7, 8, 9, 0, 1, 2]);
        assert_eq!(reverse_step("cut -4", 10), [6, 7, 8, 9, 0, 1, 2, 3, 4, 5]);
    }

    #[test]
    fn reverse_increment_test() {
        assert_eq!(
            reverse_step("deal with increment 3", 10),
            [0, 7, 4, 1, 8, 5, 2, 9, 6, 3]
        );
    }

    #[test]
    fn full_reverse_test1() {
        assert_eq!(index_before_instructions(0, EXAMPLE_1), 0);
        assert_eq!(index_before_instructions(2, EXAMPLE_1), 6);
    }

    #[test]
    fn full_reverse_test2() {
        assert_eq!(index_before_instructions(0, EXAMPLE_2), 3);
        assert_eq!(index_before_instructions(6, EXAMPLE_2), 5);
    }

    #[test]
    fn full_reverse_test3() {
        assert_eq!(index_before_instructions(0, EXAMPLE_3), 6);
        assert_eq!(index_before_instructions(9, EXAMPLE_3), 9);
    }

    #[test]
    fn test_twice() {
        let mut deck = create_deck(10);
        for line in EXAMPLE_5.lines() {
            apply(&mut deck, line);
        }
        for line in EXAMPLE_5.lines() {
            apply(&mut deck, line);
        }
        assert_eq!(deck, &[2, 1, 0, 9, 8, 7, 6, 5, 4, 3]);
    }

    #[test]
    fn interpolate_drift_test() {
        //get_original_index(0, 1, 10, EXAMPLE_4);
    }

    #[test]
    fn full_reverse_test4() {
        let instructions = EXAMPLE_4;
        assert_eq!(index_before_instructions(1, instructions), 2);
    }

    fn index_before_instructions(mut last: usize, instructions: &str) -> usize {
        for line in instructions.lines().rev() {
            last = get_previous_index(last, line, 10);
        }
        last
    }

    fn reverse_step(instruction: &str, len: usize) -> Vec<usize> {
        (0..len)
            .into_iter()
            .map(|n| get_previous_index(n, instruction, 10))
            .collect()
    }
}
