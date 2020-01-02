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
    for line in EXAMPLE_1.lines().map(|l| l.into()) {
        apply(&mut deck, line);
    }
    assert_eq!(deck, &[0, 3, 6, 9, 2, 5, 8, 1, 4, 7]);
}

#[test]
fn cut_deal_test() {
    let mut deck = create_deck(10);
    for line in EXAMPLE_2.lines().map(|l| l.into()) {
        apply(&mut deck, line);
    }
    assert_eq!(deck, &[3, 0, 7, 4, 1, 8, 5, 2, 9, 6]);
}

#[test]
fn test_all() {
    let mut deck = create_deck(10);
    for line in EXAMPLE_4.lines().map(|l| l.into()) {
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
    for line in EXAMPLE_5.lines().map(|l| l.into()) {
        apply(&mut deck, line);
    }
    for line in EXAMPLE_5.lines().map(|l| l.into()) {
        apply(&mut deck, line);
    }
    assert_eq!(deck, &[2, 1, 0, 9, 8, 7, 6, 5, 4, 3]);
}

#[test]
fn get_original_index_test() {
    let instructions = EXAMPLE_1;
    let mut deck = create_deck(10);
    for line in instructions.lines().map(|l| l.into()) {
        apply(&mut deck, line);
    }
    assert_eq!(deck, &[0, 3, 6, 9, 2, 5, 8, 1, 4, 7]);
    assert_eq!(
        deck,
        (0..10)
            .map(|n| index_before_instructions(n, instructions))
            .collect::<Vec<_>>()
    );
    assert_eq!(
        deck,
        (0..10)
            .map(|n| get_original_index(
                n,
                1,
                10,
                &instructions
                    .lines()
                    .map(|l| l.into())
                    .collect::<Vec<_>>()
            ) as usize)
            .collect::<Vec<_>>()
    );

    assert_eq!(
        (0..10)
            .map(|n| index_before_instructions(n, EXAMPLE_4))
            .collect::<Vec<_>>(),
        (0..10)
            .map(|n| get_original_index(
                n,
                1,
                10,
                &EXAMPLE_4.lines().map(|l| l.into()).collect::<Vec<_>>()
            ) as usize)
            .collect::<Vec<_>>()
    );
}

#[test]
fn interpolate_drift_test() {
    const INSTRUCTIONS: &str = EXAMPLE_5;
    const ITERATIONS: usize = 4;
    let mut deck = create_deck(10);
    for _ in 0..ITERATIONS {
        for line in INSTRUCTIONS.lines().map(|l| l.into()) {
            apply(&mut deck, line);
        }
    }
    assert_eq!(
        deck,
        (0..10)
            .map(|n| get_original_index(
                n,
                ITERATIONS as i128,
                10,
                &INSTRUCTIONS
                    .lines()
                    .map(|l| l.into())
                    .collect::<Vec<_>>()
            ) as usize)
            .collect::<Vec<_>>()
    );
}

#[test]
fn full_reverse_test4() {
    assert_eq!(index_before_instructions(1, EXAMPLE_4), 2);
}

fn index_before_instructions(mut last: usize, instructions: &str) -> usize {
    for line in instructions.lines().rev().map(|l| l.into()) {
        last = get_previous_index(last, line, 10);
    }
    last
}

fn reverse_step(instruction: &str, len: usize) -> Vec<usize> {
    (0..len)
        .map(|n| get_previous_index(n, instruction.into(), 10))
        .collect()
}
