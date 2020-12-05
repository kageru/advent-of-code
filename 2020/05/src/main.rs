#![feature(test)]
extern crate test;

#[derive(Debug, PartialEq)]
struct Position {
    row: usize,
    col: usize,
}

fn get_position(pass: &str) -> Position {
    unimplemented!();
}

fn calculate_id(p: &Position) -> usize {
    unimplemented!();
}

fn main() {
    println!("Hello, world!");
}

fn read_input() -> String {
    std::fs::read_to_string("input").unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::black_box;

    const PASS_1: &str = "BFFFBBFRRR";
    const PASS_2: &str = "FFFBBBFRRR";
    const PASS_3: &str = "BBFFBBFRLL";

    const POS_1: Position = Position { row: 70, col: 7 };
    const POS_2: Position = Position { row: 14, col: 7 };
    const POS_3: Position = Position { row: 102, col: 4 };

    const SID_1: usize = 567;
    const SID_2: usize = 119;
    const SID_3: usize = 820;

    #[test]
    fn test_get_position() {
        assert_eq!(get_position(PASS_1), POS_1);
        assert_eq!(get_position(PASS_2), POS_2);
        assert_eq!(get_position(PASS_3), POS_3);
    }

    #[test]
    fn test_calculate_id() {
        assert_eq!(calculate_id(&POS_1), SID_1);
        assert_eq!(calculate_id(&POS_2), SID_2);
        assert_eq!(calculate_id(&POS_3), SID_3);
    }
}
