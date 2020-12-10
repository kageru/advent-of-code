use std::env;

pub fn read_file(day: usize) -> String {
    std::fs::read_to_string(
        env::args()
            .nth(1)
            .filter(|n| n != "--bench")
            .unwrap_or_else(|| format!("inputs/day{:0>2}", day)),
    )
    .unwrap()
}
