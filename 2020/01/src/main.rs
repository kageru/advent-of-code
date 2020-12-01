use itertools::iproduct;
use std::io::BufRead;

fn main() {
    let input: Vec<usize> = std::io::stdin()
        .lock()
        .lines()
        .filter_map(|l| l.unwrap().parse().ok())
        .collect();
    let p1 = iproduct!(input.iter(), input.iter())
        .filter(|(&a, &b)| a + b == 2020)
        .map(|(a, b)| a * b)
        .next()
        .unwrap();
    println!("Part 1: {}", p1);
    /*
    smol-brain, n³ solution:
    let p2 = iproduct!(input.iter(), input.iter(), input.iter())
        .filter(|(&a, &b, &c)| a + b + c == 2020)
        .map(|(a, b, c)| a * b * c)
        .next()
        .unwrap();
    println!("Part 2: {}", p2);
    */
    // Ascended n² solution (thanks to Lypheo)
    let mut p2_table = vec![None; 2020];
    for (&a, &b) in iproduct!(input.iter(), input.iter()) {
        if a + b < 2020 {
            p2_table[a + b] = Some((a, b))
        }
    }
    let (a, b) = input
        .iter()
        .filter_map(|x| p2_table[2020 - x])
        .next()
        .unwrap();
    println!("Part 2: {}", a * b * (2020 - a - b));
}
