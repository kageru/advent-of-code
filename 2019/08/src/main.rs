mod counter;
use counter::Counter;
use std::io::BufRead;

const WIDTH: usize = 25;
const HEIGHT: usize = 6;

fn main() {
    let input: Vec<_> = std::io::stdin().lock().lines().next().unwrap().unwrap().chars().collect();
    let mut counters: Vec<_> = vec![];
    let mut chunks = input.chunks(WIDTH).peekable();
    while chunks.peek().is_some() {
        let mut counter = Counter::new();
        for _ in 0..HEIGHT {
            counter.add(chunks.next().unwrap());
        }
        counters.push(counter);
    }
    
    let fewest_zeros = counters.into_iter().min_by_key(|c| c['0']).unwrap();
    println!("{:?}", fewest_zeros['1'] * fewest_zeros['2']);
}
