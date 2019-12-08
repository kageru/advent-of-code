mod counter;
use counter::Counter;
use itertools::Itertools;
use std::io::BufRead;

const WIDTH: usize = 25;
const HEIGHT: usize = 6;

fn main() {
    let input = std::io::stdin().lock().lines().next().unwrap().unwrap();
    let chunked = input.chars().chunks(HEIGHT * WIDTH);
    let layers = chunked.into_iter();
    let mut img = vec!['2'; WIDTH * HEIGHT];
    let counters = layers.map(|layer| {
        Counter::of(layer.enumerate().map(|(i, p)| {
            if let Some(&'2') = img.get(i) {
                img[i] = p
            }
            p
        }))
    });

    let fewest_zeros = counters.min_by_key(|c| c['0']).unwrap();
    println!("Part 1: {}", fewest_zeros['1'] * fewest_zeros['2']);

    for line in img.chunks(WIDTH) {
        println!("{}", line.iter().collect::<String>().replace('0', " "));
    }
}
