mod counter;
use counter::Counter;
use std::io::BufRead;

const WIDTH: usize = 25;
const HEIGHT: usize = 6;

fn main() {
    let input: Vec<_> = std::io::stdin()
        .lock()
        .lines()
        .next()
        .unwrap()
        .unwrap()
        .chars()
        .collect();
    let mut counters: Vec<_> = vec![];
    let mut chunks = input.chunks(WIDTH).peekable();
    let mut img = vec!['2'; WIDTH * HEIGHT];
    let mut cur_pos = (0..WIDTH * HEIGHT).cycle();
    while chunks.peek().is_some() {
        let mut counter = Counter::new();
        for _ in 0..HEIGHT {
            let next_chunk = chunks.next().unwrap();
            for p in next_chunk {
                let pos = cur_pos.next().unwrap();
                if let Some(&'2') = img.get(pos) {
                    img[pos] = *p;
                }
            }
            counter.add(next_chunk);
        }
        counters.push(counter);
    }

    let fewest_zeros = counters.into_iter().min_by_key(|c| c['0']).unwrap();
    println!("Part 1: {}", fewest_zeros['1'] * fewest_zeros['2']);

    for line in img.chunks(WIDTH) {
        println!("{}", line.into_iter().collect::<String>().replace('0', " "));
    }
}
