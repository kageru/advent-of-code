use grid::*;
use std::char;
use std::collections::{HashMap,HashSet};
use std::io::{self, BufRead};
#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref MAP: HashMap<Position2D, char> = io::stdin()
        .lock()
        .lines()
        .enumerate()
        .flat_map(move |(y, l)| l
            .unwrap()
            .to_owned()
            .chars()
            .enumerate()
            .map(move |(x, c)| ((x, y).into(), c))
            .collect::<Vec<_>>())
        .collect();
}

fn visit_neighbors(position: Position2D, steps: usize, distances: &mut HashMap<Position2D, usize>, dependencies: &mut HashMap<char, HashSet<char>>, mut doors: HashSet<char>) {
    let mut unvisited = Vec::new();
    // TODO: fix ownership and make this a single filter().inspect().for_each()
    for (_, p) in &position.neighbors() {
        match MAP.get(p) {
            Some(&'#') => (),
            Some(n) if n.is_alphanumeric() => {
                if n.is_lowercase() {
                    dependencies.insert(*n, doors.clone());
                }
                if n.is_uppercase() {
                    doors.insert(*n);
                }
                if distances.get(p).is_none() {
                    unvisited.push(p.to_owned());
                }
            },
            _ => {
                if distances.get(p).is_none() {
                    unvisited.push(p.to_owned());
                }
            },
        }
        if MAP.get(p) != Some(&'#') && distances.get(p).is_none() {
            unvisited.push(p.to_owned());
        }
    }
    /*
    unvisited.into_iter()
        .inspect(|p| { distances.insert(p.to_owned(), steps+1);})
        .for_each(|p| visit_neighbors(p.to_owned(), steps+1, distances));
    */
    for p in &unvisited {
        distances.insert(p.to_owned(), steps+1);
    }
    for p in &unvisited {
        visit_neighbors(p.to_owned(), steps+1, distances, dependencies, doors.clone());
    }
}

fn traverse(start: Position2D) -> (HashMap<Position2D, usize>, HashMap<char, HashSet<char>>) {
    let mut distances = HashMap::new();
    let mut dependencies = HashMap::new();
    distances.insert(start, 0usize);
    visit_neighbors(start, 0, &mut distances, &mut dependencies, HashSet::new());
    (distances, dependencies)
}

fn main() {
    //println!("{:?}", MAP.keys());
    println!("{}", draw_ascii(&MAP, ' '));
    let start = MAP.keys().find(|p| MAP.get(&p) == Some(&'@')).unwrap();
    let v = MAP.keys().find(|p| MAP.get(&p) == Some(&'v')).unwrap();
    println!("Start: {:?}", start);
    let (distances, dependencies) = traverse(start.to_owned());
    println!("{:?}", distances);
    println!("{:?}", distances.get(&v));
    println!("{:?}", distances.len());
    println!("{:?}", dependencies.get(&'m'));
    println!("{}", dependencies.len());
}
