use grid::*;
use std::char;
use std::collections::{HashMap, HashSet};
use std::io::{self, BufRead};
use std::sync::Mutex;
use std::ops::Mul;
#[macro_use]
extern crate lazy_static;

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
struct Door(char);

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
struct Key(char);

lazy_static! {
    static ref MAP: Mutex<HashMap<Position2D, char>> = Mutex::new(
        io::stdin()
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
            .collect()
    );
}

fn get(p: &Position2D) -> Option<char> {
    MAP.lock().unwrap().get(p).copied()
}

fn visit_neighbors(
    position: Position2D,
    steps: usize,
    distances: &mut HashMap<Position2D, usize>,
    dependencies: &mut HashMap<Key, (HashSet<Key>, HashSet<Door>)>,
    door_dependencies: &mut HashMap<Door, HashSet<Door>>,
    mut doors: HashSet<Door>,
    mut keys: HashSet<Key>,
) {
    let c = get(&position).unwrap();
    if c.is_alphabetic() {
        if c.is_lowercase() {
            dependencies.insert(Key { 0: c }, (keys.clone(), doors.clone()));
            keys.insert(Key { 0: c });
        }
        if c.is_uppercase() {
            door_dependencies.insert(Door { 0: c }, doors.clone());
            doors.insert(Door { 0: c });
        }
    }
    let mut unvisited = Vec::new();
    // TODO: fix ownership and make this a single filter().inspect().for_each()
    for (_, p) in &position.neighbors() {
        match get(p) {
            Some('#') => (),
            _ => {
                if distances.get(p).is_none() {
                    unvisited.push(p.to_owned());
                }
            }
        }
        if get(p) != Some('#') && distances.get(p).is_none() {
            unvisited.push(p.to_owned());
        }
    }
    /*
    unvisited.into_iter()
        .inspect(|p| { distances.insert(p.to_owned(), steps+1);})
        .for_each(|p| visit_neighbors(p.to_owned(), steps+1, distances));
    */
    for p in &unvisited {
        distances.insert(p.to_owned(), steps + 1);
    }
    for p in &unvisited {
        visit_neighbors(
            p.to_owned(),
            steps + 1,
            distances,
            dependencies,
            door_dependencies,
            doors.clone(),
            keys.clone(),
        );
    }
}

fn traverse(
    start: Position2D,
) -> (
    HashMap<Position2D, usize>,
    HashMap<Key, (HashSet<Key>, HashSet<Door>)>,
    HashMap<Door, HashSet<Door>>,
) {
    let mut distances = HashMap::new();
    let mut dependencies = HashMap::new();
    let mut door_dependencies = HashMap::new();
    distances.insert(start, 0usize);
    visit_neighbors(
        start,
        0,
        &mut distances,
        &mut dependencies,
        &mut door_dependencies,
        HashSet::new(),
        HashSet::new(),
    );
    (distances, dependencies, door_dependencies)
}

fn find_keys_at_dead_ends(deps: &HashMap<Key, (HashSet<Key>, HashSet<Door>)>) -> HashSet<Key> {
    let keydeps: HashSet<_> = deps.iter().flat_map(|(_, (keys, _))| keys).collect();
    deps.keys()
        .collect::<HashSet<_>>()
        .difference(&keydeps)
        .into_iter()
        .map(|k| k.to_owned().to_owned())
        .collect()
}

fn find(c: char) -> Option<Position2D> {
    let map = MAP.lock().unwrap();
    map.keys().find(|p| map.get(&p) == Some(&c)).copied()
}

fn clear_field(p: &Position2D) {
    MAP.lock().unwrap().insert(*p, '.');
}

fn remove(k: Key) {
    let key_pos = find(k.0).unwrap();
    clear_field(&key_pos);
    if let Some(door_pos) = find(k.0.to_uppercase().next().unwrap()) {
        clear_field(&door_pos);
    }
}

fn distance_to_door(k: &Key) -> usize {
    let (distances, _, _) = traverse(find(k.0).unwrap());
    let door_pos = find(k.0.to_uppercase().next().unwrap());
    *door_pos.map(|p| distances.get(&p).unwrap()).unwrap_or(&10)
}
fn distance_to_locked_door(k: &Key) -> usize {
    let (distances, _, doors) = traverse(find(k.0).unwrap());
    *doors.keys()
        .map(|d| find(d.0).unwrap())
        .map(|p| distances.get(&p).unwrap_or(&9999999999999))
        .min()
        .unwrap_or(&0)
}

fn main() {
    let mut pos = find('@').unwrap();
    let mut steps = 0;
    let (_, dependencies, _) = traverse(pos.to_owned());
    let mut i = 0;
    while i < dependencies.len() {
        println!("{}", i);
        let (distances, dependencies, door_dependencies) = traverse(pos.to_owned());
        //dbg!(distances.get(&find('c').unwrap()));

        let reachable_doors: HashSet<_> = door_dependencies
            .iter()
            .filter_map(|(door, doors)| if doors.is_empty() { Some(door) } else { None })
            .collect();
        let useful_keys = dependencies
            .iter()
            .filter(|(_, (_, d))| d.is_empty())
            .filter(|(k, _)| {
                reachable_doors.contains(&Door {
                    0: k.0.to_uppercase().next().unwrap(),
                })
            })
            //.min_by_key(|(k, _)| distances.get(&find(k.0).unwrap()))
            //.unwrap()
            .map(|(k, _)| (k, distances.get(&find(k.0).unwrap()).unwrap().mul(2)));
            //.min_by_key(|k| )

        let next: Key = find_keys_at_dead_ends(&dependencies)
            .iter()
            .filter(|k| dependencies.get(k).unwrap().1.is_empty())
            .map(|k| (k, distances.get(&find(k.0).unwrap()).unwrap().mul(1)))
            //.min_by_key(|k| distances.get(&find(k.0).unwrap()))
            .chain(useful_keys)
            .min_by_key(|(k, n)| *n)// + distance_to_locked_door(&k))// * 100 + k.0 as usize)
            .expect("no candidate found")
            .0
            .to_owned();
        dbg!(&next);
        let next_pos = find(next.0).unwrap();
        steps += distances.get(&next_pos).unwrap();
        pos = next_pos;
        for k in &dependencies.get(&next).unwrap().0 {
            dbg!(&k);
            remove(k.to_owned());
            i += 1;
        }
        remove(next);
        i += 1;
        dbg!(&i);
        dbg!(&steps);
    }
    println!("{}", steps);
}
