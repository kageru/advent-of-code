use grid::*;
use rayon::prelude::*;
use std::char;
use std::collections::{HashMap, HashSet};
use std::io::{self, BufRead};
use std::ops::Mul;
use std::sync::Mutex;
#[macro_use]
extern crate lazy_static;

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
struct Door(char);

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
struct Key(char);

lazy_static! {
    static ref BEST: Mutex<usize> = Mutex::new(999999);
}

fn too_slow(i: usize) -> bool {
    let best = BEST.lock().unwrap();
    *best < i
}

fn replace(i: usize) {
    let mut best = BEST.lock().unwrap();
    if i < *best {
        println!("Best so far: {}", i);
        *best = i;
    }
}

fn visit_neighbors(
    map: &HashMap<Position2D, char>,
    position: Position2D,
    steps: usize,
    distances: &mut HashMap<Position2D, usize>,
    dependencies: &mut HashMap<Key, (HashSet<Key>, HashSet<Door>)>,
    door_dependencies: &mut HashMap<Door, HashSet<Door>>,
    mut doors: HashSet<Door>,
    mut keys: HashSet<Key>,
) {
    let c = map.get(&position).unwrap();
    if c.is_alphabetic() {
        if c.is_lowercase() {
            dependencies.insert(Key { 0: *c }, (keys.clone(), doors.clone()));
            keys.insert(Key { 0: *c });
        }
        if c.is_uppercase() {
            door_dependencies.insert(Door { 0: *c }, doors.clone());
            doors.insert(Door { 0: *c });
        }
    }
    let mut unvisited = Vec::new();
    // TODO: fix ownership and make this a single filter().inspect().for_each()
    for (_, p) in &position.neighbors() {
        match map.get(p) {
            Some('#') => (),
            _ => {
                if distances.get(p).is_none() {
                    unvisited.push(p.to_owned());
                }
            }
        }
        if map.get(p) != Some(&'#') && distances.get(p).is_none() {
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
            map,
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
    map: &HashMap<Position2D, char>,
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
        map,
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

fn find(map: &HashMap<Position2D, char>, c: char) -> Option<Position2D> {
    map.keys().find(|p| map.get(&p) == Some(&c)).copied()
}

fn clear_field(map: &mut HashMap<Position2D, char>, p: &Position2D) {
    map.insert(*p, '.');
}

fn remove(map: &mut HashMap<Position2D, char>, k: Key) {
    let key_pos = find(map, k.0).unwrap();
    clear_field(map, &key_pos);
    if let Some(door_pos) = find(map, k.0.to_uppercase().next().unwrap()) {
        clear_field(map, &door_pos);
    }
}

fn distance_to_door(map: &HashMap<Position2D, char>, k: &Key) -> usize {
    let (distances, _, _) = traverse(map, find(map, k.0).unwrap());
    let door_pos = find(map, k.0.to_uppercase().next().unwrap());
    *door_pos.map(|p| distances.get(&p).unwrap()).unwrap_or(&10)
}
fn distance_to_locked_door(map: &HashMap<Position2D, char>, k: &Key) -> usize {
    let (distances, _, doors) = traverse(map, find(map, k.0).unwrap());
    *doors
        .keys()
        .map(|d| find(map, d.0).unwrap())
        .map(|p| distances.get(&p).unwrap_or(&9999999999999))
        .min()
        .unwrap_or(&0)
}

fn next_key(
    map: &mut HashMap<Position2D, char>,
    pos: &Position2D,
    steps: usize,
) -> usize {
    if too_slow(steps) {
        // println!("Aborting after {} steps", steps);
        return 999999;
    }
    let (distances, dependencies, door_dependencies) = traverse(map, pos.to_owned());
    //dbg!(distances.get(&find('c').unwrap()));
    if dependencies.len() == 0 {
        // println!("End of path after {} steps", steps);
        replace(steps);
        return steps;
    }

    let reachable_doors: HashSet<_> = door_dependencies
        .iter()
        .filter_map(|(door, doors)| if doors.is_empty() { Some(door) } else { None })
        .collect();
    let mut useful_keys: Vec<_> = dependencies
        .iter()
        .filter(|(_, (_, d))| d.is_empty())
        .filter(|(k, _)| {
            reachable_doors.contains(&Door {
                0: k.0.to_uppercase().next().unwrap(),
            })
        })
        //.min_by_key(|(k, _)| distances.get(&find(k.0).unwrap()))
        //.unwrap()
        .map(|(k, _)| {
            (
                k,
                distances
                    .get(&find(map, k.0).expect("could not find key"))
                    .expect("No distance to key")
                    .mul(1),
            )
        })
        .collect();
    //.min_by_key(|k| )

    let dead_end_keys = find_keys_at_dead_ends(&dependencies);
    let mut next: Vec<_> = dead_end_keys
        .iter()
        .filter(|k| dependencies.get(k).expect("not in deps").1.is_empty())
        .map(|k| {
            (
                k,
                distances
                    .get(&find(map, k.0).expect("could not find key #2"))
                    .expect("No distance to key #2")
                    .mul(1),
            )
        })
        //.min_by_key(|k| distances.get(&find(k.0).unwrap()))
        //.chain(useful_keys)
        .collect();
    next.sort_by_key(|(k, n)| *n);//*3 + distance_to_door(map, k));
    useful_keys.sort_by_key(|(k, n)| *n);
    useful_keys.reverse();
    useful_keys.pop().map(|k| next.insert(0, k));
    useful_keys.pop().map(|k| next.push(k));
    useful_keys.pop().map(|k| next.push(k));
    useful_keys.pop().map(|k| next.push(k));
    useful_keys.pop().map(|k| next.push(k));
    let len = next.len();
    next.par_iter()
        .take((len.min(5) - 1).max(1))
        .map(|(k, _)| {
            let mut map2 = map.clone();
            let next_pos = find(&map2, k.0).expect("Could not find key #3");
            let steps_after_next = steps + distances.get(&next_pos).expect("No distance to key #3");
            // collect keys on the way (if any)
            for k in &dependencies.get(&k).expect("No dependency found").0 {
                remove(&mut map2, k.to_owned());
            }
            remove(&mut map2, k.to_owned().to_owned());
            next_key(&mut map2, &next_pos, steps_after_next)
        })
        .min()
        .unwrap()
}

fn main() {
    let map: HashMap<Position2D, char> = io::stdin()
        .lock()
        .lines()
        .enumerate()
        .flat_map(move |(y, l)| {
            l.unwrap()
                .to_owned()
                .chars()
                .enumerate()
                .map(move |(x, c)| ((x, y).into(), c))
                .collect::<Vec<_>>()
        })
        .collect();
    let pos = find(&map, '@').unwrap();
    let steps = 0;
    let (_, dependencies, _) = traverse(&map, pos.to_owned());
    let p1 = next_key(&mut map.clone(), &pos, 0);
    println!("Part 1: {}", p1);
    println!("Part 1: {}", BEST.lock().unwrap());
}
