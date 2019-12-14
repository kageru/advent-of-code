#[macro_use]
extern crate lazy_static;

use std::collections::HashMap;
use std::default::Default;
use std::io::{stdin, BufRead};
use std::sync::Mutex;

#[derive(Debug, Clone, Copy, Default)]
struct Element<'a> {
    quantity: usize,
    element: &'a str,
}

#[derive(Debug, Clone, Default)]
struct Reaction<'a> {
    inputs: Vec<Element<'a>>,
    output: Element<'a>,
}

impl<'a> From<&'a str> for Element<'a> {
    fn from(other: &'a str) -> Self {
        let mut parts = other.split(' ');
        Element {
            quantity: parts.next().unwrap().parse().unwrap(),
            element: parts.next().unwrap(),
        }
    }
}

impl<'a> From<&'a str> for Reaction<'a> {
    fn from(other: &'a str) -> Self {
        let in_out: Vec<_> = other.split(" => ").collect();
        let inputs = in_out[0].split(", ").map(|p| p.into()).collect();
        let output = in_out[1].into();
        Reaction { inputs, output }
    }
}

lazy_static! {
    static ref LINES: Vec<String> = stdin().lock().lines().map(|l| l.unwrap()).collect();
    static ref REACTIONS: HashMap<&'static str, Reaction<'static>> = LINES
        .iter()
        .map::<Reaction, _>(|l| l.as_str().into())
        .map(|r| (r.output.element.clone(), r))
        .collect();
    static ref STORAGE: Mutex<HashMap<&'static str, usize>> = Mutex::new(HashMap::new());
}

const ORE: &'static str = "ORE";
const FUEL: &'static str = "FUEL";

fn main() {
    println!("FUEL: {:?}", REACTIONS.get("FUEL"));
    println!("{}", count_ingredients(FUEL, 1));
}

fn count_ingredients(element: &'static str, desired_quantity: usize) -> usize {
    if element == ORE {
        return desired_quantity;
    }
    let mut ores = 0;
    let reaction = REACTIONS.get(&element).unwrap();
    let output_quantity = reaction.output.quantity;
    //dbg!(&reaction);
    let num_reactions = (desired_quantity as f32 / output_quantity as f32).ceil() as usize;
    //dbg!(&num_reactions);
    //println!("A: {}, B: {}", get_from_storage("A"), get_from_storage("B"));
    for input in &reaction.inputs {
        let in_storage = get_from_storage(input.element);
        let mut needed = input.quantity * num_reactions;
        if in_storage >= needed {
            put_into_storage(input.element, in_storage - needed);
        } else {
            needed -= in_storage;
            put_into_storage(input.element, 0);
            ores += count_ingredients(input.element, needed);
        }
    }
    //let num_reactions = (desired_quantity as f32 / output_quantity as f32).ceil() as usize;
    //dbg!(&num_reactions);
    let surplus = output_quantity * num_reactions - desired_quantity;
    put_into_storage(element, get_from_storage(element) + surplus);
    return ores;// * num_reactions;
}

#[inline]
fn get_from_storage(element: &str) -> usize {
    *STORAGE.lock().unwrap().get(element).unwrap_or(&0)
}

#[inline]
fn put_into_storage(element: &'static str, quantity: usize) {
    STORAGE.lock().unwrap().insert(element, quantity);
}
