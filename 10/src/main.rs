#[macro_use] extern crate lazy_static;
extern crate rayon;
extern crate regex;

use regex::Regex;
use rayon::prelude::*;

struct Star {
    x: i32,
    y: i32,
    x_vel: i32,
    y_vel: i32,
}

impl Star {
    pub fn new(x: i32, y: i32, x_vel: i32, y_vel: i32) -> Self {
        Star {
            x, y, x_vel, y_vel
        }
    }
}

struct Bounds {
    min_x: i32,
    max_x: i32,
    min_y: i32,
    max_y: i32,
}

impl Bounds {
    pub fn from_stars(stars: &Vec<Star>) -> Self {
        Bounds {
            min_x: stars.par_iter().min_by_key(|a| a.x).unwrap().x,
            max_x: stars.par_iter().max_by_key(|a| a.x).unwrap().x,
            min_y: stars.par_iter().min_by_key(|a| a.y).unwrap().y, 
            max_y: stars.par_iter().max_by_key(|a| a.y).unwrap().y 
        }
    }
}

lazy_static! {
    static ref STAR_REGEX: Regex = Regex::new(r"position=<\s?(-?\d+),\s{1,2}(-?\d+)>\svelocity=<\s?(-?\d+),\s{1,2}(-?\d+)>").unwrap();
}

fn parse_star(input: &str) -> Star {
    let captures = STAR_REGEX.captures(input).expect("unexpected input line");
    let (x, y, x_vel, y_vel): (i32, i32, i32, i32);
    x = captures.get(1).unwrap().as_str().parse::<i32>().unwrap();
    y = captures.get(2).unwrap().as_str().parse::<i32>().unwrap();
    x_vel = captures.get(3).unwrap().as_str().parse::<i32>().unwrap();
    y_vel = captures.get(4).unwrap().as_str().parse::<i32>().unwrap();
    return Star::new(x, y, x_vel, y_vel);
}

fn move_star(star: &Star) -> Star {
    return Star::new(star.x + star.x_vel, star.y + star.y_vel, star.x_vel, star.y_vel);
}

fn print_stars(stars: &Vec<Star>) {
    let bounds = Bounds::from_stars(&stars);
    let width: i32 = bounds.max_x - bounds.min_x;
    let height: i32 = bounds.max_y - bounds.min_y;
    let line = vec!('.'; (width + 1) as usize);
    let mut field = vec!(line.clone(); (height + 1) as usize);
    for star in stars {
        field[(star.y - bounds.min_y) as usize][(star.x - bounds.min_x) as usize] = '#';
    }
    for l in field {
        let s: String = l.into_iter().collect();
        println!("{}", s);
    }
}

fn main() {
    let lines: Vec<&str> = include_str!("../input").lines().collect();

    let mut stars: Vec<Star> = lines.par_iter().map(|line| parse_star(line)).collect();

    let mut bounds = Bounds::from_stars(&stars);
    let mut step = 0;
    while (bounds.max_y - bounds.min_y).abs() > 15 {
        stars = stars.par_iter().map(|star| move_star(&star)).collect();
        bounds = Bounds::from_stars(&stars);
        step+=1;
    }
    // Part 1
    print_stars(&stars);
    
    // Part 2
    println!("{}", step);
}
