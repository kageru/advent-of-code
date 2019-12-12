use std::cmp::Ordering;
use std::io::{self, BufRead};
#[macro_use]
extern crate scan_fmt;

#[derive(Debug, PartialEq, Eq, Clone)]
struct Moon {
    x: i64,
    y: i64,
    z: i64,
    x_vel: i64,
    y_vel: i64,
    z_vel: i64,
}

impl std::fmt::Display for Moon {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(
            fmt,
            "pos=<x={}, y={}, z={}>, vel=<x={}, y={}, z={}>",
            self.x, self.y, self.z, self.x_vel, self.y_vel, self.z_vel
        )
    }
}

impl std::fmt::Display for System {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(
            fmt,
            "{}\n{}\n{}\n{}",
            self.moons[0], self.moons[1], self.moons[2], self.moons[3],
        )
    }
}

impl Moon {
    fn mv(&mut self) {
        self.x += self.x_vel;
        self.y += self.y_vel;
        self.z += self.z_vel;
    }

    fn calculate_gravity(&mut self, others: &[Moon]) {
        for moon in others {
            self.x_vel += int(self.x.cmp(&moon.x));
            self.y_vel += int(self.y.cmp(&moon.y));
            self.z_vel += int(self.z.cmp(&moon.z));
        }
    }

    fn energy(&self) -> i64 {
        (self.x.abs() + self.y.abs() + self.z.abs())
            * (self.x_vel.abs() + self.y_vel.abs() + self.z_vel.abs())
    }
}

#[derive(Clone)]
struct System {
    moons: Vec<Moon>,
}

impl System {
    // TODO: don’t take ownership
    fn step(self) -> Self {
        let old_moons = self.moons.clone();
        System {
            moons: self
                .moons
                .into_iter()
                .map(|mut moon| {
                    moon.calculate_gravity(&old_moons);
                    moon.mv();
                    moon
                })
                .collect(),
        }
    }
}

fn main() {
    let system = System {
        moons: io::stdin()
            .lock()
            .lines()
            .map(|l| parse(&l.unwrap()))
            .collect(),
    };
    let mut part1_system = system.clone();
    for _ in 0..1000 {
        part1_system = part1_system.step();
    }
    let energy: i64 = part1_system.moons.into_iter().map(|m| m.energy()).sum();
    println!("Part 1: {}", energy);

    let mut part2_system = system.clone();
    let initial_x = extract_attribute(&part2_system, |m| (m.x, m.x_vel));
    let initial_y = extract_attribute(&part2_system, |m| (m.y, m.y_vel));
    let initial_z = extract_attribute(&part2_system, |m| (m.z, m.z_vel));
    let (mut x_found, mut y_found, mut z_found) = (0u64, 0, 0);
    for i in 1.. {
        let _: u64 = i;
        part2_system = part2_system.step();
        let xs = extract_attribute(&part2_system, |m| (m.x, m.x_vel));
        if x_found == 0 && xs == initial_x {
            x_found = i;
        }
        let ys = extract_attribute(&part2_system, |m| (m.y, m.y_vel));
        if y_found == 0 && ys == initial_y {
            y_found = i;
        }
        let zs = extract_attribute(&part2_system, |m| (m.z, m.z_vel));
        if z_found == 0 && zs == initial_z {
            z_found = i;
        }
        if x_found != 0 && y_found != 0 && z_found != 0 {
            break;
        }
    }
    println!("Part 2: {}", lcm(lcm(x_found, y_found), z_found));
}

fn extract_attribute<F>(system: &System, f: F)  -> Vec<(i64, i64)> where F: (FnMut(Moon) -> (i64, i64)){
    system.moons.clone().into_iter().map(f).collect()
}

fn int(ord: Ordering) -> i64 {
    match ord {
        Ordering::Less => 1,
        Ordering::Equal => 0,
        Ordering::Greater => -1,
    }
}

fn gcd(mut x: u64, mut y: u64) -> u64 {
    let mut remainder;
    while y != 0 {
        remainder = x % y;
        x = y;
        y = remainder;
    }
    x
}

fn lcm(x: u64, y: u64) -> u64 {
    x * y / gcd(x, y)
}

#[rustfmt::skip]
fn parse(line: &str) -> Moon {
    let (x, y, z) = scan_fmt!(line, "<x={}, y={}, z={}>", i64, i64, i64).unwrap();
    Moon { x, y, z, x_vel: 0, y_vel: 0, z_vel: 0, }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gcd() {
        assert_eq!(gcd(20, 10), 10);
        assert_eq!(gcd(20, 15), 5);
        assert_eq!(gcd(15, 20), 5);
    }

    #[test]
    fn test_lcm() {
        assert_eq!(lcm(20, 10), 20);
        assert_eq!(lcm(3, 7), 21);
        assert_eq!(lcm(7, 3), 21);
        assert_eq!(lcm(lcm(7, 3), 5), 105);
    }

    #[test]
    #[rustfmt::skip]
    fn test_parse() {
        assert_eq!(parse("<x=-14, y=9, z=-4>"), Moon { x: -14, y: 9, z: -4, x_vel: 0, y_vel: 0, z_vel: 0 });
        assert_ne!(parse("<x=-14, y=9, z=-4>"), Moon { x: 14, y: 9, z: -4, x_vel: 0, y_vel: 0, z_vel: 0 });
    }
}
