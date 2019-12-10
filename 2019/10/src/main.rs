use std::collections::HashMap;
use std::io::{self, BufRead};

#[derive(Clone, Debug)]
struct Asteroid {
    x: i64,
    y: i64,
    visible_asteroids: HashMap<i64, Vec<Asteroid>>,
}

impl Asteroid {
    pub fn new(x: usize, y: usize) -> Self {
        Self {
            x: x as i64,
            y: y as i64,
            visible_asteroids: HashMap::new(),
        }
    }

    fn compute_visibles(mut self, all_asteroids: &[Asteroid]) -> Self {
        for ast in all_asteroids {
            if ast.x == self.x && ast.y == self.y {
                continue;
            }
            let angle = calculate_angle(self.x - ast.x, self.y - ast.y);
            self.visible_asteroids.entry(angle).or_insert_with(Vec::new);
            self.visible_asteroids
                .get_mut(&angle)
                .unwrap()
                .push(ast.to_owned());
        }
        self
    }

    fn fire_mah_laz0r(self, limit: usize) -> Asteroid {
        let x = self.x;
        let y = self.y;
        let mut visibles: Vec<(i64, Vec<Asteroid>)> = self
            .visible_asteroids
            .into_iter()
            .map(|(angle, mut asts)| {
                asts.sort_by_key(|a| -((a.x - x).abs() + (a.y - y).abs()));
                (angle, asts)
            })
            .collect();
        visibles.sort_by_key(|&(angle, _)| angle);
        let mut num_destroyed = 0;
        for (_, mut asts) in visibles.into_iter().cycle() {
            match asts.pop() {
                None => (),
                Some(ast) => {
                    num_destroyed += 1;
                    if num_destroyed == limit {
                        return ast;
                    }
                }
            }
        }
        unreachable!()
    }
}

fn read_input() -> Vec<Asteroid> {
     io::stdin()
        .lock()
        .lines()
        .enumerate()
        .flat_map(|(y, l)| {
            l.unwrap()
                .chars()
                .enumerate()
                .filter(|(_, c)| c == &'#')
                .map(move |(x, _)| Asteroid::new(x, y))
                .collect::<Vec<_>>()
        })
        .collect()
}

fn main() {
    let input = read_input();
    let part1 = input
        .clone()
        .into_iter()
        .map(|ast| ast.compute_visibles(&input))
        .max_by_key(|ast| ast.visible_asteroids.len())
        .unwrap();
    println!("Part 1: {}", part1.visible_asteroids.len());
    let part2 = part1.fire_mah_laz0r(200);
    println!("Part 2: {}", part2.x * 100 + part2.y);
}

fn calculate_angle(x_offset: i64, y_offset: i64) -> i64 {
    // Angles in the assignment start at 0 degrees when pointing up (negative y), so we have to
    // offset them here.
    let mut raw_angle =
        (((y_offset as f64).atan2(x_offset as f64).to_degrees() * 1000.0).round() as i64) - 90_000;
    if raw_angle < 0 {
        raw_angle += 360_000;
    }
    raw_angle
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_angle_calculation() {
        assert_eq!(calculate_angle(0, -1), 0);
        assert_eq!(calculate_angle(1, 0), 90_000);
        assert_eq!(calculate_angle(1, 1), 135_000);
        assert_eq!(calculate_angle(-1, -1), 315_000);
    }
}
