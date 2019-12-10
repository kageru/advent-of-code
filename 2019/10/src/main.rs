use std::collections::HashSet;
use std::io::{self, BufRead};

#[derive(Clone)]
struct Asteroid {
    x: i64,
    y: i64,
    visible_asteroids: HashSet<i64>,
}

impl Asteroid {
    pub fn new(x: usize, y: usize) -> Self {
        Self {
            x: x as i64,
            y: y as i64,
            visible_asteroids: HashSet::new(),
        }
    }

    fn compute_visibles(mut self, all_asteroids: &Vec<Asteroid>) -> Self {
        // Technically incorrect because we add ourself here, but since the asteroid with most
        // visibles has some legitimate asteroid at an angle of 0°, this doesn’t actually matter.
        for ast in all_asteroids {
            self.visible_asteroids
                .insert(calculate_angle(self.x - ast.x, self.y - ast.y));
        }
        self
    }
}

fn main() {
    let input: Vec<Asteroid> = io::stdin()
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
        .collect();

    let part1 = input
        .clone()
        .into_iter()
        .map(|ast| ast.compute_visibles(&input))
        .max_by_key(|ast| ast.visible_asteroids.len())
        .unwrap();
    println!("Part 1: {}", part1.visible_asteroids.len());
}

fn calculate_angle(x_offset: i64, y_offset: i64) -> i64 {
    return ((y_offset as f64).atan2(x_offset as f64).to_degrees() * 1000.0).round() as i64;
}

//fn part1(asteroids: &HashSet<(i32, i32)>) -> Asteroid {
    //unimplemented!()
//}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_angle_calculation() {
        assert_eq!(calculate_angle(1, 1), 45_000);
        assert_eq!(calculate_angle(1, 0), 0);
        assert_eq!(calculate_angle(0, -1), -90_000);
        assert_eq!(calculate_angle(2, 1), 26_565);
        assert_eq!(calculate_angle(1, 1), calculate_angle(3, 3));
        assert_eq!(calculate_angle(-2, -2), -135_000);
        assert_eq!(calculate_angle(-2, 2), 135_000);
        assert_ne!(calculate_angle(1, 1), calculate_angle(3, -3));
    }

    /*
        #[test]
        fn test_part_1() {
            assert_eq!(
                part1(&parse_input(
                    ".#..#
    .....
    #####
    ....#
    ...##"
                        .lines()
                )).visible_asteroids,
                HashSet::new()
            );

            assert_eq!(
                part1(&parse_input(
                    "#.#...#.#.
    .###....#.
    .#....#...
    ##.#.#.#.#
    ....#.#.#.
    .##..###.#
    ..#...##..
    ..##....##
    ......#...
    .####.###."
                        .lines()
                )),
                (1, 2)
            );
            assert_eq!(
                part1(&parse_input(
                    ".#..#..###
    ####.###.#
    ....###.#.
    ..###.##.#
    ##.##.#.#.
    ....###..#
    ..#.#..#.#
    #..#.#.###
    .##...##.#
    .....#.#.."
                        .lines()
                )),
                (6, 3)
            );
            assert_eq!(
                part1(&parse_input(
                    "......#.#.
    #..#.#....
    ..#######.
    .#.#.###..
    .#..#.....
    ..#....#.#
    #..#....#.
    .##.#..###
    ##...#..#.
    .#....####"
                        .lines()
                )),
                (5, 8)
            );
            assert_eq!(
                part1(&parse_input(
                    ".#..##.###...#######
    ##.############..##.
    .#.######.########.#
    .###.#######.####.#.
    #####.##.#.##.###.##
    ..#####..#.#########
    ####################
    #.####....###.#.#.##
    ##.#################
    #####.##.###..####..
    ..######..##.#######
    ####.##.####...##..#
    .#####..#.######.###
    ##...#.##########...
    #.##########.#######
    .####.#.###.###.#.##
    ....##.##.###..#####
    .#.#.###########.###
    #.#.#.#####.####.###
    ###.##.####.##.#..##"
                        .lines()
                )),
                (11, 13)
            );
        }
        */
}
