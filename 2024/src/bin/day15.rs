#![feature(test, if_let_guard)]
extern crate test;
use Tile::*;
use aoc2024::{
    boilerplate,
    common::*,
    direction::Direction::{self, *},
    grid::{Grid, VecGrid},
    position::Pos,
};

const DAY: usize = 15;
type Map = VecGrid<Tile>;
type Parsed = (Map, Vec<Direction>);
type P = Pos<usize, 2>;

#[repr(u8)]
#[allow(dead_code)] // transmuted into
#[derive(Clone, Copy, PartialEq, Eq, Default)]
enum Tile {
    Wall = b'#',
    Box = b'O',
    Robot = b'@',
    #[default]
    Empty = b'.',
    BoxLeft = b'[',
    BoxRight = b']',
}

fn parse_input(raw: &str) -> Parsed {
    let (map, path) = raw.split_once("\n\n").unwrap();
    let map = VecGrid::transmute_from_lines(map);
    let path = path
        .bytes()
        .filter(|&b| b != b'\n')
        .map(|b| match b {
            b'^' => Up,
            b'>' => Right,
            b'<' => Left,
            b'v' => Down,
            _ => unreachable!(),
        })
        .collect();
    (map, path)
}

fn move_box(map: &Map, pos: P, dir: Direction) -> Option<P> {
    match map[pos + dir] {
        Empty => Some(pos + dir),
        Wall => None,
        Box => move_box(map, pos + dir, dir),
        _ => unreachable!(),
    }
}

fn part1((map, path): &Parsed) -> usize {
    let mut pos = map.find(&Robot).expect("no robot on map");
    let mut map = map.clone();
    for &dir in path {
        let newpos = pos + dir;
        match map[newpos] {
            Empty => (map[pos], map[newpos], pos) = (Empty, Robot, newpos),
            Box if let Some(free) = move_box(&map, pos + dir, dir) => (map[free], map[pos], map[newpos], pos) = (Box, Empty, Robot, newpos),
            _ => (),
        }
    }
    let height = map.0.len() - 1;
    map.indices().filter(|&p| map[p] == Box).map(|Pos([x, y])| 100 * (height - x) + y).sum()
}

fn part2((map, path): &Parsed) -> usize {
    let map: VecGrid<Tile> =
        map.0.iter().map(|line| line.iter().flat_map(|&t| if t == Box { [BoxLeft, BoxRight] } else { [t, t] }).collect()).collect();
    unimplemented!()
}

boilerplate! {
    TEST_INPUT == "\
##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"
    for tests: {
        part1: {
            TEST_INPUT => 10092,
            "\
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<" => 2028,
        },
        part2: { TEST_INPUT => 9021 },
    },
    bench1 == 1577255,
    bench2 == 0,
    bench_parse: |(map, path): &Parsed| (map.len(), path.len()) => (50, 20_000),
}
