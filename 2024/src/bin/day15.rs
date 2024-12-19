#![feature(test, if_let_guard)]
extern crate test;
use aoc2024::{
    boilerplate,
    common::*,
    direction::Direction,
    grid::{Grid, VecGrid},
    position::Pos,
};

const DAY: usize = 15;
type Map = VecGrid<Tile>;
type Parsed = (Map, Vec<Direction>);
type P = Pos<usize, 2>;

#[repr(u8)]
#[allow(dead_code)] // transmuted into
#[derive(Clone, Copy, PartialEq, Eq)]
enum Tile {
    Wall = b'#',
    Box = b'O',
    Robot = b'@',
    Empty = b'.',
}

fn parse_input(raw: &str) -> Parsed {
    let (map, path) = raw.split_once("\n\n").unwrap();
    let map = VecGrid::transmute_from_lines(map);
    let path = path
        .bytes()
        .filter(|&b| b != b'\n')
        .map(|b| match b {
            b'^' => Direction::Up,
            b'>' => Direction::Right,
            b'<' => Direction::Left,
            b'v' => Direction::Down,
            _ => unreachable!(),
        })
        .collect();
    (map, path)
}

fn move_box(map: &Map, pos: P, dir: Direction) -> Option<P> {
    match map[pos + dir] {
        Tile::Empty => Some(pos + dir),
        Tile::Wall => None,
        Tile::Box => move_box(map, pos + dir, dir),
        Tile::Robot => unreachable!(),
    }
}

fn part1((map, path): &Parsed) -> usize {
    let mut pos = map.indices().find(|&p| map[p] == Tile::Robot).expect("no robot on map");
    let mut map = map.clone();
    for &dir in path {
        let newpos = pos + dir;
        match map[newpos] {
            Tile::Empty => (map[pos], map[newpos], pos) = (Tile::Empty, Tile::Robot, newpos),
            Tile::Box if let Some(free) = move_box(&map, pos + dir, dir) => {
                (map[free], map[pos], map[newpos], pos) = (Tile::Box, Tile::Empty, Tile::Robot, newpos)
            }
            _ => (),
        }
    }
    let height = map.0.len() - 1;
    map.indices().filter(|&p| map[p] == Tile::Box).map(|Pos([x, y])| 100 * (height - x) + y).sum()
}

fn part2(_parsed: &Parsed) -> usize {
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
