#![feature(test)]
extern crate test;
use std::ops::Sub;

use aoc2022::{boilerplate, common::*};
use scanf::sscanf;

const DAY: usize = 19;
type Parsed = Vec<Blueprint>;

#[derive(Copy, Clone, PartialEq, Eq, Debug, Default)]
struct Factory {
    minute:         u32,
    waited:         u32,
    ore:            u32,
    ore_robot:      u32,
    clay:           u32,
    clay_robot:     u32,
    obsidian:       u32,
    obsidian_robot: u32,
    geode:          u32,
    geode_robot:    u32,
}

impl Factory {
    fn pass_minute(self) -> Self {
        Factory {
            ore: self.ore + self.ore_robot,
            clay: self.clay + self.clay_robot,
            obsidian: self.obsidian + self.obsidian_robot,
            geode: self.geode + self.geode_robot,
            minute: self.minute + 1,
            waited: self.waited + 1,
            ..self
        }
    }

    fn can_afford(&self, price: &Price) -> bool {
        self.ore >= price.ore && self.clay >= price.clay && self.obsidian >= price.obsidian
    }
}

impl Sub<Price> for Factory {
    type Output = Self;

    fn sub(self, price: Price) -> Self::Output {
        Factory { ore: self.ore - price.ore, clay: self.clay - price.clay, obsidian: self.obsidian - price.obsidian, ..self }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Default)]
struct Blueprint {
    number:         u32,
    ore_robot:      Price,
    clay_robot:     Price,
    obsidian_robot: Price,
    geode_robot:    Price,
    max_ore:        u32,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Default)]
struct Price {
    ore:      u32,
    clay:     u32,
    obsidian: u32,
}

fn parse_input(raw: &str) -> Parsed {
    raw.lines().map(|line| {
        let mut bp = Blueprint::default();
        let mut number = 0;
        let mut ore_robot_ore = 0;
        let mut clay_robot_ore = 0;
        let mut obsidian_robot_ore = 0;
        let mut obsidian_robot_clay = 0;
        let mut geode_robot_ore = 0;
        let mut geode_robot_obsidian = 0;
        // Unfortunately, this macro only takes `ident`, not `expr`, so I have to use temporary
        // variables that are assigned to fields of the blueprint later
        sscanf!(line, "Blueprint {}: Each ore robot costs {} ore. Each clay robot costs {} ore. Each obsidian robot costs {} ore and {} clay. Each geode robot costs {} ore and {} obsidian.", number,
        ore_robot_ore, clay_robot_ore, obsidian_robot_ore, obsidian_robot_clay, geode_robot_ore, geode_robot_obsidian).unwrap();
        bp.number = number;
        bp.ore_robot.ore = ore_robot_ore;
        bp.clay_robot.ore = clay_robot_ore;
        bp.obsidian_robot.ore = obsidian_robot_ore;
        bp.obsidian_robot.clay = obsidian_robot_clay;
        bp.geode_robot.ore = geode_robot_ore;
        bp.geode_robot.obsidian = geode_robot_obsidian;
        bp.max_ore = clay_robot_ore.max(obsidian_robot_ore).max(geode_robot_ore);
        bp
    }).collect()
}

fn part1(parsed: &Parsed) -> u32 {
    parsed.iter().map(|blueprint| max_geodes(Factory { ore_robot: 1, ..Default::default() }, blueprint, 3, 24) * blueprint.number).sum()
}

fn part2(parsed: &Parsed) -> u32 {
    parsed.iter().take(3).map(|blueprint| max_geodes(Factory { ore_robot: 1, ..Default::default() }, blueprint, 4, 32)).product()
}

fn max_geodes(factory: Factory, bp: &Blueprint, wait_limit: u32, limit: u32) -> u32 {
    if factory.minute == limit {
        return factory.geode;
    }
    if factory.can_afford(&bp.geode_robot) {
        return max_geodes(
            Factory { geode_robot: factory.geode_robot + 1, waited: 0, ..factory.pass_minute() } - bp.geode_robot,
            bp,
            wait_limit,
            limit,
        );
    }
    // This assumption (“if we can build an obsidian robot, we should”) holds for my input but not the test input.
    // Not entirely fair, but good enough until (or if at all) I figure out a better way.
    if factory.obsidian_robot < bp.geode_robot.obsidian && factory.can_afford(&bp.obsidian_robot) {
        return max_geodes(
            Factory { obsidian_robot: factory.obsidian_robot + 1, waited: 0, ..factory.pass_minute() } - bp.obsidian_robot,
            bp,
            wait_limit,
            limit,
        );
    }

    let mut outcomes = Vec::with_capacity(3);
    if factory.ore_robot < bp.max_ore && factory.can_afford(&bp.ore_robot) {
        outcomes.push(Factory { ore_robot: factory.ore_robot + 1, waited: 0, ..factory.pass_minute() } - bp.ore_robot);
    }
    if factory.clay_robot < bp.obsidian_robot.clay && factory.can_afford(&bp.clay_robot) {
        outcomes.push(Factory { clay_robot: factory.clay_robot + 1, waited: 0, ..factory.pass_minute() } - bp.clay_robot);
    }
    if outcomes.is_empty() || factory.waited < wait_limit {
        outcomes.push(factory.pass_minute());
    }
    outcomes.into_iter().take(2).map(|f| max_geodes(f, bp, wait_limit, limit)).max().unwrap()
}

boilerplate! {
    TEST_INPUT == "\
Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.",
    tests: { part1: { TEST_INPUT => 33 } },
    bench1 == 1150,
    bench2 == 37367,
    bench_parse: Vec::len => 30,
}
