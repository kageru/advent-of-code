#[macro_use] extern crate text_io;
use std::fs;
mod types;
use types::*;


fn parse_action(input: &String) -> GuardAction {
    match input.chars().next().unwrap() {
        'f' => GuardAction::FallAsleep,
        'w' => GuardAction::WakeUp,
        'G' => {
            let gid: i32;
            scan!(input.bytes() => "Guard #{} begins shift", gid);
            GuardAction::BeginShift(gid)
        }
        _  => std::panic!()
    }
}

fn event_from_line(line: &String) -> Event {
    let (month, day, hour, minute, unparsed_action): (i32, i32, i32, i32, String);
    scan!(line.bytes() => "[1518-{}-{} {}:{}] {}", month, day, hour, minute, unparsed_action);
    let datetime = types::DateTime::new(month, day, hour, minute);
    return Event::new(datetime, parse_action(&unparsed_action));
}

fn main() {
    let file = fs::read_to_string("../input").expect("can’t access file");
    let lines: Vec<&str> = file.split("\n").collect();
    let mut events: Vec<Event> = Vec::new();
    for line in lines {
        events.push(event_from_line(&line.to_string()));
    }
}

