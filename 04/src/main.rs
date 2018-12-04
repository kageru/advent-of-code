#[macro_use] extern crate text_io;
use std::{fs, cmp};

enum GuardAction {
    BeginShift(i32),
    FallAsleep,
    WakeUp,
}

struct Guard {
    events: Vec<Event>,
    id: i32,
}

#[derive(Eq)]
struct DateTime {
    month: i32,
    day: i32,
    hour: i32,
    minute: i32,
    sortable_time: i32,
}
    // Calculate the absolute minute count relative to 01/01 0:00. Months are assumed to have 31
    // days because we just want to ensure that a higher month always results in a higher
    // timestamp. Think of this as a primitive and less correct unix epoch.
    pub fn get_sortable_time(month: i32, day: i32, hour: i32, minute: i32) -> i32 {
        return minute + hour * 60 + day * 60 * 24 + month * 60 * 24 * 31;
    }


impl DateTime {
    pub fn new(month: i32, day: i32, hour: i32, minute: i32) -> Self {
        DateTime {
            month,
            day,
            hour,
            minute,
            sortable_time: get_sortable_time(month, day, hour, minute)
        }
    }

}

impl Ord for DateTime {
    fn cmp(&self, other: &DateTime) -> cmp::Ordering {
        self.sortable_time.cmp(&other.sortable_time)
    }
}

impl PartialOrd for DateTime {
    fn partial_cmp(&self, other: &DateTime) -> Option<cmp::Ordering> {
        Some(other.cmp(self))
    }
}

impl PartialEq for DateTime {
    fn eq(&self, other: &DateTime) -> bool {
        self.sortable_time == other.sortable_time
    }
}

struct Event {
    datetime: DateTime,
    action: GuardAction,
}

impl Event {
    pub fn new(datetime: DateTime, action: GuardAction) -> Self {
        Event {
            datetime,
            action
        }
    }
}

fn parse_action(input: &String) -> GuardAction {
    match input[0] {
        "f" => GuardAction::FallAsleep,
        "w" => GuardAction::WakeUp,
        "G" => {
            let gid: i32;
            scan!(input.bytes() => "Guard #{} begins shift", gid);
            GuardAction::BeginShift(gid)
        }
        _ => // undefined?
    }
}

fn event_from_line(line: &String) -> Event {
    let (month, day, hour, minute, unparsed_action): (i32, i32, i32, i32, String);
    scan!(line.bytes() => "[1518-{}-{} {}:{}] {}", month, day, hour, minute, unparsed_action);
    let datetime = DateTime::new(month, day, hour, minute);
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
