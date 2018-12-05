#[macro_use] extern crate text_io;
use std::collections::HashMap;
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

fn event_from_line(line: String) -> Event {
    let (month, day, hour, minute, unparsed_action): (i32, i32, i32, i32, String);
    // I’m only adding the \n here to use it as a marker for scan!,
    // which would otherwise stop at the first space.
    let line2 = line.to_string() + "\n";
    scan!(line2.bytes() => "[1518-{}-{} {}:{}] {}\n", month, day, hour, minute, unparsed_action);
    let datetime = types::DateTime::new(month, day, hour, minute);
    return Event::new(datetime, parse_action(&unparsed_action));
}

fn events_to_shifts(events: Vec<Event>) -> Vec<Shift> {
    let mut shifts: Vec<Shift> = Vec::new();
    // It was easier to parse by just adding a dummy shift at the beginning
    let mut current_shift = Shift::new(-1);
    for event in events {
        match event.action {
            GuardAction::BeginShift(gid) => {
                shifts.push(current_shift);
                current_shift = Shift::new(gid);
            },
            _ => current_shift.events.push(event)
        }

    }
    // Remove the dummy we added earlier
    shifts.remove(0);
    return shifts;
}

fn group_shifts_by_guard(shifts: Vec<Shift>) -> HashMap<i32, Vec<Shift>> {
    let mut shifts_by_guard: HashMap<i32, Vec<Shift>> = HashMap::new();
    for shift in shifts {
        let shifts_for_guard = shifts_by_guard.entry(shift.guard.id).or_insert_with(Vec::new);
        shifts_for_guard.push(shift);
    }
    return shifts_by_guard;
}

fn calculate_sleep(events: Vec<Event>) -> Vec<bool> {
    let mut minutes = vec![false; 60];
    let mut sleeping = false;
    let mut last = 0;
    for e in events {
        if sleeping {
            for i in (last..e.datetime.minute) {
                minutes[i as usize] = true;
            }
        }
        last = e.datetime.minute;
        sleeping = match e.action {
            GuardAction::FallAsleep => true,
            _ => false
        }
    }
    return minutes;
}

fn main() {
    let lines: Vec<&str> = include_str!("../input").split("\n").collect();
    
    let mut events: Vec<Event> = Vec::new();
    for line in lines {
        events.push(event_from_line(line.to_string()));
    }

    events.sort();
    let shifts = events_to_shifts(events);
    /*
    for shift in shifts {
        for event in shift.events {
            println!("{}", event.datetime);
        }
    }
    */
    let shifts_by_guard = group_shifts_by_guard(shifts);
    let sleepy_guard = find_sleepy_guard(&shifts_by_guard);
}

