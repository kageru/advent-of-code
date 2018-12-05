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

fn calculate_sleep(events: &Vec<Event>) -> Vec<bool> {
    let mut minutes = vec![false; 60];
    let mut sleeping = false;
    let mut last = 0;
    for e in events {
        if sleeping {
            for i in last..e.datetime.minute {
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

fn count_awake_minutes(minutes: &Vec<bool>) -> i32 {
    let mut sum = 0;
    for minute in minutes {
        if  *minute {
            sum+=1;
        }
    }
    return sum;
}

fn find_sleepy_guard(guards: &HashMap<i32, Vec<Shift>>) -> i32 {
    let mut max_sleep = 0;
    let mut id_of_sleepiest = -1;
    for (guard, shifts) in guards {
        let mut minutes_asleep = 0;
        for shift in shifts {
            minutes_asleep += count_awake_minutes(&calculate_sleep(&shift.events));
        }
        if minutes_asleep > max_sleep {
            max_sleep = minutes_asleep;
            id_of_sleepiest = guard.clone();
        }
    }
    return id_of_sleepiest;
}

fn get_sleepy_minute(shifts: &Vec<Shift>) -> (i32, i32) {
    let mut sleep_during_shifts = vec![0; 60];
    for shift in shifts {
        let minutes_asleep = calculate_sleep(&shift.events);
        for i in 0..sleep_during_shifts.len() {
            sleep_during_shifts[i] += if minutes_asleep[i] {1} else {0};
        }
    }
    let sleepy_minute = sleep_during_shifts.iter().max().unwrap();
    return (*sleepy_minute, sleep_during_shifts.iter().position(|&m| &m == sleepy_minute).unwrap() as i32);
}

fn main() {
    let lines: Vec<&str> = include_str!("../input").split("\n").collect();
    
    let mut events: Vec<Event> = Vec::new();
    for line in lines {
        events.push(event_from_line(line.to_string()));
    }

    events.sort();
    let shifts = events_to_shifts(events);
    let shifts_by_guard = group_shifts_by_guard(shifts);
    
    // part 1
    let mut sleepy_guard = find_sleepy_guard(&shifts_by_guard);
    let sleepy_minute = get_sleepy_minute(&shifts_by_guard.get(&sleepy_guard).unwrap()).1;
    println!("part 1: guard {}, minute {}: {}", sleepy_guard, sleepy_minute, sleepy_guard * sleepy_minute);
    
    // part 2
    let (mut sleepy_minute_amount, mut sleepy_minute_id) = (-1, -1);
    sleepy_guard = -1;
    for (guard, shifts) in shifts_by_guard {
        let (minute_amount, minute_id) = get_sleepy_minute(&shifts);
        if minute_amount > sleepy_minute_amount {
            sleepy_minute_amount = minute_amount;
            sleepy_minute_id = minute_id;
            sleepy_guard = guard;
        }
    }
    println!("part 2: guard {}, minute {}, asleep for {}: {}", sleepy_guard, sleepy_minute_id, sleepy_minute_amount, sleepy_guard * sleepy_minute_id);
}

