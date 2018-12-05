use std::{cmp,fmt};

pub struct Event {
    pub datetime: DateTime,
    pub action: GuardAction,
}

pub enum GuardAction {
    BeginShift(i32),
    FallAsleep,
    WakeUp,
}

pub struct Guard {
    pub id: i32,
}

pub struct Shift {
    pub guard: Guard,
    pub events: Vec<Event>,
}

#[derive(Eq)]
pub struct DateTime {
    pub month: i32,
    pub day: i32,
    pub hour: i32,
    pub minute: i32,
    pub sortable_time: i32,
}
    // Calculate the absolute minute count relative to 01/01 0:00. Months are assumed to have 31
    // days because we just want to ensure that a higher month always results in a higher
    // timestamp. Think of this as a primitive and less correct unix epoch.
    pub fn get_sortable_time(month: i32, day: i32, hour: i32, minute: i32) -> i32 {
        return minute + hour * 60 + day * 60 * 24 + month * 60 * 24 * 31;
    }

impl Shift {
    pub fn new(gid: i32) -> Self {
        Shift {
            guard: Guard::new(gid),
            events: Vec::new()
        }
    }
}

impl Guard {
    pub fn new(id: i32) -> Self {
        Guard { id }
    }
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

impl fmt::Display for DateTime {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{} {}:{}", self.day, self.month, self.hour, self.minute)
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

impl Event {
    pub fn new(datetime: DateTime, action: GuardAction) -> Self {
        Event {
            datetime,
            action
        }
    }
}

impl Eq for Event {}

impl Ord for Event {
    fn cmp(&self, other: &Event) -> cmp::Ordering {
        self.datetime.cmp(&other.datetime)
    }
}

impl PartialOrd for Event {
    fn partial_cmp(&self, other: &Event) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Event {
    fn eq(&self, other: &Event) -> bool {
        self.datetime == other.datetime
    }
}

