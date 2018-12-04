use std::cmp;

pub struct Event {
    datetime: DateTime,
    action: GuardAction,
}

pub enum GuardAction {
    BeginShift(i32),
    FallAsleep,
    WakeUp,
}

pub struct Guard {
    events: Vec<Event>,
    id: i32,
}

#[derive(Eq)]
pub struct DateTime {
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

impl Event {
    pub fn new(datetime: DateTime, action: GuardAction) -> Self {
        Event {
            datetime,
            action
        }
    }
}

