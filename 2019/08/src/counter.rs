use std::collections::HashMap;
use std::ops::Index;
use std::hash::Hash;

#[derive(Debug)]
pub struct Counter<T: Hash + Eq + Clone> {
    values: HashMap<T, usize>
}

impl <T: Hash + Eq + Clone> Counter<T> {
    pub fn new() -> Self {
        Self { values: HashMap::new() }
    }

    pub fn of<C: IntoIterator<Item=T>>(collection: C) -> Counter<T> {
        let mut counter = Counter::new();
        counter.add(collection);
        counter
    }

    pub fn add<C: IntoIterator<Item=T>>(&mut self, collection: C) {
        for x in collection {
            self.values.insert(x.clone(), self.values.get(&x).unwrap_or(&0) + 1);
        }
    }
}

impl <T: Hash + Eq + Clone> Index<T> for Counter<T> {
    type Output = usize;

    fn index(&self, idx: T) -> &usize {
        self.values.get(&idx).unwrap_or(&0)
    }
}
