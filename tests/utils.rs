use essential_state_read_vm::{
    types::{ContentAddress, Key, Word},
    StateRead,
};
use std::{
    collections::BTreeMap,
    future::{self, Ready},
};
use thiserror::Error;

// A test `StateRead` implementation represented using a map.
#[derive(Clone)]
pub struct State(BTreeMap<ContentAddress, BTreeMap<Key, Vec<Word>>>);

#[derive(Debug, Error)]
#[error("no value for the given intent set, key pair")]
pub struct InvalidStateRead;

pub type Kv = (Key, Vec<Word>);

impl State {
    // Shorthand test state constructor.
    pub fn new(sets: Vec<(ContentAddress, Vec<Kv>)>) -> Self {
        State(
            sets.into_iter()
                .map(|(addr, vec)| {
                    let map: BTreeMap<_, _> = vec.into_iter().collect();
                    (addr, map)
                })
                .collect(),
        )
    }

    pub fn set(&mut self, set_addr: ContentAddress, key: &Key, value: Vec<Word>) {
        let set = self.0.entry(set_addr).or_default();
        if value.is_empty() {
            set.remove(key);
        } else {
            set.insert(key.clone(), value);
        }
    }

    /// Retrieve a word range.
    pub fn key_range(
        &self,
        set_addr: ContentAddress,
        mut key: Key,
        num_words: usize,
    ) -> Result<Vec<Vec<Word>>, InvalidStateRead> {
        // Get the key that follows this one.
        fn next_key(mut key: Key) -> Option<Key> {
            for w in key.iter_mut().rev() {
                match *w {
                    Word::MAX => *w = Word::MIN,
                    _ => {
                        *w += 1;
                        return Some(key);
                    }
                }
            }
            None
        }

        // Collect the words.
        let mut words = vec![];
        for _ in 0..num_words {
            let opt = self
                .get(&set_addr)
                .ok_or(InvalidStateRead)?
                .get(&key)
                .cloned()
                .unwrap_or_else(|| Vec::with_capacity(0));
            words.push(opt);
            key = next_key(key).ok_or(InvalidStateRead)?;
        }
        Ok(words)
    }
}

impl core::ops::Deref for State {
    type Target = BTreeMap<ContentAddress, BTreeMap<Key, Vec<Word>>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl StateRead for State {
    type Error = InvalidStateRead;
    type Future = Ready<Result<Vec<Vec<Word>>, Self::Error>>;
    fn key_range(&self, set_addr: ContentAddress, key: Key, num_words: usize) -> Self::Future {
        future::ready(self.key_range(set_addr, key, num_words))
    }
}
