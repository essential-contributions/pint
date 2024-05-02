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
pub struct State(BTreeMap<ContentAddress, BTreeMap<Key, Word>>);

#[derive(Debug, Error)]
#[error("no value for the given intent set, key pair")]
pub struct InvalidStateRead;

impl State {
    // Shorthand test state constructor.
    pub fn new(sets: Vec<(ContentAddress, Vec<(Key, Word)>)>) -> Self {
        State(
            sets.into_iter()
                .map(|(addr, vec)| {
                    let map: BTreeMap<_, _> = vec.into_iter().collect();
                    (addr, map)
                })
                .collect(),
        )
    }

    // Update the value at the given key within the given intent set address.
    pub fn set(&mut self, set_addr: ContentAddress, key: &Key, value: Option<Word>) {
        let set = self.0.entry(set_addr).or_default();
        match value {
            None => {
                set.remove(key);
            }
            Some(value) => {
                set.insert(*key, value);
            }
        }
    }

    /// Retrieve a word range.
    pub fn word_range(
        &self,
        set_addr: ContentAddress,
        mut key: Key,
        num_words: usize,
    ) -> Result<Vec<Option<Word>>, InvalidStateRead> {
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
                .cloned();
            words.push(opt);
            key = next_key(key).ok_or(InvalidStateRead)?;
        }
        Ok(words)
    }
}

impl core::ops::Deref for State {
    type Target = BTreeMap<ContentAddress, BTreeMap<Key, Word>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl StateRead for State {
    type Error = InvalidStateRead;
    type Future = Ready<Result<Vec<Option<Word>>, Self::Error>>;
    fn word_range(&self, set_addr: ContentAddress, key: Key, num_words: usize) -> Self::Future {
        future::ready(self.word_range(set_addr, key, num_words))
    }
}
