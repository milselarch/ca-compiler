use std::collections::{HashMap};
use std::hash::{Hash, Hasher};
use crate::automata::composer::{Direction, TapeKey, TapeState};
use indexmap::{IndexMap, IndexSet};
use indexmap::set::Iter as IndexSetIter;

#[derive(Debug, Clone)]
#[derive(Eq)]
#[derive(PartialEq)]
pub struct StateOverlaps {
    pub overlaps: IndexSet<TapeState>
}
impl StateOverlaps {
    pub fn new() -> StateOverlaps {
        StateOverlaps {
            // we use index set to ensure deterministic hashes
            overlaps: IndexSet::new(),
        }
    }
    pub fn insert_overlap(&mut self, tape_state: TapeState) -> bool {
        self.overlaps.insert(tape_state)
    }
    pub fn get_tape_keys(&self) -> IndexSet<TapeKey> {
        let mut tape_keys = IndexSet::new();
        for overlap in self.overlaps.iter() {
            tape_keys.insert(overlap.get_tape_key());
        }
        tape_keys
    }
    pub fn iter(&self) -> indexmap::set::Iter<'_, TapeState> {
        self.overlaps.iter()
    }
}
impl Hash for StateOverlaps {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for overlap in self.overlaps.iter() {
            overlap.hash(state);
        }
    }
}
impl<'a> IntoIterator for &'a StateOverlaps {
    type Item = &'a TapeState;
    type IntoIter = IndexSetIter<'a, TapeState>;

    fn into_iter(self) -> Self::IntoIter {
        self.overlaps.iter()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DirectionStateOverlaps {
    // we use index map to ensure deterministic hashes
    map: IndexMap<Direction, StateOverlaps>
}
impl DirectionStateOverlaps {
    pub fn new() -> DirectionStateOverlaps {
        DirectionStateOverlaps {
            map: IndexMap::new(),
        }
    }
    pub fn to_pairs(&self) -> IndexSet<(Direction, TapeState)> {
        let mut pairs = IndexSet::new();
        for (direction, state_overlaps) in self.map.iter() {
            for tape_state in state_overlaps.overlaps.iter() {
                pairs.insert((direction.clone(), tape_state.clone()));
            }
        }
        pairs
    }
    pub fn get_or_insert_entry(
        &mut self, direction: Direction
    ) -> &mut StateOverlaps {
        self.map.entry(direction).or_insert(StateOverlaps::new())
    }
    pub fn read_entry(
        &self, direction: &Direction
    ) -> Option<&StateOverlaps> {
        self.map.get(direction)
    }
    pub fn get_tape_keys(&self) -> IndexSet<TapeKey> {
        let mut tape_keys = IndexSet::new();
        for (_direction, state_overlaps) in self.map.iter() {
            let overlaps_keys = state_overlaps.get_tape_keys();
            for key in overlaps_keys.into_iter() {
                tape_keys.insert(key);
            }
        }
        tape_keys
    }
    pub fn contains_pair(
        &self, direction: &Direction, tape_state: &TapeState
    ) -> bool {
        if let Some(state_overlaps) = self.read_entry(direction) {
            state_overlaps.overlaps.contains(tape_state)
        } else {
            false
        }
    }
    pub fn insert_pair(
        &mut self, direction: Direction, tape_state: TapeState
    ) -> bool {
        let state_overlaps = self.get_or_insert_entry(direction);
        state_overlaps.insert_overlap(tape_state)
    }
}
impl Hash for DirectionStateOverlaps {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for (direction, state_overlaps) in self.map.iter() {
            direction.hash(state);
            state_overlaps.hash(state);
        }
    }
}

#[derive(Debug, Clone)]
pub struct AutomataDirectionStateOverlaps {
    /*
    represents which states can overlap with which other states
    in which directions (left, right, middle)
    */
    map: IndexMap<TapeState, DirectionStateOverlaps>,
}
impl AutomataDirectionStateOverlaps {
    pub fn new() -> AutomataDirectionStateOverlaps {
        AutomataDirectionStateOverlaps {
            map: IndexMap::new(),
        }
    }
    pub fn get_or_insert_entry(
        &mut self, tape_state: TapeState
    ) -> &mut DirectionStateOverlaps {
        self.map.entry(tape_state).or_insert(DirectionStateOverlaps::new())
    }
    pub fn insert_entry(
        &mut self, tape_state: TapeState,
        direction_overlaps: DirectionStateOverlaps
    ) {
        if self.map.contains_key(&tape_state) {
            panic!("Entry for tape state already exists");
        }
        self.map.insert(tape_state, direction_overlaps);
    }
    pub fn read_entry(
        &self, tape_state: &TapeState
    ) -> Option<&DirectionStateOverlaps> {
        self.map.get(tape_state)
    }
    pub fn load_tape_states(&self) -> IndexSet<TapeState> {
        let mut tape_states = IndexSet::new();
        for (tape_state, _direction_overlaps) in self.map.iter() {
            tape_states.insert(tape_state.clone());
        }
        tape_states
    }
}
