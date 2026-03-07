use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::ops::Index;
use cartesian::cartesian;
use enum_iterator::Sequence;
use indexmap::{IndexMap, IndexSet};
use crate::automata::overlaps::{AutomataDirectionStateOverlaps, DirectionStateOverlaps};
use crate::automata::terms::Expression;

pub(crate) type TapeKey = usize;
type TapeCellState = u32;

/*
TODO:
    - add rules accumulator
    - translate rules to multi-tape equations
    - compose multi-tape automata to single-tape automata
    - APL style rule builders (accumulator, reduct input, unary expansion)
    - theres a lot of runtime behavior that might be movable to compile time
*/

const VOID_STATE: TapeCellState = 0;
const HALT_STATE: TapeCellState = 1;

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Sequence)]
/*
relative position of a cell with respect to the current cell
we assume left / right means a -1 / +1 offset from the current cell
position respectively, and middle means the current cell position itself
*/
pub enum Direction {
    Left,
    Right,
    Middle,
}
impl Direction {
    pub fn to_left(&self) -> Option<Direction> {
        match self {
            Direction::Left => None,
            Direction::Right => Some(Direction::Middle),
            Direction::Middle => Some(Direction::Left),
        }
    }
    pub fn to_right(&self) -> Option<Direction> {
        match self {
            Direction::Left => Some(Direction::Middle),
            Direction::Right => None,
            Direction::Middle => Some(Direction::Right),
        }
    }
    pub fn flip(&self) -> Direction {
        match self {
            Direction::Left => Direction::Right,
            Direction::Right => Direction::Left,
            Direction::Middle => Direction::Middle,
        }
    }
    pub fn to_offset(&self) -> i32 {
        match self {
            Direction::Left => -1,
            Direction::Middle => 0,
            Direction::Right => 1,
        }
    }
    pub fn add_direction(&self, direction: Direction) -> Option<Direction> {
        match direction {
            Direction::Left => self.to_left(),
            Direction::Middle => Some(self.clone()),
            Direction::Right => self.to_right(),
        }
    }
    pub fn subtract_direction(&self, direction: Direction) -> Option<Direction> {
        match direction {
            Direction::Left => self.to_right(),
            Direction::Middle => Some(self.clone()),
            Direction::Right => self.to_left(),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct TapeState {
    tape_key: TapeKey,
    tape_cell_state: TapeCellState
}
impl TapeState {
    pub fn new(
        tape_key: TapeKey,
        tape_cell_state: TapeCellState
    ) -> TapeState {
        TapeState {
            tape_key,
            tape_cell_state,
        }
    }
    pub fn get_tape_key(&self) -> TapeKey {
        self.tape_key
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct CellExpectation {
    direction: Direction,
    expected_state: TapeState,
}
impl CellExpectation {
    pub fn new(
        direction: Direction,
        expected_state: TapeState
    ) -> CellExpectation {
        CellExpectation {
            direction,
            expected_state,
        }
    }

    pub fn to_identifier(&self) -> TapeCellIdentifier {
        TapeCellIdentifier::new(
            self.expected_state.tape_key,
            self.direction.clone(),
        )
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct TapeCellIdentifier {
    tape_key: TapeKey,
    direction: Direction,
}
impl TapeCellIdentifier {
    pub fn new(
        tape_key: TapeKey,
        direction: Direction
    ) -> TapeCellIdentifier {
        TapeCellIdentifier {
            tape_key,
            direction,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MultiplyComboConflict {
    /*
    Represents a conflict when multiplying two
    CellExpectationCombos
    */
    conflicting_identifier: TapeCellIdentifier,
    expectation_a: CellExpectation,
    expectation_b: CellExpectation,
}
impl MultiplyComboConflict {
    pub fn new(
        conflicting_identifier: TapeCellIdentifier,
        expectation_a: CellExpectation,
        expectation_b: CellExpectation,
    ) -> MultiplyComboConflict {
        MultiplyComboConflict {
            conflicting_identifier,
            expectation_a,
            expectation_b,
        }
    }
}

#[derive(Debug, Clone)]
#[derive(Eq, Hash, PartialEq)]
pub struct WriteRule {
    expectations: DirectionStateOverlaps,
    /*
    New state to apply to cell at current position, current tape

    Current position = Direction::Middle
    Which is to say if there was an input cell expectation at
    Direction::Middle, then the position being written to is the same
    as the position of said cell expectation
    */
    write_output: TapeCellState,
}
impl WriteRule {
    pub fn new(
        expectations: DirectionStateOverlaps,
        write_output: TapeCellState,
    ) -> WriteRule {
        WriteRule {
            expectations,
            write_output,
        }
    }
    pub fn to_pairs(&self) -> IndexSet<(Direction, TapeState)> {
        /*
        Convert the write rule expectations to a list of
        (direction, state) pairs
        */
        self.expectations.to_pairs()
    }
    pub fn is_satisfiable_for_automata_overlaps(
        &self, automata_state_overlaps: &AutomataDirectionStateOverlaps
    ) -> bool {
        /*
        Check if the input conditions of the right rule are satisfiable
        given the current state_direction_map (set of all possible state overlaps)

        write rule expectations can be construed as
        (direction, state) pairs.

        The plan here is:
        if for every (direction, state) pair in the
        write rule expectations, all the other pairs exist in the
        state_direction_map, then the write rule is satisfiable.
        */
        let rule_pairs = self.to_pairs();

        for rule_pair in rule_pairs.iter() {
            for other_rule_pair in rule_pairs.iter() {
                if rule_pair == other_rule_pair { continue; }

                let rule_direction = &rule_pair.0;
                let rule_tape_state = &rule_pair.1;
                let other_rule_direction = &other_rule_pair.0;
                let other_rule_tape_state = &other_rule_pair.1;
                // in practice one of the directions has to be middle
                // otherwise it would go out of bounds in the state direction map
                let offset_direction = rule_direction.add_direction(
                    other_rule_direction.clone()
                ).unwrap();

                let state_overlaps_opt =
                    automata_state_overlaps.read_entry(&rule_tape_state);
                let state_overlaps = match state_overlaps_opt {
                    Some(so) => so,
                    None => { return false; }
                };
                if !state_overlaps.contains_pair(
                    &offset_direction, other_rule_tape_state
                ) {
                    return false;
                }
            }
        }
        true
    }
    pub fn is_satisfiable_for_overlaps(
        &self, state_overlaps: &DirectionStateOverlaps
    ) -> bool {
        /*
        Checks if the state overlaps

        for each direction, the direction state overlaps
        should be the same or a superset of the direction state overlaps
        in the write rule expectations
        */
        for direction in enum_iterator::all::<Direction>() {
            let dir_rule_overlaps_opt = self.expectations.read_entry(&direction);
            let dir_state_overlaps_opt = state_overlaps.read_entry(&direction);

            let dir_rule_overlaps = match dir_rule_overlaps_opt {
                Some(ro) => ro,
                None => {
                    /*
                    if the write rule doesn't have any expectations for the
                    current direction then we can skip the check for this direction
                    */
                    continue;
                }
            };
            let dir_state_overlaps_dir = match dir_state_overlaps_opt {
                Some(so) => so,
                None => {
                    /*
                    rule overlaps exist for the current direction but there
                    are no state overlaps in the current direction,
                    so the rule is not satisfiable
                    */
                    return false;
                }
            };
            for rule_overlap in dir_rule_overlaps.overlaps.iter() {
                if !dir_state_overlaps_dir.overlaps.contains(rule_overlap) {
                    return false;
                }
            }
        }
        true
    }
}


#[derive(Debug, Clone)]
pub struct BidirectionalTape {
    // cells extending rightwards
    data: Vec<TapeCellState>,
    // cells extending leftwards
    rev_data: Vec<TapeCellState>
}
impl BidirectionalTape {
    pub fn new(
        data: Vec<TapeCellState>,
    ) -> BidirectionalTape {
        BidirectionalTape {
            data,
            rev_data: vec![],
        }
    }
}

#[derive(Debug, Clone)]
pub struct Tape {
    tape_index: usize,
    write_rules: Vec<WriteRule>,
    allowed_states: HashSet<TapeCellState>,
    data: BidirectionalTape
}
impl Tape {
    pub fn new(
        write_rules: Vec<WriteRule>,
        tape_index: usize,
        data: Vec<TapeCellState>,
    ) -> Tape {
        Tape {
            write_rules,
            allowed_states: Default::default(),
            tape_index,
            data: BidirectionalTape::new(data)
        }
    }
    pub fn get_tape_key(&self) -> TapeKey {
        self.tape_index
    }

    pub fn get_dependent_tape_keys(&self) -> IndexMap<TapeKey, IndexSet<WriteRule>> {
        /*
        Get the tape keys of all tapes that the current tape uses as its input
        i.e. get all tape keys of all the tapes where the state of cells
        in the current tape are dependent on the states of cells in those tapes

        For every tape key, we also store the set of WriteRules that contain
        that tape key as one of its input expectations
        */
        let mut dependent_tape_keys: IndexMap<
            TapeKey, IndexSet<WriteRule>
        > = IndexMap::new();

        for rule in &self.write_rules {
            let cell_expectations = &rule.expectations;
            let cell_expectations_tape_keys = cell_expectations.get_tape_keys();

            for tape_key in cell_expectations_tape_keys.iter() {
                let tape_key_entry =
                    dependent_tape_keys.entry(*tape_key).or_insert_with(IndexSet::new);
                tape_key_entry.insert(rule.clone());
            }
        }
        dependent_tape_keys
    }

    pub fn build_cell_expectation(
        &self, tape_cell_state: TapeCellState, direction: Direction
    ) -> CellExpectation {
        let tape_state = TapeState::new(
            self.tape_index,
            tape_cell_state,
        );
        let cell_expectation = CellExpectation::new(
            direction,
            tape_state,
        );
        cell_expectation
    }
    pub fn get_normal_states(&self) -> HashSet<TapeCellState> {
        // get all states except for VOID_STATE and HALT_STATE
        self.allowed_states.clone().into_iter().filter(|x| {
            *x != VOID_STATE && *x != HALT_STATE
        }).collect()
    }
}

pub struct BuildFrontierResult {
    pub state_direction_map: AutomataDirectionStateOverlaps,
    pub output_tape_state_to_rule_map: IndexMap<TapeState, IndexSet<WriteRule>>,
    pub output_tapes_map: IndexMap<TapeKey, IndexSet<TapeKey>>,
    pub frontier: IndexSet<TapeKey>,
}

#[derive(Debug, Clone)]
pub struct MultiTape {
    tapes: Vec<Tape>,
    input_tape_key: TapeKey,
    tape_names_map: HashMap<String, TapeKey>,
    // rules that write to the cells of the current tape
    rules: Vec<WriteRule>,
}
impl MultiTape {
    pub fn new(
        tapes: Vec<Tape>,
        input_tape_key: TapeKey,
    ) -> MultiTape {
        MultiTape {
            tapes,
            input_tape_key,
            tape_names_map: Default::default(),
            rules: vec![],
        }
    }
    pub fn get_tape_key(&self, name: &str) -> Option<&TapeKey> {
        self.tape_names_map.get(name)
    }
    pub fn insert_named_tape(
        &mut self, name: String, tape: Tape
    ) -> Result<TapeKey, TapeKey> {
        let get_result = self.tape_names_map.get(name.as_str());

        match get_result {
            Some(existing_key) => {
                Err(*existing_key)
            },
            None => {
                let tape_index: TapeKey = self.tapes.len();
                self.tapes.push(tape);
                let tape_key  = tape_index;
                self.tape_names_map.insert(name, tape_key);
                Ok(tape_key)
            }
        }
    }

    pub fn get_tape_by_key(&self, tape_key: TapeKey) -> Option<&Tape> {
        self.tapes.get(tape_key)
    }

    pub fn init_state_direction_map(&self) -> AutomataDirectionStateOverlaps {
        /*
        Build the initial map of which states can overlap with which other states
        in every direction (left, right, same position)
        */
        let mut all_state_direction_overlaps = AutomataDirectionStateOverlaps::new();
        let input_tape = self.get_tape_by_key(self.input_tape_key).unwrap();
        let input_tape_void: TapeState = TapeState::new(self.input_tape_key, VOID_STATE);
        // let input_tape_halt: TapeState = TapeState::new(self.input_tape_key, HALT_STATE);

        for state in input_tape.get_normal_states() {
            /*
            Every state on the input tape can have void neighbors on the left
            and right, and every state can have left, right, and middle overlaps
            with every other input state
            */
            let current_tape_state = TapeState::new(self.input_tape_key, state);
            // all the possible overlaps for the current state in every direction
            let state_direction_overlaps =
                all_state_direction_overlaps.get_or_insert_entry(current_tape_state);

            for direction in enum_iterator::all::<Direction>() {
                let direction_overlaps =
                    state_direction_overlaps.get_or_insert_entry(direction.clone());

                for other_state in input_tape.get_normal_states() {
                    // every input state can overlap with every other input state
                    // (including itself) in any direction
                    let neighbor_tape_state =
                        TapeState::new(self.input_tape_key, other_state);
                    direction_overlaps.insert_overlap(neighbor_tape_state);
                }

                match direction {
                    Direction::Left => {
                        // every state can have void (own tape) as a left neighbor
                        direction_overlaps.insert_overlap(input_tape_void.clone());
                    },
                    Direction::Right => {
                        // every state can have void (own tape) as a right neighbor
                        direction_overlaps.insert_overlap(input_tape_void.clone());
                    }
                    Direction::Middle => {}
                };
            }

            for _other_tape_key in 0..self.tapes.len() {
                /*
                The void state of all other tapes can overlap positionally
                with every input tape state in all directions
                */
                let other_tape_key = _other_tape_key as TapeKey;
                if other_tape_key == self.input_tape_key { continue; }
                let other_tape_void_state = TapeState::new(other_tape_key, VOID_STATE);

                for direction in enum_iterator::all::<Direction>() {
                    let direction_overlaps =
                        state_direction_overlaps.get_or_insert_entry(direction);
                    direction_overlaps.insert_overlap(other_tape_void_state.clone());
                }
            }
        }

        all_state_direction_overlaps
    }

    pub fn build_output_tapes_map(&self) -> IndexMap<TapeKey, IndexSet<TapeKey>> {
        /*
        match each tape to the set of tapes that it can effect state change directly
        i.e. match each tape to the other tapes that it can "write to"
        */
        let mut output_tapes_map: IndexMap<TapeKey, IndexSet<TapeKey>> = IndexMap::new();

        for tape in &self.tapes {
            let tape_key = tape.get_tape_key();
            let dependent_tape_keys = tape.get_dependent_tape_keys();

            for dependent_tape_key in dependent_tape_keys.keys() {
                let output_tapes_entry =
                    output_tapes_map.entry(*dependent_tape_key).or_insert_with(IndexSet::new);
                output_tapes_entry.insert(tape_key);
            }
        }
        output_tapes_map
    }

    pub fn apply_write_rule(
        &mut self, write_rule: WriteRule, tape_key: TapeKey,
        state_direction_map: &mut AutomataDirectionStateOverlaps,
    ) {
        let output_tape_state = TapeState::new(
            tape_key, write_rule.write_output,
        );
        state_direction_map.insert_entry(
            output_tape_state, write_rule.expectations.clone()
        );
        let input_pairs = write_rule.expectations.to_pairs();
        for pair in input_pairs {
            let (direction, input_tape_state) = pair;
            let inv_direction = direction.flip();

            let tape_direction_state_overlaps_opt =
                state_direction_map.get_or_insert_entry(input_tape_state);
            let tape_state_overlaps_opt =
                tape_direction_state_overlaps_opt.get_or_insert_entry(inv_direction);
            tape_state_overlaps_opt.insert_overlap(output_tape_state.clone());
        }
    }

    pub fn build_frontier(&self) -> BuildFrontierResult {
        let output_tapes_map = self.build_output_tapes_map();
        /*
        represents which tape states might overlap with which other tape states
        in which directions (left, right, middle) across all tapes
        */
        let mut state_direction_map = self.init_state_direction_map();
        /*
        Map output tape states to the write rules that produce those tape states as output
        */
        let mut output_tape_state_to_rule_map: IndexMap<
            TapeState, IndexSet<WriteRule>
        > = IndexMap::new();
        /*
        The frontier contains all the tape keys whose corresponding tapes
        might contribute to new write rules that have not yet been processed
        */
        let mut frontier = IndexSet::new();
        frontier.insert(self.input_tape_key);

        while !frontier.is_empty() {
            let mut next_frontier = IndexSet::new();

            for tape_key in &frontier {
                let tape = self.get_tape_by_key(*tape_key).unwrap();
                let output_tapes_opt = output_tapes_map.get(tape_key);
                let output_tapes = match output_tapes_opt {
                    Some(ot) => ot,
                    None => { continue; }
                };

                for output_tape_key in output_tapes {
                    let write_rules =
                        tape.get_dependent_tape_keys().get(output_tape_key).unwrap().clone();

                    for write_rule in write_rules {
                        let write_rule_satisfiable =
                            write_rule.is_satisfiable_for_automata_overlaps(&state_direction_map);
                        if !write_rule_satisfiable { continue; }

                        let output_tape_cell_state = write_rule.write_output;
                        let output_tape_state = TapeState::new(
                            *tape_key, output_tape_cell_state
                        );
                        /*
                        add output tape state to the new frontier
                        now that we know the current write rule is satisfiable
                        */
                        next_frontier.insert(*output_tape_key);
                        /*
                        insert all neighbors of the output tape state
                        (from write rule inputs)
                        */
                        state_direction_map.insert_entry(
                            output_tape_state, write_rule.expectations.clone()
                        );
                        /*
                        add the write rule to the set of rules that
                        produce the output tape state
                        */
                        let tape_key_write_rules_set = output_tape_state_to_rule_map
                            .entry(output_tape_state)
                            .or_insert(IndexSet::new());
                        tape_key_write_rules_set.insert(write_rule.clone());

                        // insert the output tape state as a neighbor
                        // to all the input tape states in the write rule
                        let input_pairs = write_rule.expectations.to_pairs();
                        for input_pair in input_pairs {
                            let input_direction = &input_pair.0;
                            let input_tape_state = &input_pair.1;
                            // from the perspective of the input tape state,
                            // the output tape state is in the opposite direction
                            let inv_direction = input_direction.flip();
                            let input_direction_state_neighbors =
                                state_direction_map.get_or_insert_entry(*input_tape_state);
                            input_direction_state_neighbors.insert_pair(
                                inv_direction, output_tape_state
                            );
                        }
                        // find out what other downstream tapes are affected
                        // using this write rule and output_tapes_map
                    }
                }
            }
            frontier = next_frontier;
        }
        BuildFrontierResult {
            state_direction_map,
            output_tape_state_to_rule_map,
            output_tapes_map,
            frontier,
        }
    }

    pub fn generate_multi_tape_overlaps(&self) -> IndexMap<DirectionStateOverlaps, TapeState> {
        /*
        Generates the multi-tape equations for all tapes
        */
        let build_frontier_result = self.build_frontier();
        let state_direction_map = build_frontier_result.state_direction_map;
        let output_tape_states = state_direction_map.load_tape_states();
        let output_tape_state_to_rule_map =
            build_frontier_result.output_tape_state_to_rule_map;
        let mut input_to_output_state_map: IndexMap<
            DirectionStateOverlaps, TapeState
        >  = IndexMap::new();

        for output_tape_state in output_tape_states {
            // get all possible 1-radius combination of states
            let direction_state_overlaps =
                state_direction_map.read_entry(&output_tape_state).unwrap();
            let state_overlap_products = cartesian!(
                direction_state_overlaps.read_entry(&Direction::Left).unwrap(),
                direction_state_overlaps.read_entry(&Direction::Middle).unwrap(),
                direction_state_overlaps.read_entry(&Direction::Right).unwrap(),
            );

            // write rules that produce the current output tape state
            let relevant_write_rules_opt =
                output_tape_state_to_rule_map.get(&output_tape_state);
            let relevant_write_rules = match relevant_write_rules_opt {
                Some(rwr) => rwr.clone(),
                None => IndexSet::new(),
            };

            for (left, middle, right) in state_overlap_products {
                let state_overlap_factors = DirectionStateOverlaps::from_vec_pairs(&vec![
                    (Direction::Left, left),
                    (Direction::Middle, middle),
                    (Direction::Right, right),
                ]);
                for write_rule in &relevant_write_rules {
                    if !write_rule.is_satisfiable_for_overlaps(&state_overlap_factors) {
                        continue
                    }
                    let existing_value_opt = input_to_output_state_map.insert(
                        state_overlap_factors.clone(), output_tape_state
                    );
                    match existing_value_opt {
                        Some(existing_value) => {
                            if existing_value != output_tape_state {
                                // this means that the same combination of input
                                // states can lead to multiple different output states
                                // which is a conflict that needs to be resolved
                                panic!(
                                    "Conflict detected for input combination {:?}: \
                                    previously mapped to {:?}, now mapped to {:?}",
                                    state_overlap_factors, existing_value, output_tape_state
                                );
                            }
                        }
                        None => {}
                    }
                }
            }
        }
        input_to_output_state_map
    }

    pub fn to_state_equations(
        &self, input_to_output_state_map: IndexMap<DirectionStateOverlaps, TapeState>
    ) -> Expression {
        /*
        Collect all the tape states that exist across all tapes,
        so that we can reassign them to a global state space
        for the final multi-tape equations
        */
        let mut tape_states_map: IndexMap<TapeKey, TapeState> = IndexMap::new();

        for (input_combo, output_state) in input_to_output_state_map.iter() {
            let combo_tape_states = input_combo.get_tape_states();
            tape_states_map.entry(output_state.tape_key).or_insert(output_state.clone());
            combo_tape_states.iter().for_each(|tape_state| {
                tape_states_map.entry(tape_state.tape_key).or_insert(tape_state.clone());
            });
        }

        let mut global_state_counter: TapeCellState = 2;
        let mut global_tape_state_map: IndexMap<TapeState, TapeCellState> = IndexMap::new();
        todo!()
    }
}
