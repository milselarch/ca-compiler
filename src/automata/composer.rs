use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;
use std::ops::Mul;
use enum_iterator::Sequence;

type TapeKey = usize;
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

pub fn get_cell_expectation_combo_product(
    expectations1: &HashSet<CellExpectationCombo>,
    expectations2: &HashSet<CellExpectationCombo>,
) -> Result<HashSet<CellExpectationCombo>, MultiplyComboConflict> {
    /*
    Returns the Cartesian product of two sets of cell expectation combos
    */
    let mut product_combos = HashSet::new();

    for combo1 in expectations1 {
        for combo2 in expectations2 {
            let product_res = combo1.multiply(combo2);
            let product_combo = match product_res {
                Ok(combo) => combo,
                Err(_conflict) => {
                    return Err(_conflict);
                }
            };
            product_combos.insert(product_combo);
        }
    }
    Ok(product_combos)
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Sequence)]
pub enum Direction {
    Left,
    Right,
    Middle,
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
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
            self.expected_state.tape_key.clone(),
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CellExpectationCombo {
    /*
    Represents the expectation that a bunch of adjacent cells
    certain corresponding states
    */
    cell_expectations: HashMap<TapeCellIdentifier, CellExpectation>
}
impl CellExpectationCombo {
    pub fn new(
        cell_expectations: HashMap<TapeCellIdentifier, CellExpectation>
    ) -> CellExpectationCombo {
        CellExpectationCombo { cell_expectations }
    }
    pub fn new_empty() -> CellExpectationCombo {
        CellExpectationCombo {
            cell_expectations: HashMap::new()
        }
    }
    pub fn insert_expectation(
        &mut self, expectation: CellExpectation
    ) {
        let identifier = expectation.to_identifier();
        // ensure no duplicate expectations for same tape cell
        let prev_value = self.cell_expectations.insert(identifier, expectation);
        assert_eq!(prev_value, None);
    }
    pub fn multiply(
        &self, other: &CellExpectationCombo
    ) -> Result<CellExpectationCombo, MultiplyComboConflict> {
        let mut combined_expectations = self.cell_expectations.clone();
        for (identifier, expectation) in &other.cell_expectations {
            let prev_value = combined_expectations.insert(
                identifier.clone(), expectation.clone()
            );

            if let Some(existing_expectation) = prev_value {
                // conflict detected
                return Err(MultiplyComboConflict::new(
                    identifier.clone(),
                    existing_expectation,
                    expectation.clone(),
                ));
            }
        }
        Ok(CellExpectationCombo {
            cell_expectations: combined_expectations
        })
    }
}
impl Hash for CellExpectationCombo {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let mut expectations_vec: Vec<&CellExpectation> =
            self.cell_expectations.values().collect();
        expectations_vec.sort();
        for expectation in expectations_vec {
            expectation.hash(state);
        }
    }
}
impl Mul for CellExpectationCombo {
    type Output = Result<CellExpectationCombo, MultiplyComboConflict>;

    fn mul(self, rhs: Self) -> Self::Output {
        self.multiply(&rhs)
    }
}

#[derive(Debug, Clone)]
#[derive(Eq, Hash, PartialEq)]
pub struct WriteRule {
    expectations: CellExpectationCombo,
    // new state to apply to cell at current position, current tape
    write_output: TapeCellState,
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

    pub fn get_dependent_tape_keys(&self) -> HashMap<TapeKey, HashSet<WriteRule>> {
        /*
        Get the tape keys of all tapes that the current tape uses as its input
        i.e. get all tape keys of all the tapes where the state of cells
        in the current tape are dependent on the states of cells in those tapes

        For every tape key, we also store the set of WriteRules that contain
        that tape key as one of its input expectations
        */
        let mut dependent_tape_keys: HashMap<
            TapeKey, HashSet<WriteRule>
        > = HashMap::new();

        for rule in &self.write_rules {
            let cell_expectations = &rule.expectations;
            for expectation in cell_expectations.cell_expectations.values() {
                let tape_key = expectation.expected_state.tape_key;
                let tape_key_entry =
                    dependent_tape_keys.entry(tape_key).or_insert_with(HashSet::new);
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
    pub fn generate_all_combinations(&self) -> HashSet<CellExpectationCombo> {
        /*
        Generates all possible combinations of cells within a 1-cell radius
        */
        let mut combinations = HashSet::new();

        for direction in enum_iterator::all::<Direction>() {
            let mut combination = CellExpectationCombo::new_empty();
            for tape_cell_state in &self.allowed_states {
                let cell_expectation = self.build_cell_expectation(
                    *tape_cell_state, direction.clone()
                );
                combination.insert_expectation(cell_expectation);
            }

            combinations.insert(combination);
        }

        combinations
    }

    pub fn get_normal_states(&self) -> HashSet<TapeCellState> {
        // get all states except for VOID_STATE and HALT_STATE
        self.allowed_states.clone().into_iter().filter(|x| {
            *x != VOID_STATE && *x != HALT_STATE
        }).collect()
    }
}

#[derive(Debug, Clone)]
pub struct MultiTape {
    tapes: Vec<Tape>,
    input_tape_key: TapeKey,
    tape_names_map: HashMap<String, TapeKey>,
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
                Err(existing_key.clone())
            },
            None => {
                let tape_index: TapeKey = self.tapes.len();
                self.tapes.push(tape);
                let tape_key  = tape_index;
                self.tape_names_map.insert(name, tape_key.clone());
                Ok(tape_key)
            }
        }
    }

    pub fn get_tape_by_key(&self, tape_key: TapeKey) -> Option<&Tape> {
        self.tapes.get(tape_key)
    }

    pub fn get_tapes_that_write_to_tape(
        &self, target_tape_key: &TapeKey, exclude_self: bool
    ) -> HashSet<TapeKey> {
        /*
        Returns the tape keys of the write tapes that would write to
        the given target tape key
        */
        let mut writing_tape_keys = HashSet::new();

        for tape in &self.tapes {
            for rule in &tape.write_rules {
                let output_tape_keys = rule.get_output_tape_keys();
                if !output_tape_keys.contains(target_tape_key) {
                    continue;
                }
                let tape_key = tape.get_tape_key();
                if exclude_self && &tape_key == target_tape_key {
                    continue;
                }
                writing_tape_keys.insert(tape_key);
            }
        }
        writing_tape_keys
    }
    pub fn generate_tape_equation(&self) {
        /*
        Generates the multi-tape equations for all tapes
        */
        /*
        represents which states can overlap with which other states
        in which directions (left, right, middle)
        */
        let mut state_direction_map: HashMap<
            TapeState, HashMap<Direction, HashSet<TapeState>>
        > = HashMap::new();

        let mut frontier = HashSet::new();
        frontier.insert(self.input_tape_key);
        let input_tape = self.get_tape_by_key(self.input_tape_key).unwrap();
        let input_tape_void: TapeState = TapeState::new(self.input_tape_key, VOID_STATE);
        // let input_tape_halt: TapeState = TapeState::new(self.input_tape_key, HALT_STATE);

        for state in input_tape.get_normal_states() {
            /*
            Every state on the input tape can have void neighbors on the left
            and right, and every state can have left, right, and middle overlaps
            with every other input state
            TODO: this initialization is already so damn long, refactor
            */
            for direction in enum_iterator::all::<Direction>() {
                // all the neighbors for the current state in the current direction
                let mut input_state_neighbors_map:
                    HashMap<Direction, HashSet<TapeState>> = HashMap::new();

                for other_state in input_tape.get_normal_states() {
                    // every input state can overlap with every other input state
                    // (including itself) in any direction
                    let neighbor_tape_state =
                        TapeState::new(self.input_tape_key, other_state);
                    let neighbors_set =
                        input_state_neighbors_map.get_mut(&direction).unwrap();
                    neighbors_set.insert(neighbor_tape_state);
                }

                match direction {
                    Direction::Left => {
                        // every state can have void as a left neighbor
                        let left_neighbors: &mut HashSet<TapeState> =
                            input_state_neighbors_map.get_mut(&Direction::Left).unwrap();
                        left_neighbors.insert(input_tape_void.clone());
                    },
                    Direction::Right => {
                        // every state can have void as a right neighbor
                        let right_neighbors: &mut HashSet<TapeState> =
                            input_state_neighbors_map.get_mut(&Direction::Right).unwrap();
                        right_neighbors.insert(input_tape_void.clone());
                    }
                    Direction::Middle => {}
                };

                let current_tape_state = TapeState::new(self.input_tape_key, state);
                state_direction_map.insert(
                    current_tape_state.clone(),
                    input_state_neighbors_map.clone()
                );
            }
        }

        while !frontier.is_empty() {
            let mut next_frontier = HashSet::new();

            for tape_key in &frontier {
                let tape = self.get_tape_by_key(*tape_key).unwrap();
                let dependent_tape_keys = tape.get_dependent_tape_keys();

                for frontier_tape_key in frontier.iter() {
                    if !dependent_tape_keys.contains_key(frontier_tape_key) { continue; }
                    next_frontier.insert(frontier_tape_key.clone());
                }


                todo!()
            }
            frontier = next_frontier;
        }
    }
    // TODO: a method to propagate all possible state combinations
}
