use std::collections::BTreeMap;
use crate::automata::terms::CellState;
use crate::automata::terms_multitape::TapeNo;

/// A single tape's cell state, tagged with the tape it belongs to.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct MultiTapeState {
    pub tape_no: TapeNo,
    pub tape_cell_state: CellState,
}
impl MultiTapeState {
    pub fn new(tape_no: TapeNo, tape_cell_state: CellState) -> MultiTapeState {
        MultiTapeState { tape_no, tape_cell_state }
    }
}
