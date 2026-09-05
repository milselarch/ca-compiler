use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt;
use indexmap::{IndexMap, IndexSet};

use crate::automata::product_writes_map::{ProductWritesError, ProductWritesMap};
use crate::automata::renderer::{RenderFrame, TapeRenderFrame};
use crate::automata::rule_generator::{BidirectionalTape, TapeError, VOID_STATE};
use crate::automata::tape_overlaps::MultiTapeState;
use crate::automata::terms::CellState;
use crate::automata::terms_multitape::{
    AbstractMultiTapeExpression, MultiTapeExpression, MultiTapeProduct, TapeNo,
};

#[derive(Debug, Clone, Default)]
pub struct BiDirectionalMultiTape {
    /// `BTreeMap` so that iteration is always in ascending `TapeNo` order,
    /// matching the `sorted(self._tapes.keys())` calls on the Python side.
    tapes: BTreeMap<TapeNo, BidirectionalTape>,
    freeze_tapes: bool,
}
impl BiDirectionalMultiTape {
    pub fn new(tapes: BTreeMap<TapeNo, BidirectionalTape>) -> BiDirectionalMultiTape {
        BiDirectionalMultiTape { tapes, freeze_tapes: false }
    }

    pub fn get_or_make_tape(
        &mut self,
        tape_no: TapeNo,
    ) -> Result<&mut BidirectionalTape, TapeError> {
        if !self.tapes.contains_key(&tape_no) && self.freeze_tapes {
            return Err(TapeError::TapesFrozen { tape_no });
        }
        Ok(self.tapes.entry(tape_no).or_default())
    }

    /// Read-only lookup; unlike `get_or_make_tape` this never allocates a tape.
    pub fn get_tape(&self, tape_no: TapeNo) -> Option<&BidirectionalTape> {
        self.tapes.get(&tape_no)
    }

    pub fn init_tapes(&mut self, tape_nos: &[TapeNo], freeze: bool) -> Result<(), TapeError> {
        for tape_no in tape_nos {
            self.get_or_make_tape(*tape_no)?;
        }
        if freeze {
            self.freeze_tapes = true;
        }
        Ok(())
    }

    pub fn get_tape_nos(&self) -> Vec<TapeNo> {
        self.tapes.keys().copied().collect()
    }

    /// Populate the automata cells from `position` to `end_position`
    /// (inclusive) using `data` as a repeating pattern.
    pub fn write_region(
        &mut self,
        position: i64,
        end_position: i64,
        data: &[MultiTapeState],
    ) -> Result<(), TapeError> {
        if data.is_empty() {
            return Err(TapeError::EmptyWritePattern);
        }
        for new_position in position..=end_position {
            let offset = (new_position - position) as usize;
            let value = data[offset % data.len()];
            self.write(new_position, value)?;
        }
        Ok(())
    }

    pub fn write(&mut self, position: i64, value: MultiTapeState) -> Result<(), TapeError> {
        let tape = self.get_or_make_tape(value.tape_no)?;
        tape.write(position, value.tape_cell_state);
        Ok(())
    }

    pub fn get_all_states(&self) -> IndexSet<CellState> {
        let mut all_states = IndexSet::new();
        for tape in self.tapes.values() {
            all_states.extend(tape.get_all_states());
        }
        all_states
    }

    pub fn max_state(&self) -> CellState {
        self.tapes
            .values()
            .map(|tape| tape.max_state())
            .max()
            .unwrap_or(VOID_STATE)
    }

    pub fn prune(&mut self) {
        for tape in self.tapes.values_mut() {
            tape.prune();
        }
    }

    /// `cell_width == None` derives the width from the largest state present
    /// across all tapes (the `BLANK_INT` sentinel on the Python side).
    pub fn render_tapes(
        &self,
        start_position: i64,
        length: usize,
        cell_width: Option<usize>,
    ) -> Result<RenderFrame, TapeError> {
        let max_state = self.max_state();
        let min_cell_width = max_state.to_string().len();

        let cell_width = match cell_width {
            None => min_cell_width,
            Some(width) if width < min_cell_width => {
                return Err(TapeError::CellWidthTooSmall { cell_width: width, max_state });
            }
            Some(width) => width,
        };

        // tape 0 is always rendered, even if it holds no data yet
        let mut tape_nos: Vec<TapeNo> = self.get_tape_nos();
        if !tape_nos.contains(&0) {
            tape_nos.push(0);
            tape_nos.sort_unstable();
        }

        let left_tabs: Vec<String> = tape_nos
            .iter()
            .map(|tape_no| format!("Tape {}: ", tape_no))
            .collect();
        // padding keeps the sidebar rectangular even when tape numbers
        // differ in digit count
        let left_sidebar = RenderFrame::from_padded_lines(left_tabs);
        let content_width = length.saturating_sub(left_sidebar.get_width());

        let empty_tape = BidirectionalTape::default();
        let mut tape_view_lines: Vec<TapeRenderFrame> = Vec::with_capacity(tape_nos.len());

        for tape_no in &tape_nos {
            let tape = self.tapes.get(tape_no).unwrap_or(&empty_tape);
            let tape_line = tape.render_line(start_position, content_width, Some(cell_width))?;
            tape_view_lines.push(tape_line);
        }

        // TODO: align by actual space consumed by tape
        let num_cells = tape_view_lines.first().map(|frame| frame.num_cells).unwrap_or(0);
        // width of text actually consumed by tape cells, excluding padding
        let tape_content_width = tape_view_lines
            .first()
            .map(|frame| frame.get_space_consumed())
            .unwrap_or(0);

        let start_pos_str = format!("{}<", start_position);
        let end_pos_str = format!(">{}", start_position + num_cells as i64 - 1);
        let buffer_len = tape_content_width
            .saturating_sub(start_pos_str.len())
            .saturating_sub(end_pos_str.len());

        let position_str = format!(
            "{}{}{}{}{}",
            " ".repeat(left_sidebar.get_width()),
            start_pos_str,
            " ".repeat(buffer_len),
            end_pos_str,
            " ".repeat(content_width.saturating_sub(tape_content_width)),
        );

        let tape_frames: Vec<RenderFrame> =
            tape_view_lines.iter().map(|frame| (*frame).clone()).collect();
        let tapes_frame = RenderFrame::join_vertically(&tape_frames)?;
        let body = RenderFrame::join_horizontally(&[left_sidebar, tapes_frame])?;

        Ok(RenderFrame::join_vertically(&[
            RenderFrame::from_line(position_str),
            body,
        ])?)
    }

    /// Inclusive range of positions for which tape cell data is currently
    /// allocated across all tapes. Always contains 0.
    pub fn get_range(&self) -> (i64, i64) {
        let mut min_pos: i64 = 0;
        let mut max_pos: i64 = 0;

        for tape in self.tapes.values() {
            let (tape_min, tape_max) = tape.get_range();
            min_pos = min_pos.min(tape_min);
            max_pos = max_pos.max(tape_max);
        }
        (min_pos, max_pos)
    }
}

#[derive(Debug, Clone)]
pub enum AutomataError {
    Tape(TapeError),
    Writes(ProductWritesError),
    /// A product made purely out of void states would make the simulation
    /// range infinite, so it is rejected up-front.
    VoidProduct { product: String, output: MultiTapeState },
    /// `require_annotations` was set, but the product carried no annotation.
    EmptyAnnotation { product: String },
    /// Two different products want to write different states to the same cell.
    ConflictingWrite {
        tape_no: TapeNo,
        position: i64,
        product: String,
        annotation: String,
        previous: CellState,
        incoming: CellState,
        previous_annotations: Vec<String>,
    },
}

impl From<TapeError> for AutomataError {
    fn from(err: TapeError) -> Self {
        AutomataError::Tape(err)
    }
}
impl From<ProductWritesError> for AutomataError {
    fn from(err: ProductWritesError) -> Self {
        AutomataError::Writes(err)
    }
}

impl fmt::Display for AutomataError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AutomataError::Tape(err) => write!(f, "{}", err),
            AutomataError::Writes(err) => write!(f, "{}", err),
            AutomataError::VoidProduct { product, output } => write!(
                f,
                "Product {} transitions void states to non-void state {}, \
                 which is not allowed since it would make the simulation \
                 range infinite",
                product, output
            ),
            AutomataError::EmptyAnnotation { product } => {
                write!(f, "EMPTY ANNOTATION product={}", product)
            }
            AutomataError::ConflictingWrite {
                tape_no, position, product, annotation,
                previous, incoming, previous_annotations,
            } => write!(
                f,
                "Conflicting writes to tape {} from matching_product={} \
                 annotation={:?} at position {}: {} vs {} \
                 (prev_annotations={:?})",
                tape_no, product, annotation, position,
                previous, incoming, previous_annotations
            ),
        }
    }
}
impl std::error::Error for AutomataError {}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct WriteRecord {
    pub origin_product: MultiTapeProduct,
    /// `(tape_no, position)`
    pub write_target: (TapeNo, i64),
    pub tape_cell_state: CellState,
    /// for debugging purposes (to trace originating product)
    pub annotation: String,
}

impl WriteRecord {
    pub fn log(&self) {
        println!("{}", self);
    }
}

impl fmt::Display for WriteRecord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (tape_no, position) = self.write_target;
        write!(
            f,
            "{} | ({}, {}) -> {} ({})",
            self.origin_product._to_string("D"),
            tape_no, position, self.tape_cell_state, self.annotation
        )
    }
}

#[derive(Debug, Clone)]
pub struct ProcessStepResult {
    pub prev_multi_tape: BiDirectionalMultiTape,
    pub new_multi_tape: BiDirectionalMultiTape,
    pub active_writes: Vec<WriteRecord>,
}

pub struct MultiTapeAutomata {
    multi_tape: BiDirectionalMultiTape,
    prod_to_state_map: ProductWritesMap,
    leftmost_extent: i64,
    rightmost_extent: i64,
    /// `IndexMap` mirrors the insertion-ordered Python `dict`.
    state_eq_map: IndexMap<MultiTapeState, MultiTapeExpression>,
}

impl MultiTapeAutomata {
    pub fn new(
        state_eq_map: IndexMap<MultiTapeState, MultiTapeExpression>,
    ) -> Result<MultiTapeAutomata, AutomataError> {
        let prod_to_state_map = Self::reverse_state_eq_map(&state_eq_map, true)?;
        let (leftmost_extent, rightmost_extent) =
            Self::compute_rule_range(&prod_to_state_map);

        Ok(MultiTapeAutomata {
            multi_tape: BiDirectionalMultiTape::default(),
            prod_to_state_map,
            leftmost_extent,
            rightmost_extent,
            state_eq_map,
        })
    }

    /// Python `__getitem__`; `None` when the tape has not been allocated yet.
    pub fn get_tape(&self, tape_no: TapeNo) -> Option<&BidirectionalTape> {
        self.multi_tape.get_tape(tape_no)
    }

    pub fn get_tape_nos(&self) -> Vec<TapeNo> {
        self.multi_tape.get_tape_nos()
    }

    pub fn get_prod_to_state_map(&self) -> ProductWritesMap {
        self.prod_to_state_map.clone()
    }

    pub fn get_state_eq_map(&self) -> IndexMap<MultiTapeState, MultiTapeExpression> {
        self.state_eq_map.clone()
    }

    pub fn get_multi_tape(&self) -> &BiDirectionalMultiTape {
        &self.multi_tape
    }

    pub fn leftmost_extent(&self) -> i64 {
        self.leftmost_extent
    }

    pub fn rightmost_extent(&self) -> i64 {
        self.rightmost_extent
    }

    pub fn get_rule_range(&self) -> (i64, i64) {
        (self.leftmost_extent, self.rightmost_extent)
    }

    fn compute_rule_range(prod_to_state_map: &ProductWritesMap) -> (i64, i64) {
        let mut leftmost_extent: i64 = 0;
        let mut rightmost_extent: i64 = 0;

        for product in prod_to_state_map.products() {
            for term in product.to_flat_terms() {
                let offset = term.position;
                leftmost_extent = leftmost_extent.min(offset);
                rightmost_extent = rightmost_extent.max(offset);
            }
        }

        assert!(leftmost_extent <= 0);
        assert!(rightmost_extent >= 0);
        (leftmost_extent, rightmost_extent)
    }

    pub fn init_tapes(&mut self, tape_nos: &[TapeNo]) -> Result<(), AutomataError> {
        self.multi_tape.init_tapes(tape_nos, false)?;
        Ok(())
    }

    /// Populate the automata cells from `position` to `end_position`
    /// (inclusive) using `data` as a pattern.
    pub fn write_region(
        &mut self,
        position: i64,
        end_position: i64,
        data: &[MultiTapeState],
    ) -> Result<(), AutomataError> {
        self.multi_tape.write_region(position, end_position, data)?;
        Ok(())
    }

    /// `cell_width == None` is the `BLANK_INT` sentinel on the Python side.
    pub fn render_tapes(
        &self,
        start_position: i64,
        length: usize,
        cell_width: Option<usize>,
    ) -> Result<RenderFrame, AutomataError> {
        Ok(self.multi_tape.render_tapes(start_position, length, cell_width)?)
    }

    /// Given a mapping from output tape states to expressions over input tape
    /// states, create a mapping of tape state products to the `tape_no` and
    /// tape cell state they write to:
    ///
    /// `product -> tape_no -> output tape cell state`
    ///
    /// The reason this doesn't return `product -> MultiTapeState` is so that
    /// write collisions can be detected (given the same tape, a product should
    /// only ever write a single unique cell state, if at all).
    pub fn reverse_state_eq_map(
        state_eq_map: &IndexMap<MultiTapeState, MultiTapeExpression>,
        require_annotations: bool,
    ) -> Result<ProductWritesMap, AutomataError> {
        let mut prod_to_state_map = ProductWritesMap::new();

        for (multi_tape_output, expr) in state_eq_map.iter() {
            for product in expr._get_products() {
                /*
                Whether a product transitions a contiguous region of void
                states into a non-void state. This can't be allowed because
                it would make the simulation range infinite.
                */
                let product_is_void = product
                    .to_flat_terms()
                    .iter()
                    .all(|term| term.state.1 == VOID_STATE);

                if product_is_void {
                    return Err(AutomataError::VoidProduct {
                        product: product._to_string("D"),
                        output: *multi_tape_output,
                    });
                }

                let annotation = product.get_annotation();
                if require_annotations && annotation.is_empty() {
                    return Err(AutomataError::EmptyAnnotation {
                        product: product._to_string("D"),
                    });
                }

                prod_to_state_map.insert(product.copy(), *multi_tape_output)?;
            }
        }

        Ok(prod_to_state_map)
    }

    /// Check if the given product is satisfied at the given position
    /// on the tapes.
    pub fn product_satisfies(&self, product: &MultiTapeProduct, position: i64) -> bool {
        for term in product.to_flat_terms() {
            let term_offset = term.position;
            let (tape_no, tape_cell_state) = term.state;
            let term_position = position + term_offset;

            // a tape that has not been allocated reads as VOID_STATE
            // everywhere, matching `get_or_make_tape(...).read(...)`
            let read_state = self
                .multi_tape
                .get_tape(tape_no)
                .map(|tape| tape.read(term_position))
                .unwrap_or(VOID_STATE);

            if read_state != tape_cell_state {
                return false;
            }
        }
        true
    }

    pub fn process_step(
        &self, log_active_writes: bool,
    ) -> Result<ProcessStepResult, AutomataError> {
        // i.e., no void states filled in by default
        let existing_tape_nos = self.multi_tape.get_tape_nos();
        let (min_pos, max_pos) = self.multi_tape.get_range();
        let mut new_multi_tape = self.multi_tape.clone();
        let scan_start = min_pos + self.leftmost_extent;
        let scan_end = max_pos + self.rightmost_extent + 1;

        // record all (tape_no, position) -> tape_cell_state writes
        let mut writes_map: HashMap<(TapeNo, i64), CellState> = HashMap::new();
        let mut annotations_map: HashMap<(TapeNo, i64), BTreeSet<String>> =
            HashMap::new();
        let mut active_writes: Vec<WriteRecord> = Vec::new();

        for position in scan_start..scan_end {
            let mut written_tape_nos: BTreeSet<TapeNo> = BTreeSet::new();

            // apply all matching rules at this position to get new tape states
            for (matching_product, product_writes) in self.prod_to_state_map.iter() {
                if !self.product_satisfies(matching_product, position) {
                    continue;
                }

                let annotation = matching_product.get_annotation().to_string();

                for (tape_no, tape_cell_state) in product_writes.iter() {
                    let tape_no = *tape_no;
                    let tape_cell_state = *tape_cell_state;
                    let write_target: (TapeNo, i64) = (tape_no, position);
                    // previously recorded write to this tape cell, if any
                    let prev_write = writes_map
                        .get(&write_target)
                        .copied()
                        .unwrap_or(tape_cell_state);

                    if prev_write != tape_cell_state {
                        let prev_annotations = annotations_map
                            .get(&write_target)
                            .map(|annotations| {
                                annotations.iter().cloned().collect::<Vec<String>>()
                            })
                            .unwrap_or_default();

                        return Err(AutomataError::ConflictingWrite {
                            tape_no,
                            position,
                            product: matching_product._to_string("D"),
                            annotation,
                            previous: prev_write,
                            incoming: tape_cell_state,
                            previous_annotations: prev_annotations,
                        });
                    }

                    let write_record = WriteRecord {
                        origin_product: matching_product.copy(),
                        write_target,
                        tape_cell_state,
                        annotation: annotation.clone(),
                    };
                    if log_active_writes {
                        write_record.log();
                    }
                    active_writes.push(write_record);

                    writes_map.insert(write_target, tape_cell_state);
                    annotations_map
                        .entry(write_target)
                        .or_default()
                        .insert(annotation.clone());

                    let output_tape = new_multi_tape.get_or_make_tape(tape_no)?;
                    output_tape.write(position, tape_cell_state);
                    debug_assert_eq!(output_tape.read(position), tape_cell_state);
                    written_tape_nos.insert(tape_no);
                }
            }

            // copy over unchanged tape cells for tapes that
            // were not written to at this position
            for tape_no in existing_tape_nos.iter().copied() {
                if written_tape_nos.contains(&tape_no) {
                    continue;
                }

                let previous_tape_val: CellState = self
                    .multi_tape
                    .get_tape(tape_no)
                    .map(|tape| tape.read(position))
                    .unwrap_or(VOID_STATE);

                let new_tape = new_multi_tape.get_or_make_tape(tape_no)?;
                new_tape.write(position, previous_tape_val);
            }
        }

        Ok(ProcessStepResult {
            prev_multi_tape: self.multi_tape.clone(),
            new_multi_tape,
            active_writes,
        })
    }

    /// Set the new state of the multi-tape after going forward a single step.
    /// The returned result also carries the previous multi-tape state.
    pub fn step(&mut self, verbose: bool) -> Result<ProcessStepResult, AutomataError> {
        let process_result = self.process_step(verbose)?;
        self.multi_tape = process_result.new_multi_tape.clone();
        Ok(process_result)
    }
}
