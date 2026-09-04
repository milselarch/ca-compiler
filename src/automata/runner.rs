use std::fmt;
use std::ops::Index;

use indexmap::IndexSet;
use crate::automata::render_frame_utils::RenderError;
use crate::automata::rule_generator::{TapeCellState, TapeNo};

/// Static so that `Index<i64>` can hand out a reference for out-of-range cells.


#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TapeError {
    /// Requested cell width cannot fit the largest state present on the tape.
    CellWidthTooSmall {
        cell_width: usize,
        max_state: TapeCellState,
    },
    /// `write_region` was called with an empty pattern.
    EmptyWritePattern,
    /// Tried to allocate a new tape after the tape set was frozen.
    TapesFrozen { tape_no: TapeNo },
    /// Frames produced during rendering could not be laid out.
    Render(RenderError),
}
impl From<RenderError> for TapeError {
    fn from(err: RenderError) -> Self {
        TapeError::Render(err)
    }
}

impl fmt::Display for TapeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TapeError::CellWidthTooSmall {
                cell_width, max_state
            } => write!(
                f,
                "Cell width {} is too small to fit the largest state {}",
                cell_width, max_state
            ),
            TapeError::EmptyWritePattern => {
                write!(f, "write_region requires a non-empty pattern")
            }
        }
    }
}
impl std::error::Error for TapeError {}

#[derive(Debug, Clone, Default, Eq, PartialEq)]
pub struct RenderFrame {
    pub lines: Vec<String>,
}
impl TapeRenderFrame {
    pub fn new(lines: Vec<String>) -> Option<RenderFrame> {
        let first_len = lines.first().map(|line| line.chars().count());
        if let Some(len) = first_len {
            if lines.iter().any(|line| line.chars().count() != len) {
                return None;
            }
        }
        Some(RenderFrame { lines })
    }
    /// Pads every line on the right so that all of them share the widest length.
    pub fn from_padded_lines(lines: Vec<String>) -> RenderFrame {
        let width = lines.iter().map(|line| line.chars().count()).max().unwrap_or(0);
        let padded = lines
            .into_iter()
            .map(|line| {
                let pad = width - line.chars().count();
                line + &" ".repeat(pad)
            })
            .collect();
        RenderFrame { lines: padded }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PruneResult {
    pub forward_popped: usize,
    pub reverse_popped: usize,
}

#[derive(Debug, Clone, Default)]
pub struct BidirectionalTape {
    /// automata cell states from position 0 and higher
    /// note that position increases for cells as we go rightwards in data
    pub(crate) data: Vec<TapeCellState>,
    /// automata cell states from position -1 and lower
    /// note that position decreases for cells as we go rightwards in rev_data
    pub(crate) rev_data: Vec<TapeCellState>,
}
impl BidirectionalTape {
    pub fn new(data: Vec<TapeCellState>) -> BidirectionalTape {
        BidirectionalTape { data, rev_data: vec![] }
    }

    /// Inclusive range of positions for which cell data is currently allocated.
    /// Note that `max_pos` is `-1` when the rightwards half is empty.
    pub fn get_range(&self) -> (i64, i64) {
        let min_pos = -(self.rev_data.len() as i64);
        let max_pos = self.data.len() as i64 - 1;
        (min_pos, max_pos)
    }

    // TODO: consider tracking unique states instead of recomputing
    pub fn get_all_states(&self) -> IndexSet<TapeCellState> {
        self.data.iter().chain(self.rev_data.iter()).copied().collect()
    }

    pub fn max_state(&self) -> TapeCellState {
        self.data
            .iter()
            .chain(self.rev_data.iter())
            .copied()
            .max()
            .unwrap_or(VOID_STATE)
    }

    /// Prune trailing void cells in both directions.
    pub fn prune(&mut self) -> PruneResult {
        let mut forward_popped = 0;
        let mut reverse_popped = 0;

        while self.data.last() == Some(&VOID_STATE) {
            forward_popped += 1;
            self.data.pop();
        }
        while self.rev_data.last() == Some(&VOID_STATE) {
            reverse_popped += 1;
            self.rev_data.pop();
        }

        PruneResult { forward_popped, reverse_popped }
    }

    /// Get the minimal contiguous region of tape data
    /// that contains all non-void states.
    pub fn get_minimal_data_region(&mut self) -> Vec<TapeCellState> {
        self.prune();

        let data_region = self
            .rev_data
            .iter()
            .rev()
            .chain(self.data.iter())
            .copied();

        let mut minimal_data_region: Vec<TapeCellState> = Vec::new();
        let mut data_region_started = false;

        for tape_cell_state in data_region {
            if tape_cell_state != VOID_STATE {
                data_region_started = true;
            }
            if !data_region_started {
                continue;
            }
            minimal_data_region.push(tape_cell_state);
        }

        // remove trailing void state cells
        // this can happen if all data cells are from the rev_data region
        while minimal_data_region.last() == Some(&VOID_STATE) {
            minimal_data_region.pop();
        }

        minimal_data_region
    }

    /// `cell_width == None` means "derive the width from the largest state"
    /// (equivalent to the `BLANK_INT` sentinel on the Python side).
    pub fn render_line(
        &self,
        start_position: i64,
        length: usize,
        cell_width: Option<usize>,
    ) -> Result<TapeRenderFrame, TapeError> {
        let max_state = self.max_state();
        let min_cell_width = max_state.to_string().len();

        let cell_width = match cell_width {
            None => min_cell_width,
            Some(width) if width < min_cell_width => {
                return Err(TapeError::CellWidthTooSmall { cell_width: width, max_state });
            }
            Some(width) => width,
        };

        let cells_to_render = length / (cell_width + 1);
        let mut line = String::with_capacity(length);

        for k in 0..cells_to_render {
            let position = start_position + k as i64;
            let state = self.read(position);
            line.push_str(&format!("{:0width$}", state, width = cell_width));
            line.push('|');
        }

        // pad out to the requested length
        line.push_str(&" ".repeat(length.saturating_sub(line.len())));

        Ok(TapeRenderFrame::new(line, cells_to_render, cell_width))
    }

    pub fn read(&self, position: i64) -> TapeCellState {
        if position >= 0 {
            let index = position as usize;
            self.data.get(index).copied().unwrap_or(VOID_STATE)
        } else {
            let rev_index = (-position - 1) as usize;
            self.rev_data.get(rev_index).copied().unwrap_or(VOID_STATE)
        }
    }

        pub fn from_line(line: String) -> RenderFrame {
        RenderFrame { lines: vec![line] }
    }
    pub fn get_lines(&self) -> &[String] {
        &self.lines
    }
    pub fn get_width(&self) -> usize {
        self.lines.first().map(|line| line.chars().count()).unwrap_or(0)
    }
    pub fn get_height(&self) -> usize {
        self.lines.len()
    }
    pub fn get_dimensions(&self) -> (usize, usize) {
        (self.get_height(), self.get_width())
    }
    pub fn render(&self) -> String {
        self.lines.join("\n")
    }

    /// Appends `other`'s lines below our own. Widths must match
    /// unless the current frame is still empty.
    pub fn extend_down(&mut self, other: &RenderFrame) -> Result<&mut Self, RenderError> {
        if self.get_height() == 0 {
            self.lines = other.lines.clone();
            return Ok(self);
        }
        if self.get_width() != other.get_width() {
            return Err(RenderError::WidthMismatch {
                own: self.get_width(),
                other: other.get_width(),
            });
        }
        self.lines.extend(other.lines.iter().cloned());
        Ok(self)
    }
        if position >= 0 {
            let index = position as usize;
            if index >= self.data.len() {
                self.data.resize(index + 1, VOID_STATE);
            }
            self.data[index] = value;
        } else {
            let rev_index = (-position - 1) as usize;
            if rev_index >= self.rev_data.len() {
                self.rev_data.resize(rev_index + 1, VOID_STATE);
            }
            self.rev_data[rev_index] = value;
        }
    }

    /// Populate the cells from `position` to `end_position` (inclusive)
    /// using `values` as a repeating pattern.
    pub fn write_region(
        &mut self,
        position: i64,
        end_position: i64,
        values: &[TapeCellState],
    ) -> Result<(), TapeError> {
        if values.is_empty() {
            return Err(TapeError::EmptyWritePattern);
        }

        for new_position in position..=end_position {
            let offset = (new_position - position) as usize;
            let value = values[offset % values.len()];
            self.write(new_position, value);
        }
        Ok(())
    }
}
impl Index<i64> for BidirectionalTape {
    type Output = TapeCellState;

    fn index(&self, position: i64) -> &Self::Output {
        if position >= 0 {
            self.data.get(position as usize).unwrap_or(&VOID_STATE_CELL)
        } else {
            self.rev_data
                .get((-position - 1) as usize)
                .unwrap_or(&VOID_STATE_CELL)
        }
    }
}