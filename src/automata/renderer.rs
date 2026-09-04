use std::fmt;
use std::ops::{Deref, DerefMut};

#[derive(Debug, Clone, Default, Eq, PartialEq)]
pub struct RenderFrame {
    lines: Vec<String>,
}
impl RenderFrame {
    /// All lines must have the same length, otherwise `None` is returned.
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

    /// Appends `other`'s lines to the right of our own. Heights must match
    /// unless the current frame is still empty.
    pub fn extend_right(&mut self, other: &RenderFrame) -> Result<&mut Self, RenderError> {
        if self.get_height() == 0 {
            self.lines = other.lines.clone();
            return Ok(self);
        }
        if self.get_height() != other.get_height() {
            return Err(RenderError::HeightMismatch {
                own: self.get_height(),
                other: other.get_height(),
            });
        }
        for (own_line, other_line) in self.lines.iter_mut().zip(other.lines.iter()) {
            own_line.push_str(other_line);
        }
        Ok(self)
    }

    pub fn join_vertically(frames: &[RenderFrame]) -> Result<RenderFrame, RenderError> {
        let mut combined = RenderFrame::default();
        for frame in frames {
            combined.extend_down(frame)?;
        }
        Ok(combined)
    }

    pub fn join_horizontally(frames: &[RenderFrame]) -> Result<RenderFrame, RenderError> {
        let mut combined = RenderFrame::default();
        for frame in frames {
            combined.extend_right(frame)?;
        }
        Ok(combined)
    }
}

/// Equivalent of the `TapeRenderFrame` subclass.
/// Rust has no inheritance, so this wraps a `RenderFrame` and exposes its
/// methods through `Deref`/`DerefMut`.
pub struct TapeRenderFrame {
    frame: RenderFrame,
    pub(crate) num_cells: usize,
    cell_width: usize,
}

impl TapeRenderFrame {
    pub fn new(line: &str, num_cells: usize, cell_width: usize) -> Self {
        TapeRenderFrame {
            frame: RenderFrame::from_line(line.parse().unwrap()),
            num_cells,
            cell_width,
        }
    }

    pub fn get_space_consumed(&self) -> usize {
        self.num_cells * (self.cell_width + 1)
    }
}

impl Deref for TapeRenderFrame {
    type Target = RenderFrame;

    fn deref(&self) -> &RenderFrame {
        &self.frame
    }
}

impl DerefMut for TapeRenderFrame {
    fn deref_mut(&mut self) -> &mut RenderFrame {
        &mut self.frame
    }
}

impl fmt::Debug for TapeRenderFrame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "TapeRenderFrame(lines={:?}, num_cells={}, cell_width={})",
            self.frame.lines, self.num_cells, self.cell_width
        )
    }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub enum RenderError {
    WidthMismatch { own: usize, other: usize },
    HeightMismatch { own: usize, other: usize },
}
impl std::fmt::Display for RenderError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RenderError::WidthMismatch { own, other } => {
                write!(f, "Frame widths must match: {} vs {}", own, other)
            }
            RenderError::HeightMismatch { own, other } => {
                write!(f, "Frame heights must match: {} vs {}", own, other)
            }
        }
    }
}
impl std::error::Error for RenderError {}
