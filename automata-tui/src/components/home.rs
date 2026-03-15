use color_eyre::Result;
use indexmap::IndexMap;
use ratatui::{prelude::*, widgets::*};
use tokio::sync::mpsc::UnboundedSender;

use super::Component;
use crate::{action::Action, config::Config};
use py_ca_compiler::automata::composer::{MultiTape, Tape};

const NAMED_COLORS: [Color; 16] = [
    Color::Black,
    Color::Red,
    Color::Green,
    Color::Yellow,
    Color::Blue,
    Color::Magenta,
    Color::Cyan,
    Color::Gray,
    Color::DarkGray,
    Color::LightRed,
    Color::LightGreen,
    Color::LightYellow,
    Color::LightBlue,
    Color::LightMagenta,
    Color::LightCyan,
    Color::White,
];

#[derive(Default)]
pub struct Home {
    command_tx: Option<UnboundedSender<Action>>,
    config: Config,
    position: i64,
    multi_tape_state: MultiTape,
}

impl Home {
    pub fn new() -> Self {
        let mut new_instance = Self::default();
        let new_tape = Tape::default();
        new_instance.multi_tape_state.insert_named_tape(
            "tape0".to_string(),
            new_tape
        ).unwrap();
        new_instance
    }

}

impl Component for Home {
    fn register_action_handler(&mut self, tx: UnboundedSender<Action>) -> Result<()> {
        self.command_tx = Some(tx);
        Ok(())
    }

    fn register_config_handler(&mut self, config: Config) -> Result<()> {
        self.config = config;
        Ok(())
    }

    fn update(&mut self, action: Action) -> Result<Option<Action>> {
        match action {
            Action::Tick => {
                // add any logic here that should run on every tick
            }
            Action::Render => {
                // add any logic here that should run on every render
            }
            _ => {}
        }
        Ok(None)
    }

    fn draw(&mut self, frame: &mut Frame, area: Rect) -> Result<()> {
        let width = frame.area().width;
        let tape_key_to_names_map = self.multi_tape_state.invert_tape_names_map();
        let tapes = self.multi_tape_state.get_tapes();

        for (index, tape) in tapes.iter().enumerate() {
            let tape_key = tape.get_tape_key();
            let tape_name = tape_key_to_names_map.get(&tape_key).unwrap();

            let area = Rect::new(0, index as u16, width, 1);
            let tape_contents = tape.get_contents();
            let tape_str = format!("{}: {}", tape_name, tape_contents);
            frame.render_widget(Paragraph::new(tape_str), area);
        }


        frame.render_widget(Paragraph::new("hello world"), area);
        for i in 0..10 {
            let area = Rect::new(0, i, frame.area().width, 1);
            frame.render_widget(Paragraph::new("Hello world!"), area);
        }
        Ok(())
    }
}
