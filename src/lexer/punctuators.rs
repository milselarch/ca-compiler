use std::fmt::{Display, Formatter};
use crate::lexer::base_token_builder::BaseTokenBuilder;
use crate::lexer::lexer::{ProcessResult, Tokens};
use crate::lexer::tokens::{Punctuators, TokenBuilder};

pub struct PunctuatorProcessor {
    // things like "(", ")", "{", "}", ";"
    pattern: String,
    punctuator: Punctuators,
    base: BaseTokenBuilder,
}
impl PunctuatorProcessor {
    pub(crate) fn new(pattern: String, punctuator: Punctuators) -> PunctuatorProcessor {
        PunctuatorProcessor {
            pattern, punctuator, base: BaseTokenBuilder::new()
        }
    }
    pub fn is_done(&self) -> bool {
        self.pattern.len() == self._get_built_str().len()
    }
    pub fn get_punctuator(&self) -> Punctuators {
        self.punctuator
    }
}

impl Display for PunctuatorProcessor {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "PunctuatorProcessor")
    }
}

impl crate::lexer::lexer::TokenBuilder for PunctuatorProcessor {
    fn base(&self) -> &BaseTokenBuilder { &self.base }
    fn base_mut(&mut self) -> &mut BaseTokenBuilder { &mut self.base }

    fn process_char(&self, c: char) -> ProcessResult {
        let pattern_length = self.pattern.len();
        let chars_collected = self.base.get_length();
        if chars_collected >= pattern_length {
            // println!("SITUATION_1");
            return ProcessResult::complete_without_continue(false)
        }
        if c != self.pattern.chars().nth(chars_collected).unwrap() {
            // character does not match punctuator
            // println!("SITUATION_2");
            return ProcessResult::reject()
        }

        let new_length = chars_collected + 1;
        let complete = new_length == pattern_length;
        let accepting = !complete;
        // println!("SITUATION_3");
        ProcessResult::new(complete, accepting, true)
    }

    fn build_token(&self) -> Option<Tokens> {
        if self.is_done() {
            Some(Tokens::Punctuator(self.punctuator))
        } else {
            None
        }
    }
}

pub struct PunctuatorsBuilder {
    pub(crate) base: BaseTokenBuilder,
    pub(crate) punctuators: Vec<PunctuatorProcessor>,
}
impl PunctuatorsBuilder {
    fn create_processors(
        punctuator_patterns: Vec<(&str, Punctuators)>
    ) -> Vec<PunctuatorProcessor> {
        let mut processors = Vec::new();
        for raw_punctuator in punctuator_patterns {
            // Create a PunctuatorProcessor for each pattern
            let processor = PunctuatorProcessor::new(
                raw_punctuator.0.parse().unwrap(), raw_punctuator.1
            );
            processors.push(processor);
        }
        processors
    }

    pub(crate) fn new() -> PunctuatorsBuilder {
        PunctuatorsBuilder {
            base: BaseTokenBuilder::new(),
            punctuators: PunctuatorsBuilder::create_processors(vec![
                ("(", Punctuators::OpenParens),
                (")", Punctuators::CloseParens),
                ("{", Punctuators::OpenBrace),
                ("}", Punctuators::CloseBrace),
                (";", Punctuators::Semicolon),
            ])
        }
    }
}