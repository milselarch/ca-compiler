use std::fmt;
use std::fmt::{Display, Formatter};
use crate::lexer::base_token_builder::BaseTokenBuilder;
use crate::lexer::lexer::{ProcessResult, Tokens};
use crate::lexer::tokens::{Punctuators, TokenBuilder};

#[derive(Debug)]
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

impl TokenBuilder for PunctuatorProcessor {
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
            // TODO: use enum iterators macro instead of hardcoding
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

impl Display for PunctuatorsBuilder {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "PunctuatorsBuilder")
    }
}

impl TokenBuilder for PunctuatorsBuilder {
    fn base(&self) -> &BaseTokenBuilder { &self.base }
    fn base_mut(&mut self) -> &mut BaseTokenBuilder { &mut self.base }

    fn _push_char(&mut self, char: char) {
        // println!("PUSH_CHAR {}", char);
        self.base_mut()._push_char(char);

        for processor in &mut self.punctuators {
            let process_result = processor.process_char(char);
            if process_result.complete || process_result.accepting {
                // println!("PROC_PUSH {}", process_result.accepting);
                processor._push_char(char);
            }
        }
    }

    fn process_char(&self, c: char) -> ProcessResult {
        let mut complete = false;
        let mut accepting = false;

        for processor in &self.punctuators {
            let process_result = processor.process_char(c);
            complete = complete || process_result.complete;
            accepting = accepting || process_result.accepting;
        }

        ProcessResult::new(complete, accepting, true)
    }
    fn build_token(&self) -> Option<Tokens> {
        // println!("BUILT_STR {}", self._get_built_str());
        for processor in &self.punctuators {
            if processor.is_done() {
                return Some(Tokens::Punctuator(processor.get_punctuator()));
            }
        }
        None
    }
}
