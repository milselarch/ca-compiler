use std::fmt;
use std::fmt::{Display, Formatter};
use crate::lexer::base_token_builder::BaseTokenBuilder;
use crate::lexer::lexer::{ProcessResult, TokenBuilder, Tokens};
use crate::lexer::tokens::{Operators};


#[derive(Debug)]
pub struct OperatorProcessor {
    // things like "~", "-", "--"
    pattern: String,
    operator: Operators,
    base: BaseTokenBuilder,
}
impl OperatorProcessor {
    pub(crate) fn new(pattern: String, operator: Operators) -> OperatorProcessor {
        OperatorProcessor {
            pattern, operator, base: BaseTokenBuilder::new()
        }
    }
    pub fn is_done(&self) -> bool {
        self.pattern.len() == self._get_built_str().len()
    }
    pub fn get_operator(&self) -> Operators {
        self.operator
    }
}

impl Display for OperatorProcessor {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let output = format!(
            "OperatorProcessor(pattern: '{}', operator: {:?})",
            self.pattern, self.operator
        );
        write!(f, "{}", output)
    }
}

impl TokenBuilder for OperatorProcessor {
    fn base(&self) -> &BaseTokenBuilder { &self.base }
    fn base_mut(&mut self) -> &mut BaseTokenBuilder { &mut self.base }

    fn process_char(&self, c: char) -> ProcessResult {
        // println!("{}", format!("Processing char: '{}'", c));
        let pattern_length = self.pattern.len();
        let chars_collected = self.base.get_length();

        if chars_collected >= pattern_length {
            // println!("COMPLETE_NO_CONT");
            return ProcessResult::complete_without_continue(false)
        }
        if c != self.pattern.chars().nth(chars_collected).unwrap() {
            // println!("REJECT");
            // character does not match operator at position
            return ProcessResult::reject()
        }

        let new_length = chars_collected + 1;
        let complete = new_length == pattern_length;
        let accepting = !complete;
        // println!("OP {self} > COMPLETE: {}, ACCEPTING: {}", complete, accepting);
        ProcessResult::new(complete, accepting, true)
    }

    fn build_token(&self) -> Option<Tokens> {
        if self.is_done() {
            Some(Tokens::Operator(self.operator))
        } else {
            None
        }
    }
}

pub struct OperatorsBuilder {
    pub(crate) base: BaseTokenBuilder,
    pub(crate) operators: Vec<OperatorProcessor>,
}
impl OperatorsBuilder {
    fn create_processors(
        operator_patterns: Vec<(&str, Operators)>
    ) -> Vec<OperatorProcessor> {
        let mut processors = Vec::new();
        for raw_operator in operator_patterns {
            // Create an OperatorProcessor for each pattern
            let processor = OperatorProcessor::new(
                raw_operator.0.parse().unwrap(), raw_operator.1
            );
            processors.push(processor);
        }
        processors
    }

    pub(crate) fn new() -> OperatorsBuilder {
        OperatorsBuilder {
            base: BaseTokenBuilder::new(),
            // TODO: use enum iterators macro instead of hardcoding
            operators: OperatorsBuilder::create_processors(vec![
                ("-", Operators::Minus),
                ("~", Operators::BitwiseNot),
                ("--", Operators::Decrement)
            ])
        }
    }
}

impl Display for OperatorsBuilder {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "OperatorsBuilder {:?}", self.operators)
    }
}

impl TokenBuilder for OperatorsBuilder {
    fn base(&self) -> &BaseTokenBuilder { &self.base }
    fn base_mut(&mut self) -> &mut BaseTokenBuilder { &mut self.base }

    fn _push_char(&mut self, char: char) {
        // println!("PUSH_CHAR {}", char);
        self.base_mut()._push_char(char);

        for processor in &mut self.operators {
            let process_result = processor.process_char(char);
            if process_result.accept_char {
                // println!("OP_PUSH {:?} {:?}", processor, process_result);
                // println!("PROC_PUSH {}", process_result.accepting);
                processor._push_char(char);
            }
        }
    }

    fn process_char(&self, c: char) -> ProcessResult {
        // println!("\nPROCESS_CHAR {}", c);
        let mut complete = false;
        let mut accepting = false;
        let mut accept_char = false;

        for processor in &self.operators {
            let process_result = processor.process_char(c);
            complete = complete || process_result.complete;
            // println!("PROCESS_RESULT: {:?} {}", process_result, complete);
            accepting = accepting || process_result.accepting;
            accept_char = accept_char || process_result.accept_char;
        }

        // println!("COM: {}, ACC: {}, ACH: {}", complete, accepting, accept_char);
        ProcessResult::new(complete, accepting, accept_char)
    }
    fn build_token(&self) -> Option<Tokens> {
        // println!("BUILT_STR {}", self._get_built_str());
        for processor in &self.operators {
            if processor.is_done() {
                return Some(Tokens::Operator(processor.get_operator()));
            }
        }
        None
    }
}