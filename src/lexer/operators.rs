use std::fmt::{Display, Formatter};
use crate::lexer::base_token_builder::BaseTokenBuilder;
use crate::lexer::lexer::{ProcessResult, TokenBuilder, Tokens};
use crate::lexer::tokens::{Operators};


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
        write!(f, "OperatorProcessor")
    }
}

impl TokenBuilder for OperatorProcessor {
    fn base(&self) -> &BaseTokenBuilder { &self.base }
    fn base_mut(&mut self) -> &mut BaseTokenBuilder { &mut self.base }

    fn process_char(&self, c: char) -> ProcessResult {
        let pattern_length = self.pattern.len();
        let chars_collected = self.base.get_length();
        if chars_collected >= pattern_length {
            return ProcessResult::complete_without_continue(false)
        }
        if c != self.pattern.chars().nth(chars_collected).unwrap() {
            // character does not match operator at position
            return ProcessResult::reject()
        }

        let new_length = chars_collected + 1;
        let complete = new_length == pattern_length;
        let accepting = !complete;
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
            // Create a OperatorProcessor for each pattern
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
            operators: OperatorsBuilder::create_processors(vec![
                ("-", Operators::Minus),
                ("~", Operators::BitwiseNot),
                ("--", Operators::Decrement)
            ])
        }
    }
}