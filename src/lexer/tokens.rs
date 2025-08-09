use std::fmt;
use std::fmt::{Debug, Display};
use regex::Regex;
use crate::lexer::base_token_builder::{BaseTokenBuilder, TokenBuilderStates};

pub fn is_word_boundary(c: char) -> bool {
    let word_char_regex = Regex::new(r"^\w").unwrap();
    !word_char_regex.is_match(&c.to_string())
}

pub(crate) trait HasLength {
    fn get_length(&self) -> usize;
}

#[derive(PartialEq, Copy, Clone, Debug, Eq)]
pub enum Keywords {
    Integer,
    Void,
    Return
}
impl Keywords {
    fn to_string(&self) -> String {
        match self {
            Keywords::Integer => "int".to_string(),
            Keywords::Void => "void".to_string(),
            Keywords::Return => "return".to_string(),
        }
    }
}
impl HasLength for Keywords {
    fn get_length(&self) -> usize {
        self.to_string().chars().count()
    }
}
impl fmt::Display for Keywords {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

#[derive(PartialEq, Copy, Clone, Debug, Eq)]
pub enum Punctuators {
    OpenParens,
    CloseParens,
    OpenBrace,
    CloseBrace,
    Semicolon
}
impl Punctuators {
    fn to_string(&self) -> String {
        match self {
            Punctuators::OpenParens => "(".parse().unwrap(),
            Punctuators::CloseParens => ")".parse().unwrap(),
            Punctuators::OpenBrace => "{".parse().unwrap(),
            Punctuators::CloseBrace => "}".parse().unwrap(),
            Punctuators::Semicolon => ";".parse().unwrap(),
        }
    }
}
impl fmt::Display for Punctuators {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}
impl HasLength for Punctuators {
    fn get_length(&self) -> usize {
        self.to_string().chars().count()
    }
}

#[derive(PartialEq, Clone, Debug, Eq)]
pub enum Tokens {
    Keyword(Keywords),
    Identifier(String),
    // e.g. "+", "-", "*", "/"
    Operator(Operators),
    // a bunch of digits, e.g. "1234"
    Constant(String),
    Punctuator(Punctuators),
    Comment(String),
}
impl Tokens {
    fn to_string(&self) -> String {
        match self {
            Tokens::Identifier(s) => s.to_string(),
            Tokens::Constant(s) => s.to_string(),
            Tokens::Operator(op) => op.to_string(),
            Tokens::Keyword(k) => k.to_string(),
            Tokens::Punctuator(p) => p.to_string(),
            Tokens::Comment(s) => s.to_string(),
        }
    }
}
impl HasLength for Tokens {
    fn get_length(&self) -> usize {
        self.to_string().chars().count()
    }
}
impl fmt::Display for Tokens {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Tokens::Identifier(s) => write!(f, "Identifier({})", s),
            Tokens::Constant(s) => write!(f, "Constant({})", s),
            Tokens::Operator(op) => write!(f, "Operator({})", op),
            Tokens::Keyword(k) => write!(f, "Keyword({})", k),
            Tokens::Punctuator(p) => write!(f, "Punctuator({})", p),
            Tokens::Comment(c) => write!(f, "Comment({})", c),
        }
    }
}

#[derive(PartialEq, Clone, Debug, Eq)]
pub struct SourceContext {
    pub source: String,
    pub start_position: usize,
    pub end_position: usize
}
impl SourceContext {
    pub fn new(source: String, start_position: usize, end_position: usize) -> Self {
        SourceContext {
            source,
            start_position,
            end_position
        }
    }
}
impl fmt::Display for SourceContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f, "SourceContext('{}' @ {}-{})",
            self.source, self.start_position, self.end_position
        )
    }
}

#[derive(PartialEq, Clone, Debug, Eq)]
pub struct WrappedToken {
    pub token: Tokens,
    context: SourceContext,
}
impl WrappedToken {
    pub fn new(token: Tokens, context: SourceContext) -> Self {
        WrappedToken { token, context }
    }
    pub fn get_max_position(&self) -> usize {
        // returns the maximum position of the token
        self.context.end_position
    }
    pub fn get_min_position(&self) -> usize {
        // returns the minimum position of the token
        self.context.start_position
    }
}
impl fmt::Display for WrappedToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "WrappedToken({}, {})", self.token, self.context)
    }
}



#[derive(PartialEq, Copy, Clone, Debug, Eq)]
pub struct ProcessResult {
    pub(crate) complete: bool, // whether the token is well-formed in its current state
    pub(crate) accepting: bool, // whether token is still accepting new characters
    pub(crate) accept_char: bool, // whether to add current character
}
impl ProcessResult {
    pub(crate) fn new(complete: bool, accepting: bool, accept_char: bool) -> Self {
        ProcessResult { complete, accepting, accept_char }
    }
    pub(crate) fn add_and_continue(complete: bool) -> ProcessResult {
        ProcessResult::new(complete, true, true)
    }
    pub(crate) fn complete_without_continue(accept_char: bool) -> ProcessResult {
        ProcessResult::new(true, false, accept_char)
    }
    fn add_then_end(complete: bool) -> ProcessResult {
        ProcessResult::new(complete, false, true)
    }
    pub(crate) fn reject() -> ProcessResult {
        ProcessResult::new(false, false, false)
    }
}

pub trait TokenBuilder: Display {
    fn base(&self) -> &BaseTokenBuilder;
    fn base_mut(&mut self) -> &mut BaseTokenBuilder;

    fn get_state(&self) -> TokenBuilderStates {
        self.base().get_state()
    }

    fn _get_built_str(&self) -> &String {
        self.base()._get_built_str()
    }

    fn is_done(&self) -> bool {
        self.get_state() == TokenBuilderStates::Done
    }

    fn get_length(&self) -> usize {
        self._get_built_str().chars().count()
    }

    fn is_valid(&self) -> bool {
        let state = self.get_state();
        (state == TokenBuilderStates::Accepting) || (state == TokenBuilderStates::Done)
    }

    fn is_accepting(&self) -> bool {
        let state = self.get_state();
        (state == TokenBuilderStates::Start) || (state == TokenBuilderStates::Accepting)
    }

    fn set_done(&mut self) {
        self.base_mut().set_done();
    }

    fn set_state(&mut self, state: TokenBuilderStates) {
        self.base_mut().set_state(state)
    }

    fn _push_char(&mut self, char: char) {
        self.base_mut()._push_char(char);
    }

    /*
    returns complete, accepting
    complete - whether the token builder token is complete
    accepting - whether the token builder is accepting new characters
    */
    fn process_char(&self, char: char) -> ProcessResult;

    fn add_char(&mut self, c: char) -> bool {
        // returns whether builder is still accepting characters
        if !self.is_accepting() {
            return false
        }
        let process_res = self.process_char(c);
        if process_res.accept_char {
            self._push_char(c);
        }

        if process_res.accepting {
            self.set_state(TokenBuilderStates::Accepting);
        } else if !process_res.complete {
            self.set_state(TokenBuilderStates::Invalid);
        } else {
            self.set_state(TokenBuilderStates::Done);
        }

        process_res.accepting
    }

    fn add_characters(&mut self, chars: String) -> bool {
        for c in chars.chars() {
            let accepting = self.add_char(c);
            if !accepting { return false }
        }
        self.is_done()
    }

    fn build(&self) -> Option<String> {
        if self.is_done() {
            Some(self._get_built_str().clone())
        } else {
            None
        }
    }

    fn build_token(&self) -> Option<Tokens>;
}

#[derive(PartialEq, Copy, Clone, Debug, Eq)]
pub enum Operators {
    Decrement,
    Minus,
    BitwiseNot,
}
impl Operators {
    pub fn to_string(&self) -> String {
        match self {
            Operators::Minus => "-".to_string(),
            Operators::BitwiseNot => "~".to_string(),
            Operators::Decrement => "--".to_string(),
        }
    }
}
impl fmt::Display for Operators {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}
impl HasLength for Operators {
    fn get_length(&self) -> usize {
        self.to_string().chars().count()
    }
}
