use std::{error::Error, fmt};
use std::fmt::Display;
use std::fs::File;
use std::io::Read;
use regex::Regex;

use crate::lexer::base_token_builder::{BaseTokenBuilder, TokenBuilderStates};
use crate::parser::parser::ParseError;

trait HasLength {
    fn get_length(&self) -> usize;
}

#[derive(PartialEq, Copy, Clone)]
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

#[derive(PartialEq, Copy, Clone)]
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

#[derive(PartialEq, Clone)]
pub enum Tokens {
    Keyword(Keywords),
    Identifier(String),
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
            Tokens::Keyword(k) => write!(f, "Keyword({})", k),
            Tokens::Punctuator(p) => write!(f, "Punctuator({})", p),
            Tokens::Comment(c) => write!(f, "Comment({})", c),
        }
    }
}

#[derive(PartialEq, Copy, Clone, Debug, Eq)]
pub struct ProcessResult {
    complete: bool, // whether token is complete
    accepting: bool, // whether token is still accepting new characters
    accept_char: bool, // whether to add current character
}
impl ProcessResult {
    fn new(complete: bool, accepting: bool, accept_char: bool) -> Self {
        ProcessResult { complete, accepting, accept_char }
    }
    fn add_and_continue(complete: bool) -> ProcessResult {
        ProcessResult::new(complete, true, true)
    }
    fn complete_without_continue(accept_char: bool) -> ProcessResult {
        ProcessResult::new(true, false, accept_char)
    }
    fn add_then_end(complete: bool) -> ProcessResult {
        ProcessResult::new(complete, false, true)
    }
    fn reject() -> ProcessResult {
        ProcessResult::new(false, false, false)
    }
}

trait TokenBuilder {
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

fn is_word_boundary(c: char) -> bool {
    let word_char_regex = Regex::new(r"^\w").unwrap();
    !word_char_regex.is_match(&c.to_string())
}

struct IdentifierBuilder {
    base: BaseTokenBuilder,
    base_re: Regex,
    main_re: Regex
}
impl IdentifierBuilder {
    fn new() -> IdentifierBuilder {
        IdentifierBuilder {
            base: BaseTokenBuilder::new(),
            base_re: Regex::new(r"^[a-zA-Z_]").unwrap(),
            main_re: Regex::new(r"^\w").unwrap()
        }
    }
}
impl TokenBuilder for IdentifierBuilder {
    fn base(&self) -> &BaseTokenBuilder { &self.base }
    fn base_mut(&mut self) -> &mut BaseTokenBuilder { &mut self.base }
    fn process_char(&self, c: char) -> ProcessResult {
        let length = self.get_length();
        let new_char_valid = match length {
            0 => self.base_re.is_match(&c.to_string()),
            _ => self.main_re.is_match(&c.to_string())
        };

        let is_valid_identifier = (length > 0) || new_char_valid;

        if !new_char_valid {
            if is_word_boundary(c) {
                ProcessResult::new(
                    is_valid_identifier, false, false
                )
            } else {
                ProcessResult::reject()
            }
        } else {
            ProcessResult::add_and_continue(is_valid_identifier)
        }
    }
    fn build_token(&self) -> Option<Tokens> {
        if self.is_done() {
            let identifier = self._get_built_str().clone();
            match identifier.as_str() {
                "int" => Some(Tokens::Keyword(Keywords::Integer)),
                "void" => Some(Tokens::Keyword(Keywords::Void)),
                "return" => Some(Tokens::Keyword(Keywords::Return)),
                _ => Some(Tokens::Identifier(identifier)),
            }
        } else {
            None
        }
    }
}

struct ConstantBuilder {
    base: BaseTokenBuilder,
}
impl ConstantBuilder {
    fn new() -> ConstantBuilder {
        ConstantBuilder {
            base: BaseTokenBuilder::new(),
        }
    }
}
impl TokenBuilder for ConstantBuilder {
    fn base(&self) -> &BaseTokenBuilder { &self.base }
    fn base_mut(&mut self) -> &mut BaseTokenBuilder { &mut self.base }
    fn process_char(&self, c: char) -> ProcessResult {
        // println!("CONST_PROCESS_CHAR {}", c);
        let length = self.get_length();

        if c.is_digit(10) {
            return ProcessResult::add_and_continue(true)
        } else if length == 0 {
            return ProcessResult::reject()
        } else if is_word_boundary(c) {
            return ProcessResult::complete_without_continue(false)
        }
        // println!("C4");
        ProcessResult::reject()
    }

    fn build_token(&self) -> Option<Tokens> {
        if self.is_done() {
            let constant = self._get_built_str().clone();
            Some(Tokens::Constant(constant))
        } else {
            None
        }
    }
}

struct PunctuatorProcessor {
    // things like "(", ")", "{", "}", ";"
    pattern: String,
    punctuator: Punctuators,
    base: BaseTokenBuilder,
}
impl PunctuatorProcessor {
    fn new(pattern: String, punctuator: Punctuators) -> PunctuatorProcessor {
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

struct PunctuatorsBuilder {
    base: BaseTokenBuilder,
    punctuators: Vec<PunctuatorProcessor>,
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

    fn new() -> PunctuatorsBuilder {
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

struct SingleLineCommentBuilder {
    base: BaseTokenBuilder,
}
impl SingleLineCommentBuilder {
    fn new() -> SingleLineCommentBuilder {
        SingleLineCommentBuilder {
            base: BaseTokenBuilder::new(),
        }
    }
}
impl TokenBuilder for SingleLineCommentBuilder {
    fn base(&self) -> &BaseTokenBuilder { &self.base }
    fn base_mut(&mut self) -> &mut BaseTokenBuilder { &mut self.base }
    fn process_char(&self, c: char) -> ProcessResult {
        let length = self.get_length();
        let new_char_valid = match length {
            0 => c == '/',
            1 => c == '/',
            _ => c != '\n'
        };
        let complete = length >= 2;
        ProcessResult::new(complete, new_char_valid, new_char_valid)
    }

    fn build_token(&self) -> Option<Tokens> {
        if self.is_done() {
            let comment = self._get_built_str().clone();
            Some(Tokens::Comment(comment))
        } else {
            None
        }
    }
}

struct MultiLineCommentBuilder {
    base: BaseTokenBuilder,
}
impl MultiLineCommentBuilder {
    fn new() -> MultiLineCommentBuilder {
        MultiLineCommentBuilder {
            base: BaseTokenBuilder::new(),
        }
    }
}
impl TokenBuilder for MultiLineCommentBuilder {
    fn base(&self) -> &BaseTokenBuilder { &self.base }
    fn base_mut(&mut self) -> &mut BaseTokenBuilder { &mut self.base }
    fn process_char(&self, c: char) -> ProcessResult {
        let length = self.get_length();
        let current_str = self._get_built_str();
        let new_char_valid;
        let complete;
        let accepting;

        if length == 0 {
            complete = false;
            new_char_valid = c == '/';
            accepting = new_char_valid;
        } else if length == 1 {
            new_char_valid = c == '*';
            accepting = new_char_valid;
            complete = new_char_valid;
        } else {
            new_char_valid = true;
            complete = true;

            let last_char = current_str.chars().last().unwrap();
            if (last_char == '*') && (c == '/') {
                accepting = false
            } else {
                accepting = true
            }
        }

        ProcessResult::new(complete, accepting, new_char_valid)
    }
    fn build_token(&self) -> Option<Tokens> {
        let comment = self._get_built_str().clone();
        if self.is_done() { Some(Tokens::Comment(comment)) } else { None }
    }
}

#[derive(Debug)]
pub struct InvalidToken {
    characters: String,
    start_position: usize,
    end_position: usize,
}
impl InvalidToken {
    fn new(
        characters: String, start_position: usize, end_position: usize
    ) -> InvalidToken {
        InvalidToken {
            characters,
            start_position,
            end_position,
        }
    }
}
impl fmt::Display for InvalidToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f, "InvalidToken('{}' @ {}-{})",
            self.characters, self.start_position, self.end_position
        )
    }
}

pub struct Lexer {}
impl Lexer {
    pub fn new() -> Self {
        Lexer {}
    }
    fn make_token_builders() -> Vec<Box<dyn TokenBuilder>> {
        vec![
            Box::new(IdentifierBuilder::new()),
            Box::new(ConstantBuilder::new()),
            Box::new(PunctuatorsBuilder::new()),
            Box::new(SingleLineCommentBuilder::new()),
            Box::new(MultiLineCommentBuilder::new())
        ]
    }

    fn pad_input_str(input: &str) -> String {
        let mut padded_input = String::from(input);
        padded_input.push('\n');
        padded_input
    }

    pub fn tokenize(&self, raw_source: &str) -> Result<Vec<Tokens>, InvalidToken> {
        let mut tokens = Vec::new();
        let mut processing_token: bool = false;
        let source = Lexer::pad_input_str(raw_source);
        let length = source.chars().count();
        let mut search_end = 0;

        for (k, c) in source.chars().enumerate() {
            if k < search_end { continue }
            if !processing_token && c.is_whitespace() { continue }

            let mut token_builders = Lexer::make_token_builders();
            let mut searched_string = String::new();
            let search_start = k;

            // search for a valid token
            for i in search_start..length {
                let next_char = source.chars().nth(i).unwrap();
                // println!("ADD CHAR: {}", next_char);
                searched_string.push(next_char);

                let mut search_complete = true;
                for builder in token_builders.iter_mut() {
                    let accepting = builder.add_char(next_char);
                    if accepting { search_complete = false; }
                }
                if search_complete {
                    break
                }
            }
            // extract out the built token
            let mut token_found = false;
            for builder in token_builders.iter() {
                if builder.is_done() {
                    // TODO: use match pattern get get token instead
                    let token = builder.build_token().unwrap();
                    println!("MADE TOKEN {}", token);
                    search_end = search_start + token.get_length();
                    tokens.push(token);
                    processing_token = false;
                    token_found = true;
                    break
                }
            }

            if !token_found {
                let search_length = searched_string.len();
                return Err(InvalidToken::new(
                    searched_string, search_start,
                    search_start + search_length
                ));
            }
        }

        Ok(tokens)
    }
}


#[derive(Debug)]
pub enum LexerFromFileError {
    InvalidToken(InvalidToken),
    IoError(std::io::Error),
}
impl LexerFromFileError {
    pub fn message(&self) -> String {
        match self {
            LexerFromFileError::InvalidToken(token) => format!(
                "Invalid token: {}", token
            ),
            LexerFromFileError::IoError(e) => format!(
                "I/O error: {}", e
            ),
        }
    }
}
impl Display for LexerFromFileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "LexerFromFileError: {}", self.message())
    }
}


pub fn lex_from_filepath(
    file_path: &String, verbose: bool
) -> Result<Vec<Tokens>, LexerFromFileError> {
    let open_result = File::open(file_path);
    let mut file = match open_result {
        Ok(f) => f,
        Err(e) => return Err(LexerFromFileError::IoError(e)),
    };

    let mut contents = String::new();
    let read_result = file.read_to_string(&mut contents);
    if read_result.is_err() {
        return Err(LexerFromFileError::IoError(read_result.unwrap_err()));
    }

    // Print the file contents
    if verbose { println!("{}", contents); }
    let lexer = Lexer::new();
    let tokens_res = lexer.tokenize(&contents);
    let tokens = match tokens_res {
        Ok(t) => t,
        Err(e) => return Err(LexerFromFileError::InvalidToken(e)),
    };
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_process_char() {
        let mut builder = IdentifierBuilder::new();
        // Test valid identifier
        assert_eq!(builder.add_char('i'), true);
        assert_eq!(builder.add_char('n'), true);
        assert_eq!(builder.add_char('t'), true);
        // println!("BUILDER LENGTH = {}", builder.get_length());
        assert_eq!(builder.add_char(' '), false);
        assert_eq!(builder.is_done(), true);
        assert_eq!(builder.is_valid(), true)
    }

    #[test]
    fn test_punctuator() {
        let processor = PunctuatorProcessor::new(
            "(".parse().unwrap(), Punctuators::OpenParens
        );
        assert_eq!(processor.is_done(), false);
        assert_eq!(processor.process_char('('), ProcessResult::new(
            true, false, true
        ));
    }

    #[test]
    fn test_punctuator_builder() {
        let mut builder = PunctuatorsBuilder::new();
        assert_eq!(builder.is_done(), false);
        assert_eq!(builder.add_char('('), false);
        assert_eq!(builder.is_done(), true);
        assert_eq!(builder.add_char('('), false);
    }
}