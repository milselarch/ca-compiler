use std::{fmt};
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::Read;
use regex::Regex;

use crate::lexer::base_token_builder::{BaseTokenBuilder};
use crate::lexer::operators::OperatorsBuilder;
use crate::lexer::punctuators::PunctuatorsBuilder;
pub(crate) use crate::lexer::tokens::{is_word_boundary, Keywords, ProcessResult, TokenBuilder, Tokens};
pub(crate) use crate::lexer::tokens::{HasLength, SourceContext, WrappedToken};

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

impl Display for IdentifierBuilder {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "IdentifierBuilder")
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

impl Display for ConstantBuilder {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "ConstantBuilder")
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

impl Display for OperatorsBuilder {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "OperatorsBuilder")
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
            if process_result.complete || process_result.accepting {
                // println!("PROC_PUSH {}", process_result.accepting);
                processor._push_char(char);
            }
        }
    }

    fn process_char(&self, c: char) -> ProcessResult {
        let mut complete = false;
        let mut accepting = false;

        for processor in &self.operators {
            let process_result = processor.process_char(c);
            complete = complete || process_result.complete;
            accepting = accepting || process_result.accepting;
        }

        ProcessResult::new(complete, accepting, true)
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

impl Display for SingleLineCommentBuilder {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "SingleLineCommentBuilder")
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

impl Display for MultiLineCommentBuilder {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "MultiLineCommentBuilder")
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
            Box::new(OperatorsBuilder::new()),
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

    pub fn tokenize(
        &self, raw_source: &str
    ) -> Result<Vec<WrappedToken>, InvalidToken> {
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
                    let error_message = format!(
                        "Token builder {builder} did not produce a token for '{}'",
                        searched_string
                    );
                    let token = builder.build_token().expect(
                        format!("Token builder error: {}", error_message).as_str()
                    );
                    println!("MADE TOKEN {}", token);

                    search_end = search_start + token.get_length();
                    let content = builder._get_built_str().clone();
                    let context = SourceContext::new(
                        content, search_start, search_end
                    );

                    let wrapped_token = WrappedToken::new(token.clone(), context);
                    tokens.push(wrapped_token);
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
    file_path: &str, verbose: bool
) -> Result<Vec<WrappedToken>, LexerFromFileError> {
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
    use crate::lexer::punctuators::PunctuatorProcessor;
    use crate::lexer::tokens::Punctuators;
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