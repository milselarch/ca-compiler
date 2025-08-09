use std::collections::VecDeque;
use std::fmt::{Debug, Display, Formatter};
use crate::lexer::lexer::{
    LexerFromFileError, Tokens, WrappedToken
};

/*
Recursive descent parser_helpers implementation
TODO: use fancier error type
TODO: implement rollback for token stack for failed parse paths
*/

#[derive(Debug)]
pub enum ParseErrorVariants {
    GenericError(String),
    NoMoreTokens(String),
    UnexpectedToken(String),
    UnexpectedExtraTokens(String),
    LexerError(LexerFromFileError)
}

#[derive(Debug)]
pub struct ParseError {
    pub(crate) variant: ParseErrorVariants,
    pub(crate) token_stack: TokenStack
}
impl ParseError {
    pub fn new(message: String, token_stack: &TokenStack) -> ParseError {
        ParseError {
            variant: ParseErrorVariants::GenericError(message),
            token_stack: token_stack.soft_copy(),
        }
    }
    pub fn message(&self) -> String {
        match &self.variant {
            ParseErrorVariants::GenericError(msg) => msg.clone(),
            ParseErrorVariants::NoMoreTokens(msg) => msg.clone(),
            ParseErrorVariants::UnexpectedToken(msg) => msg.clone(),
            ParseErrorVariants::UnexpectedExtraTokens(msg) => msg.clone(),
            ParseErrorVariants::LexerError(err) => format!("Lexer error: {}", err),
        }
    }
}
impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ParseError: {}", self.message())
    }
}

#[derive(Debug)]
pub struct TokenStack {
    pub(crate) tokens: VecDeque<WrappedToken>,
    popped_tokens: Vec<WrappedToken>,
}
impl TokenStack {
    pub fn pop_front(&mut self) -> Result<WrappedToken, ParseError> {
        let wrapped_token_res = match self.tokens.pop_front() {
            None => {
                Err(ParseError {
                    variant: ParseErrorVariants::NoMoreTokens("".to_owned()),
                    token_stack: self.soft_copy()
                })
            }
            Some(token) => { Ok(token) }
        };

        if let Ok(ref token) = wrapped_token_res {
            let popped_token = token.clone();
            self.popped_tokens.push(popped_token);
        }

        wrapped_token_res
    }

    pub fn push(&mut self, token: WrappedToken) {
        self.tokens.push_back(token);
    }

    fn rollback_once(&mut self) -> Result<(), ParseError> {
        if let Some(token) = self.popped_tokens.pop() {
            self.tokens.push_front(token);
            Ok(())
        } else {
            Err(ParseError {
                variant: ParseErrorVariants::NoMoreTokens("No tokens to rollback".to_string()),
                token_stack: self.soft_copy()
            })
        }
    }

    pub fn soft_copy(&self) -> TokenStack {
        TokenStack {
            tokens: self.tokens.clone(),
            popped_tokens: self.popped_tokens.clone(),
            // max_seek_extent: self.max_seek_extent
        }
    }

    pub fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }

    pub fn expect_pop_front(
        &mut self, expected_token: Tokens
    ) -> Result<WrappedToken, ParseError> {
        self._expect_pop_front(expected_token, true)
    }

    pub fn _expect_pop_front(
        &mut self, expected_token: Tokens, skip_comments: bool
    ) -> Result<WrappedToken, ParseError> {
        while skip_comments {
            // Skip comments if the token is a comment
            let wrapped_token = match self.tokens.front() {
                Some(token) => token,
                None => {
                    return Err(ParseError {
                        variant: ParseErrorVariants::NoMoreTokens(
                            "No more tokens available".to_string()
                        ),
                        token_stack: self.soft_copy()
                    });
                }
            };

            match wrapped_token.token {
                Tokens::Comment(_) => {
                    self.pop_front()?;
                }
                _ => break, // Exit loop if the next token is not a comment
            }
        }

        // pops the front token and checks that it matches expected_token
        let popped_wrapped_token = self.pop_front()?;
        let popped_token = popped_wrapped_token.token.clone();
        if popped_token == expected_token {
            Ok(popped_wrapped_token)
        } else {
            Err(ParseError {
                variant: ParseErrorVariants::UnexpectedToken(format!(
                    "Unexpected token [{}]", popped_token
                ).to_string()),
                token_stack: self.soft_copy()
            })
        }
    }

    pub fn peek_front(&self, skip_comments: bool) -> Result<WrappedToken, ParseError> {
        for token in &self.tokens {
            if !skip_comments {
                return Ok(token.clone())
            }
            match token.token {
                Tokens::Comment(_) => continue,
                _ => return Ok(token.clone()),
            }
        }
        Err(ParseError {
            variant: ParseErrorVariants::NoMoreTokens(
                "No non-comment tokens available".to_string()
            ),
            token_stack: self.soft_copy()
        })
    }

    pub fn new(tokens: VecDeque<WrappedToken>) -> TokenStack {
        TokenStack { tokens, popped_tokens: vec![] }
    }

    pub fn new_from_vec(tokens: Vec<WrappedToken>) -> TokenStack {
        TokenStack::new(VecDeque::from(tokens))
    }

    pub fn get_current_source_position(&self) -> usize {
        // current position in the source code
        match self.tokens.front() {
            Some(wrapped_token) => wrapped_token.get_min_position(),
            None => 0, // If there are no tokens, return position 0
        }
    }

    pub fn get_current_token_position(&self) -> usize {
        // current position in the original token stack
        self.popped_tokens.len()
    }

    pub fn run_with_rollback<F, T, E>(
        &mut self, func: F
    ) -> Result<T, E>
    where
        F: for<'b> FnOnce(&mut StackPopper<'b>) -> Result<T, E>,
    {
        let mut stack_popper= StackPopper::new(self);

        let result = func(&mut stack_popper);
        // If the result is an error, rollback the popped tokens
        if result.is_err() {
            stack_popper.rollback().expect("Failed to rollback token stack");
        }
        result
    }
}

#[derive(Clone, Debug)]
pub struct PoppedTokenContext {
    pub start_token_position: usize,
    pub end_token_position: usize,
    pub start_source_position: usize,
    pub end_source_position: usize,
}


#[derive(Debug)]
pub struct StackPopper<'a> {
    pub(crate) token_stack: &'a mut TokenStack,
    start_source_position: usize,
    start_token_position: usize,
}
impl StackPopper<'_> {
    pub fn new(token_stack: &mut TokenStack) -> StackPopper {
        let start_source_position = token_stack.get_current_source_position();
        let start_token_position = token_stack.get_current_token_position();
        StackPopper {
            token_stack,
            start_source_position,
            start_token_position,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.token_stack.is_empty()
    }

    pub fn clone_stack(&self) -> TokenStack {
        self.token_stack.soft_copy()
    }

    pub fn pop_front(&mut self) -> Result<WrappedToken, ParseError> {
        self.token_stack.pop_front()
    }

    pub fn expect_pop_front(
        &mut self, expected_token: Tokens
    ) -> Result<WrappedToken, ParseError> {
        self.token_stack.expect_pop_front(expected_token)
    }

    pub fn build_pop_context(&self) -> PoppedTokenContext {
        let current_source_position = self.token_stack.get_current_source_position();
        let current_token_position = self.token_stack.get_current_token_position();

        PoppedTokenContext {
            start_token_position: self.start_token_position,
            end_token_position: current_token_position,
            start_source_position: self.start_source_position,
            end_source_position: current_source_position,
        }
    }

    pub fn pop(&mut self) -> Result<WrappedToken, ParseError> {
        let token = self.token_stack.pop_front()?;
        Ok(token)
    }

    pub fn rollback(&mut self) -> Result<(), ParseError> {
        // Rollback the token stack to the state before this popper was created
        let rollback_count =
            self.token_stack.popped_tokens.len() - self.start_token_position;
        for _ in 0..rollback_count {
            if let Err(err) = self.token_stack.rollback_once() {
                eprintln!("Error during rollback: {}", err);
                return Err(err);
            }
        }
        Ok(())
    }
}
