use std::collections::VecDeque;
use std::fmt::{Display};
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
            token_stack: token_stack.clone(),
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

#[derive(Clone, Debug)]
pub struct TokenStack {
    pub(crate) tokens: VecDeque<WrappedToken>,
    popped_tokens: Vec<WrappedToken>
}
impl TokenStack {
    pub fn pop_front(&mut self) -> Result<WrappedToken, ParseError> {
        match self.tokens.pop_front() {
            None => {
                Err(ParseError {
                    variant: ParseErrorVariants::NoMoreTokens("".to_owned()),
                    token_stack: self.clone()
                })
            }
            Some(token) => { Ok(token) }
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
                        token_stack: self.clone()
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
                token_stack: self.clone()
            })
        }
    }

    pub fn new(tokens: VecDeque<WrappedToken>) -> TokenStack {
        TokenStack {
            tokens,
            popped_tokens: vec![],
        }
    }

    pub fn new_from_vec(tokens: Vec<WrappedToken>) -> TokenStack {
        TokenStack::new(VecDeque::from(tokens))
    }

    pub fn run_with_rollback<F, T, E>(
        &mut self, func: F
    ) -> Result<T, E>
    where
        F: for<'b> FnOnce(&mut StackPopper<'b>) -> Result<T, E>,
    {
        let mut stack_popper = StackPopper::new(self);
        let result = func(&mut stack_popper);
        if result.is_err() {
            stack_popper.rollback();
        }
        result
    }
}

#[derive(Clone, Debug)]
pub struct PoppedTokenContext {
    pub popped_tokens: Vec<WrappedToken>,
}


pub struct StackPopper<'a> {
    pub(crate) token_stack: &'a mut TokenStack,
    pub(crate) popped_tokens : Vec<WrappedToken>
}
impl <'a> StackPopper<'a> {
    pub fn new(token_stack: &'a mut TokenStack) -> StackPopper<'a> {
        StackPopper {
            token_stack,
            popped_tokens: vec![],
        }
    }

    pub fn is_empty(&self) -> bool {
        self.token_stack.is_empty()
    }

    pub fn clone_stack(&self) -> TokenStack {
        self.token_stack.clone()
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
        PoppedTokenContext {
            popped_tokens: self.popped_tokens.clone(),
        }
    }

    pub fn pop(&mut self) -> Result<WrappedToken, ParseError> {
        let token = self.token_stack.pop_front()?;
        self.popped_tokens.push(token.clone());
        Ok(token)
    }

    pub fn rollback(&mut self) {
        for token in self.popped_tokens.drain(..).rev() {
            self.token_stack.tokens.push_front(token);
        }
    }
}
