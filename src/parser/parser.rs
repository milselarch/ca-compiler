use std::collections::VecDeque;
use std::fmt::Display;
use crate::lexer::lexer::{lex_from_filepath, Keywords, LexerFromFileError, Punctuators, Tokens, WrappedToken};
use crate::parser::asm_symbols::{
    AsmFunction, AsmImmediateValue, AsmInstruction,
    AsmOperand, AsmProgram, MovInstruction
};
/*
Recursive descent parser implementation
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
    variant: ParseErrorVariants,
    token_stack: TokenStack
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
    tokens: VecDeque<WrappedToken>
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
        TokenStack { tokens }
    }

    pub fn new_from_vec(tokens: Vec<WrappedToken>) -> TokenStack {
        TokenStack { tokens: VecDeque::from(tokens) }
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


pub struct StackPopper<'a> {
    token_stack: &'a mut TokenStack,
    popped_tokens : Vec<WrappedToken>
}
impl <'a> StackPopper<'a> {
    pub fn new(token_stack: &'a mut TokenStack) -> StackPopper<'a> {
        StackPopper {
            token_stack,
            popped_tokens: vec![],
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


pub struct Identifier {
    name: String,
}
impl Identifier {
    pub fn new(identifier: String) -> Identifier {
        Identifier {
            name: identifier,
        }
    }

    fn parse_tokens(
        tokens: &mut TokenStack
    ) -> Result<Identifier, ParseError> {
        // <identifier> ::= ? An identifier token ?
        let identifier_wrapped_token_res = tokens.pop_front();
        let identifier_wrapped_token = match identifier_wrapped_token_res {
            Ok(token) => token,
            Err(err) => return Err(err),
        };

        let identifier_token = identifier_wrapped_token.token;
        let identifier_name = match identifier_token {
            Tokens::Identifier(name) => name,
            _ => return Err(ParseError {
                variant: ParseErrorVariants::NoMoreTokens(
                    "No identifier token found".to_string()
                ),
                token_stack: tokens.clone()
            }),
        };
        Ok(Identifier::new(identifier_name))
    }
}

pub struct Expression {
    constant: String,
}
impl Expression {
    pub fn new(constant: String) -> Expression {
        Expression {
            constant,
        }
    }

    fn parse(tokens: &mut TokenStack) -> Result<Expression, ParseError> {
        // <exp> ::= <int>
        let constant_wrapped_token_res = tokens.pop_front();
        let constant_token_res = match constant_wrapped_token_res {
            Ok(token) => token,
            Err(err) => return Err(err),
        };

        let constant_token = constant_token_res.token;
        let constant = match constant_token {
            Tokens::Constant(constant) => constant,
            _ => return Err(ParseError {
                variant: ParseErrorVariants::NoMoreTokens(
                    "Constant not found in expression".to_owned()
                ),
                token_stack: tokens.clone()
            }),
        };
        Ok(Expression::new(constant))
    }

    fn to_asm_symbol(&self) -> AsmImmediateValue {
        // Convert the expression to an assembly representation
        let value = self.constant.parse::<i64>().unwrap();
        AsmImmediateValue::new(value)
    }
}

pub struct Statement {
    expression: Expression
}
impl Statement {
    pub fn new(expression: Expression) -> Statement {
        Statement {
            expression,
        }
    }

    fn parse(tokens: &mut TokenStack) -> Result<Statement, ParseError> {
        // <statement> ::= "return" <exp> ";"
        tokens.expect_pop_front(Tokens::Keyword(Keywords::Return))?;
        let expression = Expression::parse(tokens)?;
        let punctuator_keyword_opt = tokens.pop_front();
        let punctuator_wrapped_keyword = match punctuator_keyword_opt {
            Ok(token) => token,
            _ => return Err(ParseError {
                variant: ParseErrorVariants::NoMoreTokens(
                    "No semicolon token found".to_string()
                ),
                token_stack: tokens.clone()
            }),
        };

        let punctuator_keyword = punctuator_wrapped_keyword.token;
        match punctuator_keyword {
            Tokens::Punctuator(Punctuators::Semicolon) => {},
            _ => return Err(ParseError {
                variant: ParseErrorVariants::UnexpectedToken(
                    "Statement does not end with semicolon".to_string()
                ),
                token_stack: tokens.clone()
            }),
        }

        Ok(Statement::new(expression))
    }

    fn to_asm_symbol(&self) -> AsmImmediateValue {
        self.expression.to_asm_symbol()
    }
}

pub struct Function {
    name: Identifier,
    body: Statement,
}
impl Function {
    pub fn new(name: Identifier, body: Statement) -> Function {
        Function {
            name,
            body,
        }
    }

    fn parse(tokens: &mut TokenStack) -> Result<Function, ParseError> {

        let mut stack_popper = StackPopper::new(tokens);
        tokens.run_with_rollback(|spopper| {
            // <function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
            spopper.token_stack.expect_pop_front(Tokens::Keyword(Keywords::Integer))?;
            let identifier = Identifier::parse_tokens(&mut spopper.token_stack)?;

            spopper.token_stack.expect_pop_front(Tokens::Punctuator(Punctuators::OpenParens))?;
            spopper.token_stack.expect_pop_front(Tokens::Keyword(Keywords::Void))?;
            spopper.token_stack.expect_pop_front(Tokens::Punctuator(Punctuators::CloseParens))?;

            spopper.token_stack.expect_pop_front(Tokens::Punctuator(Punctuators::OpenBrace))?;
            let statement = Statement::parse(&mut spopper.token_stack)?;
            spopper.token_stack.expect_pop_front(Tokens::Punctuator(Punctuators::CloseBrace))?;

            Ok(Function { name: identifier, body: statement })
        });

        // <function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
        tokens.expect_pop_front(Tokens::Keyword(Keywords::Integer))?;
        let identifier = Identifier::parse_tokens(tokens)?;

        tokens.expect_pop_front(Tokens::Punctuator(Punctuators::OpenParens))?;
        tokens.expect_pop_front(Tokens::Keyword(Keywords::Void))?;
        tokens.expect_pop_front(Tokens::Punctuator(Punctuators::CloseParens))?;

        tokens.expect_pop_front(Tokens::Punctuator(Punctuators::OpenBrace))?;
        let statement = Statement::parse(tokens)?;
        tokens.expect_pop_front(Tokens::Punctuator(Punctuators::CloseBrace))?;
        Ok(Function { name: identifier, body: statement })
    }

    fn to_asm_symbol(&self) -> AsmFunction {
        // Convert the function to an assembly representation
        let function_name = self.name.name.clone();
        let mut instructions: Vec<AsmInstruction> = vec![];
        let body_expression = self.body.to_asm_symbol();

        instructions.push(AsmInstruction::Mov(
            MovInstruction {
                source: AsmOperand::ImmediateValue(body_expression),
                destination: AsmOperand::Register
            })
        );
        instructions.push(AsmInstruction::Ret);

        AsmFunction {
            name: function_name,
            instructions,
        }
    }
}

pub struct Program {
    function: Function
}
impl Program {
    pub fn new(function: Function) -> Program {
        Program {
            function,
        }
    }

    pub fn to_asm_symbol(&self) -> AsmProgram {
        AsmProgram {
            function: self.function.to_asm_symbol(),
        }
    }
}


pub fn parse(tokens: &mut TokenStack) -> Result<Program, ParseError> {
    // <program> ::= <function>
    let function = Function::parse(tokens)?;
    if !tokens.tokens.is_empty() {
        return Err(ParseError {
            variant: ParseErrorVariants::UnexpectedExtraTokens(
                "Unexpected tokens after function".to_string()
            ),
            token_stack: tokens.clone()
        });
    }
    Ok(Program { function })
}

pub fn parse_from_filepath(file_path: &str, verbose: bool) -> Result<Program, ParseError> {
    let lex_result = lex_from_filepath(file_path, verbose);
    if lex_result.is_err() {
        return Err(ParseError {
            variant: ParseErrorVariants::LexerError(lex_result.err().unwrap()),
            token_stack: TokenStack::new(VecDeque::new())
        })
    }

    let tokens = lex_result.unwrap();
    let mut token_stack = TokenStack::new_from_vec(tokens);
    let parse_result = parse(&mut token_stack);
    parse_result
}

pub fn asm_gen_from_filepath(
    file_path: &str, verbose: bool
) -> Result<AsmProgram, ParseError> {
    let parse_result = parse_from_filepath(file_path, verbose);
    let program = match parse_result {
        Ok(program) => program,
        Err(err) => return Err(err),
    };

    let asm_program = program.to_asm_symbol();
    Ok(asm_program)
}
