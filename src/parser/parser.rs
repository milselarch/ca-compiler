use std::collections::VecDeque;
use crate::lexer::lexer::{Keywords, Punctuators, Tokens};

/*
Recursive descent parser implementation
TODO: use fancier error type
TODO: implement rollback for token stack for failed parse paths
*/

pub struct ParseError {
    pub(crate) message: String
}

pub struct TokenStack {
    tokens: VecDeque<Tokens>
}
impl TokenStack {
    pub fn pop_front(&mut self) -> Result<Tokens, ParseError> {
        match self.tokens.pop_front() {
            None => { Err(ParseError { message: "No more tokens".to_string() }) }
            Some(token) => { Ok(token) }
        }
    }
    pub fn expect_pop_front(
        &mut self, expected_token: Tokens
    ) -> Result<Tokens, ParseError> {
        // pops the front token and checks that it matches expected_token
        let popped_token = self.pop_front()?;
        if popped_token == expected_token {
            Ok(popped_token)
        } else {
            Err(ParseError { message: "Unexpected token".to_string() })
        }
    }

    pub fn new(tokens: VecDeque<Tokens>) -> TokenStack {
        TokenStack { tokens }
    }

    pub fn new_from_vec(tokens: Vec<Tokens>) -> TokenStack {
        TokenStack { tokens: VecDeque::from(tokens) }
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
        let identifier_token_opt = tokens.pop_front();
        let identifier_name = match identifier_token_opt {
            Ok(Tokens::Identifier(name)) => name,
            _ => return Err(ParseError { message: "No tokens found".to_string() }),
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
        let constant_token_opt = tokens.pop_front();
        let constant = match constant_token_opt {
            Ok(Tokens::Constant(constant)) => constant,
            _ => return Err(ParseError { message: "No tokens found".to_string() }),
        };
        Ok(Expression::new(constant))
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
        let punctuator_keyword = match punctuator_keyword_opt {
            Ok(token) => token,
            _ => return Err(ParseError { message:
                "No semicolon token found".to_string()
            }),
        };
        match punctuator_keyword {
            Tokens::Punctuator(Punctuators::Semicolon) => {},
            _ => return Err(ParseError { message:
                "Statement does not end with semicolon".to_string()
            }),
        }

        Ok(Statement::new(expression))
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
}

pub struct Program {
    function: Function
}


pub fn parse(tokens: &mut TokenStack) -> Result<Program, ParseError> {
    // <program> ::= <function>
    let function = Function::parse(tokens)?;
    Ok(Program { function })
}