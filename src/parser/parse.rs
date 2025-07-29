use std::collections::VecDeque;
use crate::lexer::lexer::{lex_from_filepath, Keywords, Punctuators, Tokens};
use crate::parser::asm_symbols::{AsmFunction, AsmImmediateValue, AsmInstruction, AsmOperand, AsmProgram, MovInstruction};
use crate::parser::parser_helpers::{ParseError, ParseErrorVariants, PoppedTokenContext, TokenStack};

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
    pop_context: Option<PoppedTokenContext>
}
impl Expression {
    pub fn new(constant: String) -> Expression {
        Expression {
            constant,
            pop_context: None
        }
    }

    fn parse(tokens: &mut TokenStack) -> Result<Expression, ParseError> {
        tokens.run_with_rollback(|stack_popper| {
            // <exp> ::= <int>
            let constant_wrapped_token_res = stack_popper.pop_front();
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
                    token_stack: stack_popper.token_stack.clone()
                }),
            };
            Ok(Expression {
                constant,
                pop_context: Some(stack_popper.build_pop_context())
            })
        })
    }

    fn to_asm_symbol(&self) -> AsmImmediateValue {
        // Convert the expression to an assembly representation
        let value = self.constant.parse::<i64>().unwrap();
        AsmImmediateValue::new(value).with_added_pop_context(
            self.pop_context.clone()
        )
    }
}

pub struct Statement {
    expression: Expression,
    pop_context: Option<PoppedTokenContext>
}
impl Statement {
    pub fn new(expression: Expression) -> Statement {
        Statement {
            expression,
            pop_context: None,
        }
    }

    fn parse(tokens: &mut TokenStack) -> Result<Statement, ParseError> {
        tokens.run_with_rollback(|stack_popper| {
            // <statement> ::= "return" <exp> ";"
            stack_popper.expect_pop_front(Tokens::Keyword(Keywords::Return))?;

            let expression = Expression::parse(stack_popper.token_stack)?;
            let punctuator_keyword_opt = stack_popper.pop_front();
            let punctuator_wrapped_keyword = match punctuator_keyword_opt {
                Ok(token) => token,
                _ => return Err(ParseError {
                    variant: ParseErrorVariants::NoMoreTokens(
                        "No semicolon token found".to_string()
                    ),
                    token_stack: stack_popper.clone_stack()
                }),
            };

            let punctuator_keyword = punctuator_wrapped_keyword.token;
            match punctuator_keyword {
                Tokens::Punctuator(Punctuators::Semicolon) => {},
                _ => return Err(ParseError {
                    variant: ParseErrorVariants::UnexpectedToken(
                        "Statement does not end with semicolon".to_string()
                    ),
                    token_stack: stack_popper.clone_stack()
                }),
            }

            Ok(Statement {
                expression,
                pop_context: Some(stack_popper.build_pop_context())
            })
        })
    }

    fn to_asm_symbol(&self) -> AsmImmediateValue {
        self.expression.to_asm_symbol().with_added_pop_context(
            self.pop_context.clone()
        )
    }
}

pub struct Function {
    name: Identifier,
    body: Statement,
    pop_context: Option<PoppedTokenContext>
}
impl Function {
    pub fn new(name: Identifier, body: Statement) -> Function {
        Function {
            name,
            body,
            pop_context: None,
        }
    }

    fn parse(tokens: &mut TokenStack) -> Result<Function, ParseError> {
        tokens.run_with_rollback(|stack_popper| {
            // <function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
            stack_popper.expect_pop_front(Tokens::Keyword(Keywords::Integer))?;
            let identifier = Identifier::parse_tokens(&mut stack_popper.token_stack)?;

            stack_popper.expect_pop_front(Tokens::Punctuator(Punctuators::OpenParens))?;
            stack_popper.expect_pop_front(Tokens::Keyword(Keywords::Void))?;
            stack_popper.expect_pop_front(Tokens::Punctuator(Punctuators::CloseParens))?;

            stack_popper.expect_pop_front(Tokens::Punctuator(Punctuators::OpenBrace))?;
            let statement = Statement::parse(&mut stack_popper.token_stack)?;
            stack_popper.expect_pop_front(Tokens::Punctuator(Punctuators::CloseBrace))?;

            Ok(Function {
                name: identifier, body: statement,
                pop_context: Some(stack_popper.build_pop_context())
            })
        })
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
    function: Function,
    pop_context: Option<PoppedTokenContext>
}
impl Program {
    pub fn new(function: Function) -> Program {
        Program {
            function,
            pop_context: None,
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
    tokens.run_with_rollback(|stack_popper| {
        let function = Function::parse(stack_popper.token_stack)?;
        if !stack_popper.is_empty() {
            return Err(ParseError {
                variant: ParseErrorVariants::UnexpectedExtraTokens(
                    "Unexpected tokens after function".to_string()
                ),
                token_stack: stack_popper.clone_stack()
            });
        }
        Ok(Program {
            function,
            pop_context: Some(stack_popper.build_pop_context())
        })
    })
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
