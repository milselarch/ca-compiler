use std::collections::VecDeque;
use std::num::ParseIntError;
use std::panic;
use crate::lexer::lexer::{lex_from_filepath, Keywords, Tokens};
use crate::lexer::tokens::{Operators, Punctuators};
use crate::parser::asm_symbols::{
    AsmFunction, AsmImmediateValue, AsmInstruction, AsmOperand, AsmProgram,
    MovInstruction, HasPopContexts
};
use crate::parser::parse::ExpressionVariant::UnaryOperation;
use crate::parser::parser_helpers::{
    ParseError, ParseErrorVariants, PoppedTokenContext, TokenStack
};

#[derive(Clone, Debug)]
#[derive(PartialEq)]
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
                token_stack: tokens.soft_copy()
            }),
        };
        Ok(Identifier::new(identifier_name))
    }
}


#[derive(Clone, Debug)]
pub enum SupportedUnaryOperators {
    Minus,
    BitwiseNot,
}
impl SupportedUnaryOperators {
    pub fn from_operator(op: Operators) -> Option<SupportedUnaryOperators> {
        match op {
            Operators::Minus => Some(SupportedUnaryOperators::Minus),
            Operators::BitwiseNot => Some(SupportedUnaryOperators::BitwiseNot),
            _ => None,
        }
    }
    pub fn from_operator_as_result(
        op: Operators
    ) -> Result<SupportedUnaryOperators, ParseError> {
        match Self::from_operator(op) {
            Some(supported_op) => Ok(supported_op),
            None => Err(ParseError {
                variant: ParseErrorVariants::UnexpectedToken(
                    format!("Unsupported unary operator {op}")
                ),
                token_stack: TokenStack::new(VecDeque::new())
            }),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ASTConstant {
    pub(crate) value: String,
    pub(crate) pop_context: Option<PoppedTokenContext>
}
impl ASTConstant {
    pub fn to_u64(&self) -> Result<u64, ParseIntError> {
        self.value.parse::<u64>()
    }
}


#[derive(Clone)]
pub enum ExpressionVariant {
    Constant(ASTConstant),
    UnaryOperation(SupportedUnaryOperators, Box<Expression>)
}

#[derive(Clone)]
pub struct Expression {
    pub(crate) expr_item: ExpressionVariant,
    pub(crate) pop_context: Option<PoppedTokenContext>
}
impl Expression {
    pub fn new(expr_item: ExpressionVariant) -> Expression {
        Expression {
            expr_item,
            pop_context: None
        }
    }

    fn parse(tokens: &mut TokenStack) -> Result<Expression, ParseError> {
        let wrapped_front_code_token = tokens.peek_front(true)?;
        let front_code_token = wrapped_front_code_token.token.clone();

        match front_code_token {
            Tokens::Punctuator(Punctuators::OpenParens) => {
                Self::parse_as_parens_wrapped(tokens)
            },
            Tokens::Operator(op) => {
                match SupportedUnaryOperators::from_operator_as_result(op) {
                    Ok(_) => { Self::parse_as_unary_op(tokens) },
                    Err(err) => { Err(err) }
                }
            },
            Tokens::Constant(_) => {
                Self::parse_as_constant(tokens)
            },
            _ => {
                Err(ParseError {
                    variant: ParseErrorVariants::UnexpectedToken(format!(
                        "Unexpected token at expression start \
                        {wrapped_front_code_token}"
                    )),
                    token_stack: tokens.soft_copy()
                })
            }
        }
    }

    fn parse_as_parens_wrapped(tokens: &mut TokenStack) -> Result<Expression, ParseError> {
        /*
        Try to parse a parenthesized expression first
        <exp> ::= "(" <exp> ")"
        */
        tokens.run_with_rollback(|stack_popper| {
            let open_paren_wrapped_token_res = stack_popper.pop_front();
            let open_paren_token_res = match open_paren_wrapped_token_res {
                Ok(token) => token,
                Err(err) => return Err(err),
            };

            let open_paren_token = open_paren_token_res.token;
            if open_paren_token != Tokens::Punctuator(Punctuators::OpenParens) {
                return Err(ParseError {
                    variant: ParseErrorVariants::UnexpectedToken(
                        "Expected opening parenthesis".to_owned()
                    ),
                    token_stack: stack_popper.token_stack.soft_copy()
                });
            }

            let sub_expression = Expression::parse(&mut stack_popper.token_stack)?;
            stack_popper.expect_pop_front(Tokens::Punctuator(Punctuators::CloseParens))?;

            Ok(Self {
                expr_item: sub_expression.expr_item,
                pop_context: Some(stack_popper.build_pop_context())
            })
        })
    }

    fn parse_as_unary_op(tokens: &mut TokenStack) -> Result<Expression, ParseError> {
        /*
        Try to parse a unary operation first
        <exp> ::= UnaryOperation(<op>, <exp>)
        */
        tokens.run_with_rollback(|stack_popper| {
            let unary_op_wrapped_token_res = stack_popper.pop_front();
            let unary_op_token_res = match unary_op_wrapped_token_res {
                Ok(token) => token,
                Err(err) => return Err(err),
            };

            let unary_op_token = unary_op_token_res.token;
            let operator = match unary_op_token {
                Tokens::Operator(op) => {
                    SupportedUnaryOperators::from_operator_as_result(op)?
                },
                _ => return Err(ParseError {
                    variant: ParseErrorVariants::NoMoreTokens(
                        "Unary operation not found in expression".to_owned()
                    ),
                    token_stack: stack_popper.token_stack.soft_copy()
                }),
            };

            let sub_expression = Expression::parse(&mut stack_popper.token_stack)?;
            Ok(Self {
                pop_context: Some(stack_popper.build_pop_context()),
                expr_item: ExpressionVariant::UnaryOperation(
                    operator, Box::new(sub_expression)
                )
            })
        })
    }

    fn parse_as_constant(tokens: &mut TokenStack) -> Result<Expression, ParseError> {
        // <exp> ::= Constant(<int>)
        tokens.run_with_rollback(|stack_popper| {
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
                    token_stack: stack_popper.token_stack.soft_copy()
                }),
            };

            let pop_context = stack_popper.build_pop_context();
            let ast_constant = ASTConstant {
                value: constant.clone(),
                pop_context: Some(pop_context.clone())
            };
            Ok(Expression {
                expr_item: ExpressionVariant::Constant(ast_constant),
                pop_context: Some(pop_context.clone())
            })
        })
    }


    fn to_asm_symbol(&self) -> AsmImmediateValue {
        match self.expr_item {
            ExpressionVariant::Constant(ref constant) => {
                let value = constant.to_u64().unwrap();
                AsmImmediateValue::new(value).with_added_pop_context(
                    self.pop_context.clone()
                )
            },
            ExpressionVariant::UnaryOperation(_, _) => {
                panic!("Unary operations not implemented yet");
            },
        }
    }
}

pub struct Statement {
    pub(crate) expression: Expression,
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

            let expression = Expression::parse(
                stack_popper.token_stack
            )?;
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

pub struct ASTFunction {
    pub(crate) name: Identifier,
    pub(crate) body: Statement,
    pub(crate) pop_context: Option<PoppedTokenContext>
}
impl ASTFunction {
    pub fn new(name: Identifier, body: Statement) -> ASTFunction {
        ASTFunction {
            name,
            body,
            pop_context: None,
        }
    }

    fn parse(tokens: &mut TokenStack) -> Result<ASTFunction, ParseError> {
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

            Ok(ASTFunction {
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
        let asm_func = AsmFunction::new(function_name)
                .add_instructions(instructions)
                .with_added_pop_context(self.pop_context.clone());
        asm_func
    }
}

pub struct ASTProgram {
    pub function: ASTFunction,
    pub pop_context: Option<PoppedTokenContext>
}
impl ASTProgram {
    pub fn new(function: ASTFunction) -> ASTProgram {
        ASTProgram {
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


pub fn parse(tokens: &mut TokenStack) -> Result<ASTProgram, ParseError> {
    // <program> ::= <function>
    tokens.run_with_rollback(|stack_popper| {
        let function = ASTFunction::parse(stack_popper.token_stack)?;
        if !stack_popper.is_empty() {
            return Err(ParseError {
                variant: ParseErrorVariants::UnexpectedExtraTokens(
                    "Unexpected tokens after function".to_string()
                ),
                token_stack: stack_popper.clone_stack()
            });
        }
        Ok(ASTProgram {
            function,
            pop_context: Some(stack_popper.build_pop_context())
        })
    })
}

pub fn parse_from_filepath(file_path: &str, verbose: bool) -> Result<ASTProgram, ParseError> {
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
