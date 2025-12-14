use std::collections::VecDeque;
use std::num::ParseIntError;
use crate::lexer::lexer::{lex_from_filepath, Keywords, Tokens};
use crate::lexer::tokens::{Operators, Punctuators};
use crate::parser::parser_helpers::{
    ParseError, ParseErrorVariants, PoppedTokenContext, TokenStack
};

#[derive(Clone, Debug)]
#[derive(PartialEq)]
pub struct Identifier {
    pub(crate) name: String,
}
impl Identifier {
    pub fn new(identifier: String) -> Identifier {
        Identifier {
            name: identifier,
        }
    }
    pub(crate) fn name_to_string(&self) -> String {
        self.name.clone()
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
    Subtract,
    BitwiseNot,
    Not,
}
impl SupportedUnaryOperators {
    pub fn from_operator(op: Operators) -> Option<SupportedUnaryOperators> {
        match op {
            Operators::Subtract => Some(SupportedUnaryOperators::Subtract),
            Operators::BitwiseNot => Some(SupportedUnaryOperators::BitwiseNot),
            Operators::LogicalNot => Some(SupportedUnaryOperators::Not),
            _ => None,
        }
    }
    pub fn from_operator_as_result(
        op: Operators
    ) -> Result<SupportedUnaryOperators, ParseError> {
        match Self::from_operator(op) {
            Some(supported_op) => Ok(supported_op),
            None => Err(ParseError::new_without_stack(
                ParseErrorVariants::UnexpectedToken(
                    format!("Unsupported unary operator {op}")
                ),
            ))
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SupportedBinaryOperators {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,

    And,
    Or,
    CheckEqual,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
    AssignEqual
}
impl SupportedBinaryOperators {
    pub fn from_operator(op: Operators) -> Option<SupportedBinaryOperators> {
        match op {
            Operators::Add => Some(SupportedBinaryOperators::Add),
            Operators::Subtract => Some(SupportedBinaryOperators::Subtract),
            Operators::Multiply => Some(SupportedBinaryOperators::Multiply),
            Operators::Divide => Some(SupportedBinaryOperators::Divide),
            Operators::Modulo => Some(SupportedBinaryOperators::Modulo),

            Operators::LogicalAnd => Some(SupportedBinaryOperators::And),
            Operators::LogicalOr => Some(SupportedBinaryOperators::Or),
            Operators::EqualTo => Some(SupportedBinaryOperators::CheckEqual),
            Operators::NotEqualTo => Some(SupportedBinaryOperators::NotEqual),
            Operators::LessThan => Some(SupportedBinaryOperators::LessThan),
            Operators::LessThanOrEqual => Some(SupportedBinaryOperators::LessOrEqual),
            Operators::GreaterThan => Some(SupportedBinaryOperators::GreaterThan),
            Operators::GreaterThanOrEqual => Some(SupportedBinaryOperators::GreaterOrEqual),
            Operators::AssignEqual => Some(SupportedBinaryOperators::AssignEqual),
            _ => None,
        }
    }
    pub fn is_short_circuit(&self) -> bool {
        match self {
            SupportedBinaryOperators::And => true,
            SupportedBinaryOperators::Or => true,
            _ => false,
        }
    }
    pub fn to_precedence(&self) -> u8 {
        match self {
            SupportedBinaryOperators::Multiply => 50,
            SupportedBinaryOperators::Divide => 50,
            SupportedBinaryOperators::Modulo => 50,

            SupportedBinaryOperators::Add => 45,
            SupportedBinaryOperators::Subtract => 45,

            SupportedBinaryOperators::LessThan => 35,
            SupportedBinaryOperators::LessOrEqual => 35,
            SupportedBinaryOperators::GreaterThan => 35,
            SupportedBinaryOperators::GreaterOrEqual => 35,

            SupportedBinaryOperators::CheckEqual => 30,
            SupportedBinaryOperators::NotEqual => 30,
            SupportedBinaryOperators::And => 10,
            SupportedBinaryOperators::Or => 5,

            SupportedBinaryOperators::AssignEqual => 4,
        }
    }
    pub fn from_operator_as_result(
        op: Operators
    ) -> Result<SupportedBinaryOperators, ParseError> {
        match Self::from_operator(op) {
            Some(supported_op) => Ok(supported_op),
            None => Err(ParseError::new_without_stack(
                ParseErrorVariants::UnexpectedToken(
                    format!("Unsupported binary operator {op}")
                ),
            )),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ASTConstant {
    // TODO: use bignum instead of string (?)
    pub(crate) value: String,
    pub(crate) pop_context: Option<PoppedTokenContext>
}
impl ASTConstant {
    pub fn new(value: &str) -> ASTConstant {
        ASTConstant {
            value: value.to_owned(),
            pop_context: None
        }
    }
    pub fn to_u64(&self) -> Result<u64, ParseIntError> {
        self.value.parse::<u64>()
    }
    pub fn to_usize(&self) -> Result<usize, ParseIntError> {
        self.value.parse::<usize>()
    }
}

#[derive(Clone, Debug)]
pub enum ExpressionVariant {
    Constant(ASTConstant),
    UnaryOperation(SupportedUnaryOperators, Box<Expression>),
    ParensWrapped(Box<Expression>),
    BinaryOperation(SupportedBinaryOperators, Box<Expression>, Box<Expression>)
}
impl ExpressionVariant {
    pub fn get_pop_context(&self) -> Option<PoppedTokenContext> {
        match self {
            ExpressionVariant::Constant(constant) => constant.pop_context.clone(),
            ExpressionVariant::UnaryOperation(_, expr) => expr.pop_context.clone(),
            ExpressionVariant::ParensWrapped(expr) => expr.pop_context.clone(),
            ExpressionVariant::BinaryOperation(_, left_expr, right_expr) => {
                todo!()
            }
        }
    }
}

#[derive(Clone, Debug)]
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
        Self::parse_as_exp(tokens, 0)
    }
    fn is_next_operator_consumable(
        token: &Tokens, min_precedence: u8
    ) -> Option<SupportedBinaryOperators> {
        /*
        Check if the next token is a binary operator with sufficient precedence
        */
        let binary_operator = match token {
            Tokens::Operator(op) => {
                SupportedBinaryOperators::from_operator(*op)
            },
            _ => None
        };
        if let Some(ref bin_op) = binary_operator {
            if bin_op.to_precedence() >= min_precedence {
                return binary_operator;
            }
        }
        None
    }
    fn parse_as_exp(
        tokens: &mut TokenStack, min_precedence: u8
    ) -> Result<Expression, ParseError> {
        tokens.run_with_rollback(|stack_popper| {
            // <exp> ::= <factor> | <exp> <binop> <exp>
            let mut left_expr =
                Expression::parse_as_factor(&mut stack_popper.token_stack)?;
            let wrapped_next_code_token =
                stack_popper.token_stack.peek_front(true)?;
            let mut next_code_token = wrapped_next_code_token.token.clone();

            while let Some(
                binary_operator
            ) = Self::is_next_operator_consumable(
                &next_code_token, min_precedence
            ) {
                // consume the binary operator
                stack_popper.pop_front().expect("Failed to pop binary operator");
                let right_exp = Self::parse_as_exp(
                    &mut stack_popper.token_stack,
                    binary_operator.to_precedence() + 1
                )?;
                left_expr = Expression {
                    expr_item: ExpressionVariant::BinaryOperation(
                        binary_operator,
                        Box::new(left_expr),
                        Box::new(right_exp)
                    ),
                    pop_context: Some(stack_popper.build_pop_context())
                };

                let wrapped_next_code_token =
                    stack_popper.token_stack.peek_front(true)?;
                next_code_token = wrapped_next_code_token.token.clone();
            }

            Ok(left_expr)
        })
    }
    fn parse_as_factor(
        tokens: &mut TokenStack
    ) -> Result<Expression, ParseError> {
        // TODO: precedence needs to be forwarded from previous calls
        // <factor> ::= <int> | <unop> <factor> | "(" <exp> ")"
        let wrapped_front_code_token = tokens.peek_front(true)?;
        let front_code_token = wrapped_front_code_token.token.clone();

        let get_as_unop = |
            token: &Tokens
        | -> Result<SupportedUnaryOperators, ParseError> {
            match token {
                Tokens::Operator(op) => {
                    match SupportedUnaryOperators::from_operator_as_result(*op) {
                        Ok(unop) => { Ok(unop) },
                        Err(err) => { Err(err) }
                    }
                }
                _ => Err(ParseError {
                    variant: ParseErrorVariants::UnexpectedToken(format!(
                        "Unexpected token at factor: {token}"
                    )),
                    token_stack: tokens.soft_copy()
                })
            }
        };

        if let Tokens::Constant(_) = front_code_token {
            Self::parse_as_constant(tokens)
        } else if let Ok(_) = get_as_unop(&front_code_token) {
            Self::parse_as_unary_op(tokens)
        } else if let Tokens::Punctuator(Punctuators::OpenParens) = front_code_token {
            Self::parse_as_parens_wrapped(tokens)
        } else {
            return Err(ParseError {
                variant: ParseErrorVariants::UnexpectedToken(format!(
                    "Unexpected token at factor start \
                    {wrapped_front_code_token}"
                )),
                token_stack: tokens.soft_copy()
            });
        }
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
                        "Constant not found in factor".to_owned()
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
    fn parse_as_parens_wrapped(
        tokens: &mut TokenStack
    ) -> Result<Expression, ParseError> {
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

            let sub_expression = Self::parse(&mut stack_popper.token_stack)?;
            const CLOSE_PUNCTUATOR: Tokens = Tokens::Punctuator(Punctuators::CloseParens);
            stack_popper.expect_pop_front(CLOSE_PUNCTUATOR)?;
            let expr_item = ExpressionVariant::ParensWrapped(
                Box::new(sub_expression.clone())
            );

            Ok(Self {
                expr_item,
                pop_context: Some(stack_popper.build_pop_context())
            })
        })
    }

    fn parse_as_unary_op(
        tokens: &mut TokenStack
    ) -> Result<Expression, ParseError> {
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

            let sub_expression = Expression::parse_as_factor(
                &mut stack_popper.token_stack
            )?;
            Ok(Self {
                pop_context: Some(stack_popper.build_pop_context()),
                expr_item: ExpressionVariant::UnaryOperation(
                    operator, Box::new(sub_expression)
                )
            })
        })
    }
}

pub struct Statement {
    pub(crate) expression: Expression,
    pub(crate) pop_context: Option<PoppedTokenContext>
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


#[cfg(test)]
mod tests {
    use crate::lexer::lexer::lex_from_filepath;
    use crate::parser::parse::{parse, parse_from_filepath};
    use crate::parser::parser_helpers::TokenStack;

    #[test]
    fn test_parse_unop_parens() {
        let file_path = "./writing-a-c-compiler-tests/tests/chapter_3/valid/unop_parens.c";
        let lex_result = lex_from_filepath(file_path, true);

        if lex_result.is_err() {
            panic!("Lexer error: {:?}", lex_result.err().unwrap());
        }

        let tokens = lex_result.unwrap();
        let mut token_stack = TokenStack::new_from_vec(tokens);
        let parse_result = parse(&mut token_stack);
        let program = parse_result.unwrap();
        assert_eq!(program.function.name.name_to_string(), "main");
    }
    #[test]
    fn test_parse_sub_neg() {
        let file_path = "./writing-a-c-compiler-tests/tests/chapter_3/valid/sub_neg.c";
        let lex_result = lex_from_filepath(file_path, true);

        if lex_result.is_err() {
            panic!("Lexer error: {:?}", lex_result.err().unwrap());
        }

        let tokens = lex_result.unwrap();
        let mut token_stack = TokenStack::new_from_vec(tokens);
        let parse_result = parse(&mut token_stack);
        let program = parse_result.unwrap();
        assert_eq!(program.function.name.name_to_string(), "main");
    }
    #[test]
    fn test_parse_from_sub_neg() {
        let file_path = "./writing-a-c-compiler-tests/tests/chapter_3/valid/sub_neg.c";
        let parse_result = parse_from_filepath(file_path, true);
        if parse_result.is_err() {
            panic!("Parser error: {:?}", parse_result.err().unwrap());
        }
        let program = parse_result.unwrap();
        assert_eq!(program.function.name.name_to_string(), "main");
    }
    #[test]
    fn test_parse_from_assoc() {
        let file_path = "./writing-a-c-compiler-tests/tests/chapter_3/valid/associativity.c";
        let parse_result = parse_from_filepath(file_path, true);
        if parse_result.is_err() {
            panic!("Parser error: {:?}", parse_result.err().unwrap());
        }
        let program = parse_result.unwrap();
        assert_eq!(program.function.name.name_to_string(), "main");
    }
}
