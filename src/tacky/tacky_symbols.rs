use crate::parser::asm_symbols::AsmProgram;
use crate::parser::parse::{
    Identifier, ASTProgram, SupportedUnaryOperators, ASTFunction,
    ExpressionVariant, ASTConstant, parse_from_filepath
};
use crate::parser::parser_helpers::{ParseError, PoppedTokenContext};

#[derive(Debug, Clone, PartialEq)]
pub struct TackyVariable {
    pub id: u64,
    pub name: String,
}
impl TackyVariable {
    pub fn new(id: u64) -> TackyVariable {
        TackyVariable { id, name: "".to_string() }
    }
}

#[derive(Debug, Clone)]
pub enum TackyValue {
    Constant(ASTConstant),
    Var(TackyVariable)
}

#[derive(Clone)]
pub struct UnaryInstruction {
    pub operator: SupportedUnaryOperators,
    pub src: TackyValue,
    pub dst: TackyVariable,
    pub pop_context: Option<PoppedTokenContext>
}
impl UnaryInstruction {
    pub fn to_tacky_instruction(&self) -> TackyInstruction {
        TackyInstruction::UnaryInstruction(self.clone())
    }
}

#[derive(Clone)]
pub enum TackyInstruction {
    UnaryInstruction(UnaryInstruction),
    Return(TackyValue),
}
impl TackyInstruction {
    pub fn unroll_expression(
        expr_item: ExpressionVariant,
        var_counter: u64
    ) -> (Vec<TackyInstruction>, TackyValue) {
        match expr_item {
            ExpressionVariant::Constant(ast_constant) => {
                (
                    Vec::new(),
                    TackyValue::Constant(ast_constant.clone())
                )
            },
            ExpressionVariant::UnaryOperation(
                operator, sub_expr
            ) => {
                let new_var = TackyVariable::new(var_counter);
                let sub_expr_item = sub_expr.expr_item.clone();
                let (sub_instructions, inner_value) = Self::unroll_expression(
                    sub_expr_item, var_counter + 1
                );
                let new_unary_instruction = UnaryInstruction {
                    operator,
                    src: inner_value,
                    dst: new_var.clone(),
                    pop_context: sub_expr.pop_context.clone()
                };
                let mut instructions = sub_instructions.clone();
                instructions.push(new_unary_instruction.to_tacky_instruction());
                (
                    instructions,
                    TackyValue::Var(new_var)
                )
            }
        }
    }
}


pub struct TackyFunction {
    pub name: Identifier,
    pub instructions: Vec<TackyInstruction>,
    pub pop_context: Option<PoppedTokenContext>
}
impl TackyFunction {
    pub fn from_function(function: &ASTFunction) -> TackyFunction {
        let statement = &function.body;
        let expression = &statement.expression;
        let expr_item = expression.expr_item.clone();
        let (mut sub_instructions, temp_value) =
            TackyInstruction::unroll_expression(expr_item, 0);

        let return_instruction = TackyInstruction::Return(temp_value);
        sub_instructions.push(return_instruction);

        TackyFunction {
            name: function.name.clone(),
            instructions: sub_instructions,
            pop_context: function.pop_context.clone()
        }
    }
}

pub struct TackyProgram {
    pub function: TackyFunction,
    pop_context: Option<PoppedTokenContext>
}
impl TackyProgram {
    pub fn from_program(program: &ASTProgram) -> TackyProgram {
        return TackyProgram {
            pop_context: program.pop_context.clone(),
            function: TackyFunction::from_function(
                &program.function
            )
        };
    }
}

pub fn tacky_gen_from_filepath(
    file_path: &str, verbose: bool
) -> Result<TackyProgram, ParseError> {
    let parse_result = parse_from_filepath(file_path, verbose);
    if parse_result.is_err() {
        return Err(parse_result.err().unwrap());
    }
    let program = parse_result.unwrap();
    let tacky_program = TackyProgram::from_program(&program);
    Ok(tacky_program)
}

