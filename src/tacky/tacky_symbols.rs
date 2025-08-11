use std::collections::VecDeque;
use crate::parser::parse::{
    Identifier, ASTProgram, SupportedUnaryOperators,
    ASTFunction, ExpressionVariant
};

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

#[derive(Debug, Clone, PartialEq)]
pub enum TackyValue {
    Constant(String),
    Var(TackyVariable)
}

#[derive(Clone)]
pub struct UnaryInstruction {
    pub operator: SupportedUnaryOperators,
    pub src: TackyValue,
    pub dst: TackyVariable
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

pub struct TackyFunction {
    pub name: Identifier,
    pub instructions: Vec<TackyInstruction>,
}
impl TackyFunction {
    pub fn unroll_expression(
        expr_item: ExpressionVariant,
        var_counter: u64
    ) -> (VecDeque<TackyInstruction>, TackyValue) {
        match expr_item {
            ExpressionVariant::Constant(value) => {
                return (
                    VecDeque::new(),
                    TackyValue::Constant(value)
                );
            },
            ExpressionVariant::UnaryOperation(
                operator, sub_expr
            ) => {
                let new_var = TackyVariable::new(var_counter);
                let (sub_instructions, inner_value) = TackyFunction::unroll_expression(
                    *sub_expr, var_counter + 1
                );
                let new_unary_instruction = UnaryInstruction {
                    operator,
                    src: inner_value,
                    dst: new_var.clone()
                };
                let mut instructions = sub_instructions.clone();
                instructions.push_back(new_unary_instruction.to_tacky_instruction());
                return (
                    instructions,
                    TackyValue::Var(new_var)
                );
            }
        }
    }

    pub fn from_function(function: &ASTFunction) -> TackyFunction {
        let statement = &function.body;
        let expression = &statement.expression;
        let mut expr_item = expression.expr_item.clone();



        return TackyFunction {
            name: function.name.clone(),
            instructions: vec![]
        }
    }
}

pub struct TackyProgram {
    pub function: TackyFunction
}
impl TackyProgram {
    pub fn from_program(program: &ASTProgram) -> TackyProgram {
        return TackyProgram {
            function: TackyFunction::from_function(
                &program.function
            )
        };
    }
}

