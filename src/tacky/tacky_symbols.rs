use std::hash::{Hash, Hasher};
use strum_macros::Display;
use crate::constants::TAB;
use crate::parser::parse::{
    Identifier, ASTProgram, SupportedUnaryOperators, ASTFunction, ExpressionVariant,
    ASTConstant, parse_from_filepath, SupportedBinaryOperators
};
use crate::parser::parser_helpers::{ParseError, PoppedTokenContext};

pub trait ToTackyInstruction: Sized {
    fn to_tacky_instruction(&self) -> TackyInstruction;
}
pub trait PrintableTacky {
    fn print_tacky_code(&self, depth: u64) -> String;
}

#[derive(Debug, Clone)]
pub struct TackyVariable {
    pub id: u64,
    pub name: String,
}
impl TackyVariable {
    pub fn new(id: u64) -> TackyVariable {
        TackyVariable { id, name: "".to_string() }
    }
    pub fn to_tacky_value(&self) -> TackyValue {
        TackyValue::Var(self.clone())
    }
}
impl Eq for TackyVariable {}
impl PartialEq for TackyVariable {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl Hash for TackyVariable {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}
impl PrintableTacky for TackyVariable {
    fn print_tacky_code(&self, depth: u64) -> String {
        let indent = TAB.repeat(depth as usize);
        format!("{}TackyVariable: id={}, name={}\n", indent, self.id, self.name)
    }
}

#[derive(Debug, Clone)]
pub struct UnrollResult {
    pub instructions: Vec<TackyInstruction>,
    pub value: TackyValue,
    pub next_free_var_id: u64
}
impl UnrollResult {
    pub fn new(
        instructions: Vec<TackyInstruction>,
        value: TackyValue,
        next_free_var_id: u64
    ) -> UnrollResult {
        UnrollResult {
            instructions,
            value,
            next_free_var_id
        }
    }

    pub fn with_annotation(
        &self, annotation: AnnotationStartInstruction
    ) -> Self {
        let mut new_instructions = vec![];
        new_instructions.push(
            annotation.to_tacky_instruction()
        );
        new_instructions.extend(self.instructions.clone());
        new_instructions.push(AnnotationEndInstruction::new(
            annotation.label.clone(), annotation.pop_context.clone()
        ).to_tacky_instruction());

        UnrollResult {
            instructions: new_instructions,
            value: self.value.clone(),
            next_free_var_id: self.next_free_var_id
        }
    }
}

#[derive(Debug, Clone)]
pub enum TackyValue {
    Constant(ASTConstant),
    Var(TackyVariable)
}
impl TackyValue {
    pub fn new_var(id: u64) -> TackyValue {
        TackyValue::Var(TackyVariable::new(id))
    }
    pub fn new_constant(value: &str) -> TackyValue {
        TackyValue::Constant(ASTConstant::new(value))
    }
    pub fn get_id(&self) -> Option<u64> {
        match self {
            TackyValue::Constant(_) => None,
            TackyValue::Var(v) => Some(v.id)
        }
    }
}
impl PrintableTacky for TackyValue {
    fn print_tacky_code(&self, depth: u64) -> String {
        let indent = TAB.repeat(depth as usize);
        match self {
            TackyValue::Constant(c) => {
                format!("{}Constant: {}\n", indent, c.value)
            },
            TackyValue::Var(v) => {
                format!("{}Var: id={}, name={}\n", indent, v.id, v.name)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct UnaryInstruction {
    pub operator: SupportedUnaryOperators,
    pub src: TackyValue,
    pub dst: TackyVariable,
    pub pop_context: Option<PoppedTokenContext>
}
impl UnaryInstruction {
    pub fn new(
        operator: SupportedUnaryOperators, src: TackyValue,
        dst: TackyVariable
    ) -> UnaryInstruction {
        UnaryInstruction {
            operator,
            src,
            dst,
            pop_context: None
        }
    }
}
impl ToTackyInstruction for UnaryInstruction {
    fn to_tacky_instruction(&self) -> TackyInstruction {
        TackyInstruction::UnaryInstruction(self.clone())
    }
}
impl PrintableTacky for UnaryInstruction {
    fn print_tacky_code(&self, depth: u64) -> String {
        let indent = TAB.repeat(depth as usize);
        let mut result = String::new();
        result.push_str(&format!("{indent}UnaryInstruction:\n"));
        result.push_str(&format!(
            "{indent}{TAB}Operator: {:?}\n", self.operator
        ));
        result.push_str(&format!("{indent}{TAB}Src:\n"));
        result.push_str(&self.src.print_tacky_code(depth + 2));
        result.push_str(&format!("{indent}{TAB}Dst:\n"));
        result.push_str(&self.dst.print_tacky_code(depth + 2));
        result
    }
}

#[derive(Clone, Debug)]
pub struct BinaryInstruction {
    pub operator: SupportedBinaryOperators,
    pub left: TackyValue,
    pub right: TackyValue,
    pub dst: TackyVariable,
    pub pop_context: Option<PoppedTokenContext>
}
impl BinaryInstruction {
    pub fn new(
        operator: SupportedBinaryOperators,
        left: TackyValue,
        right: TackyValue,
        dst: TackyVariable
    ) -> BinaryInstruction {
        BinaryInstruction {
            operator,
            left,
            right,
            dst,
            pop_context: None
        }
    }
}
impl ToTackyInstruction for BinaryInstruction {
    fn to_tacky_instruction(&self) -> TackyInstruction {
        TackyInstruction::BinaryInstruction(self.clone())
    }
}
impl PrintableTacky for BinaryInstruction {
    fn print_tacky_code(&self, depth: u64) -> String {
        let indent = TAB.repeat(depth as usize);
        let mut result = String::new();
        result.push_str(&format!("{indent}BinaryInstruction:\n"));
        result.push_str(&format!(
            "{indent}{TAB}Operator: {:?}\n", self.operator
        ));
        result.push_str(&format!("{indent}{TAB}Left:\n"));
        result.push_str(&self.left.print_tacky_code(depth + 2));
        result.push_str(&format!("{indent}{TAB}Right:\n"));
        result.push_str(&self.right.print_tacky_code(depth + 2));
        result.push_str(&format!("{indent}{TAB}Dst:\n"));
        result.push_str(&self.dst.print_tacky_code(depth + 2));
        result
    }
}

#[derive(Clone, Debug)]
pub struct CopyInstruction {
    pub src: TackyValue, // constant or variable
    pub dst: TackyVariable, // variable only
    pub pop_context: Option<PoppedTokenContext>
}
impl CopyInstruction {
    pub fn new(
        src: TackyValue,
        dst: TackyVariable
    ) -> CopyInstruction {
        CopyInstruction {
            src,
            dst,
            pop_context: None
        }
    }
}
impl ToTackyInstruction for CopyInstruction {
    fn to_tacky_instruction(&self) -> TackyInstruction {
        TackyInstruction::CopyInstruction(self.clone())
    }
}

#[derive(Clone, Debug)]
pub struct JumpInstruction {
    pub target: Identifier,
    pub pop_context: Option<PoppedTokenContext>
}
impl JumpInstruction {
    pub fn new(
        target: Identifier
    ) -> JumpInstruction {
        JumpInstruction {
            target,
            pop_context: None
        }
    }
}
impl ToTackyInstruction for JumpInstruction {
    fn to_tacky_instruction(&self) -> TackyInstruction {
        TackyInstruction::JumpInstruction(self.clone())
    }
}

#[derive(Clone, Debug)]
pub struct JumpIfZeroInstruction {
    pub condition: TackyValue,
    pub target: Identifier,
    pub pop_context: Option<PoppedTokenContext>
}
impl JumpIfZeroInstruction {
    pub fn new(
        condition: TackyValue,
        target: Identifier
    ) -> JumpIfZeroInstruction {
        JumpIfZeroInstruction {
            condition,
            target,
            pop_context: None
        }
    }
}
impl ToTackyInstruction for JumpIfZeroInstruction {
    fn to_tacky_instruction(&self) -> TackyInstruction {
        TackyInstruction::JumpIfZeroInstruction(self.clone())
    }
}

#[derive(Clone, Debug)]
pub struct JumpIfNotZeroInstruction {
    pub condition: TackyValue,
    pub target: Identifier,
    pub pop_context: Option<PoppedTokenContext>
}
impl JumpIfNotZeroInstruction {
    pub fn new(
        condition: TackyValue,
        target: Identifier
    ) -> JumpIfNotZeroInstruction {
        JumpIfNotZeroInstruction {
            condition,
            target,
            pop_context: None
        }
    }
}
impl ToTackyInstruction for JumpIfNotZeroInstruction {
    fn to_tacky_instruction(&self) -> TackyInstruction {
        TackyInstruction::JumpIfNotZeroInstruction(self.clone())
    }
}

#[derive(Clone, Debug)]
pub struct LabelInstruction {
    pub label: Identifier,
    pub pop_context: Option<PoppedTokenContext>
}
impl LabelInstruction {
    pub fn new(
        label: Identifier
    ) -> LabelInstruction {
        LabelInstruction {
            label,
            pop_context: None
        }
    }
}
impl ToTackyInstruction for LabelInstruction {
    fn to_tacky_instruction(&self) -> TackyInstruction {
        TackyInstruction::LabelInstruction(self.clone())
    }
}

#[derive(Clone, Debug)]
pub struct AnnotationStartInstruction {
    pub label: Identifier,
    pub pop_context: Option<PoppedTokenContext>
}
impl AnnotationStartInstruction {
    pub fn new(
        label: Identifier, pop_context: Option<PoppedTokenContext>
    ) -> AnnotationStartInstruction {
        // TODO: use this in places where we convert AST to a block
        //  of multiple Tacky instructions
        AnnotationStartInstruction {
            label, pop_context
        }
    }
}
impl ToTackyInstruction for AnnotationStartInstruction {
    fn to_tacky_instruction(&self) -> TackyInstruction {
        TackyInstruction::AnnotationStartInstruction(self.clone())
    }
}

#[derive(Clone, Debug)]
pub struct AnnotationEndInstruction {
    pub label: Identifier,
    pub pop_context: Option<PoppedTokenContext>
}
impl AnnotationEndInstruction {
    pub fn new(
        label: Identifier, pop_context: Option<PoppedTokenContext>
    ) -> AnnotationEndInstruction {
        AnnotationEndInstruction {
            label, pop_context
        }
    }
}
impl ToTackyInstruction for AnnotationEndInstruction {
    fn to_tacky_instruction(&self) -> TackyInstruction {
        TackyInstruction::AnnotationEndInstruction(self.clone())
    }
}

#[derive(Clone, Debug, Display)]
pub enum TackyInstruction {
    UnaryInstruction(UnaryInstruction),
    BinaryInstruction(BinaryInstruction),
    CopyInstruction(CopyInstruction),
    JumpInstruction(JumpInstruction),
    JumpIfZeroInstruction(JumpIfZeroInstruction),
    JumpIfNotZeroInstruction(JumpIfNotZeroInstruction),
    LabelInstruction(LabelInstruction),
    AnnotationStartInstruction(AnnotationStartInstruction),
    AnnotationEndInstruction(AnnotationEndInstruction),
    Return(TackyValue),
}
impl ToTackyInstruction for TackyInstruction {
    fn to_tacky_instruction(&self) -> TackyInstruction {
        self.clone()
    }
}

impl TackyInstruction {
    pub fn unroll_short_circuit(
        left: ExpressionVariant,
        right: ExpressionVariant,
        var_counter: u64,
        is_and: bool
    ) -> UnrollResult {
        /*
        TODO: support for an annotated block of instructions
          would be really nice

        // Conditional jump instruction to use for short-circuit
        CondJump(v) :=
            JumpIfZero(v, short_circuit_jmp_label) if AND
            JumpIfNotZero(v, short_circuit_jmp_label)

        // return value to assign if no short-circuit occurs
        NoCondJumpValue :=
            1 if AND
            0 if OR
        CondJumpValue :=
            0 if AND
            1 if OR

        output format (AND / OR):
        ----------------------
        <instructions for e1>
        v1 = <result of e1>
        CondJump(v1)
        <instructions for e2>
        v2 = <result of e2>
        CondJump(v2)

        result = NoCondJumpValue
        Jump(short_circuit_end_label)
        Label(short_circuit_jmp_label)
        result = CondJumpValue
        Label(short_circuit_end_label)
        */
        let jump_label =
            Identifier::new(format!("short_circuit_jmp_{}", var_counter));
        let end_label =
            Identifier::new(format!("short_circuit_end_{}", var_counter));

        let build_conditional_jump = |
            value: TackyValue
        | -> TackyInstruction {
            if is_and {
                // we short circuit if one of the operands is zero
                JumpIfZeroInstruction::new(
                    value, jump_label.clone()
                ).to_tacky_instruction()
            } else {
                // we short circuit if one of the operands is non-zero
                JumpIfNotZeroInstruction::new(
                    value, jump_label.clone()
                ).to_tacky_instruction()
            }
        };

        let no_jump_result_value = if is_and {
            TackyValue::Constant(ASTConstant::new("1"))
        } else {
            TackyValue::Constant(ASTConstant::new("0"))
        };
        let jump_result_value = if is_and {
            TackyValue::Constant(ASTConstant::new("0"))
        } else {
            TackyValue::Constant(ASTConstant::new("1"))
        };

        let left_unroll_result = Self::unroll_expression(left, var_counter);
        let var_counter = left_unroll_result.next_free_var_id;
        let right_unroll_result = Self::unroll_expression(right, var_counter);
        let var_counter = right_unroll_result.next_free_var_id;
        // contains the result of the short-circuit and operation
        let result_tacky_var = TackyVariable::new(var_counter);
        let var_counter = var_counter + 1;

        let mut circuit_instructions: Vec<TackyInstruction> = vec![];
        // <instructions for e1>
        circuit_instructions.extend(left_unroll_result.instructions);
        // CondJump(v1)
        circuit_instructions.push(build_conditional_jump(left_unroll_result.value));
        // <instructions for e2>
        circuit_instructions.extend(right_unroll_result.instructions);
        // CondJump(v2)
        circuit_instructions.push(build_conditional_jump(right_unroll_result.value));

        // result = NoCondJumpValue
        circuit_instructions.push(CopyInstruction::new(
            no_jump_result_value, result_tacky_var.clone()
        ).to_tacky_instruction());
        // Jump(short_circuit_end_label)
        circuit_instructions.push(
            JumpInstruction::new(end_label.clone()).to_tacky_instruction()
        );
        // Label(short_circuit_jmp_label)
        circuit_instructions.push(
            LabelInstruction::new(jump_label.clone()).to_tacky_instruction()
        );
        // result = CondJumpValue
        circuit_instructions.push(CopyInstruction::new(
            jump_result_value, result_tacky_var.clone()
        ).to_tacky_instruction());
        // Label(short_circuit_end_label)
        circuit_instructions.push(
            LabelInstruction::new(end_label).to_tacky_instruction()
        );

        let tacky_instructions: Vec<TackyInstruction> = circuit_instructions.iter().map(
            |instr| instr.to_tacky_instruction()
        ).collect();

        let unroll_result = UnrollResult::new(
            tacky_instructions,
            TackyValue::Var(result_tacky_var),
            var_counter
        );
        unroll_result
    }

    pub fn unroll_expression(
        expr_item: ExpressionVariant,
        var_counter: u64
    ) -> UnrollResult {
        let annotation_identifier =
            Identifier::new(format!("EXPR_UNROLL_{}", var_counter));

        let unroll_result = match expr_item {
            ExpressionVariant::Constant(ast_constant) => {
                UnrollResult::new(
                    Vec::new(),
                    TackyValue::Constant(ast_constant.clone()),
                    var_counter
                ).with_annotation(AnnotationStartInstruction::new(
                    annotation_identifier,
                    ast_constant.pop_context
                ))
            },
            ExpressionVariant::UnaryOperation(
                operator, sub_expr
            ) => {
                let sub_expr_item = sub_expr.expr_item.clone();
                let inner_unroll_res = Self::unroll_expression(
                    sub_expr_item, var_counter
                );

                let var_counter = inner_unroll_res.next_free_var_id;
                let new_var = TackyVariable::new(var_counter);
                let var_counter = var_counter + 1;

                let new_unary_instruction = UnaryInstruction {
                    operator,
                    src: inner_unroll_res.value,
                    dst: new_var.clone(),
                    pop_context: sub_expr.pop_context.clone()
                };

                let sub_instructions = inner_unroll_res.instructions;
                let mut instructions = sub_instructions.clone();
                instructions.push(new_unary_instruction.to_tacky_instruction());

                UnrollResult::new(
                    instructions,
                    TackyValue::Var(new_var),
                    var_counter
                ).with_annotation(AnnotationStartInstruction::new(
                    annotation_identifier,
                    sub_expr.pop_context.clone()
                ))
            }
            ExpressionVariant::BinaryOperation(
                operator, left, right
            ) => {
                let bin_pop_context = if let (
                    Some(left_ctx), Some(right_ctx)
                ) = (&left.pop_context, &right.pop_context) {
                    // both sides have pop contexts, merge them
                    Some(left_ctx | right_ctx)
                } else if let Some(left_ctx) = &left.pop_context {
                    Some(left_ctx.clone())
                } else if let Some(right_ctx) = &right.pop_context {
                    Some(right_ctx.clone())
                } else {
                    None
                };

                if operator.is_short_circuit() {
                    return Self::unroll_short_circuit(
                        left.expr_item.clone(),
                        right.expr_item.clone(),
                        var_counter,
                        operator == SupportedBinaryOperators::And
                    );
                }

                let left_expr_item = left.expr_item.clone();
                let right_expr_item = right.expr_item.clone();

                let left_unroll =
                    Self::unroll_expression(left_expr_item, var_counter);
                let var_counter = left_unroll.next_free_var_id;
                let right_unroll =
                    Self::unroll_expression(right_expr_item, var_counter);
                let var_counter = right_unroll.next_free_var_id;

                let new_var = TackyVariable::new(var_counter);
                let var_counter = var_counter + 1;

                let new_binary_instruction = BinaryInstruction {
                    operator,
                    left: left_unroll.value,
                    right: right_unroll.value,
                    dst: new_var.clone(),
                    pop_context: None
                };

                let left_instructions = left_unroll.instructions;
                let right_instructions = right_unroll.instructions;
                let mut instructions = left_instructions.clone();
                instructions.extend(right_instructions.clone());
                instructions.push(new_binary_instruction.to_tacky_instruction());

                UnrollResult::new(
                    instructions,
                    TackyValue::Var(new_var),
                    var_counter
                ).with_annotation(AnnotationStartInstruction::new(
                    annotation_identifier,
                    bin_pop_context
                ))
            }
            ExpressionVariant::ParensWrapped(sub_expr) => {
                let inner_variant = sub_expr.expr_item;
                Self::unroll_expression(inner_variant, var_counter)
            }
        };
        unroll_result
    }
}
impl PrintableTacky for TackyInstruction {
    fn print_tacky_code(&self, depth: u64) -> String {
        match self {
            TackyInstruction::UnaryInstruction(unary) => {
                unary.print_tacky_code(depth)
            },
            TackyInstruction::BinaryInstruction(binary) => {
                binary.print_tacky_code(depth)
            },
            TackyInstruction::Return(value) => {
                let indent = TAB.repeat(depth as usize);
                let mut result = String::new();
                result.push_str(&format!("{indent}Return:\n"));
                result.push_str(&value.print_tacky_code(depth + 1));
                result
            }
            _ => {
                let indent = TAB.repeat(depth as usize);
                format!("{}Unimplemented TackyInstruction {:?}\n", indent, self)
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
        let inner_unroll = TackyInstruction::unroll_expression(expr_item, 0);

        let temp_value = inner_unroll.value;
        let mut sub_instructions = vec![];
        // TODO: add annotated instructions for function start

        sub_instructions.extend(inner_unroll.instructions);
        let return_instruction = TackyInstruction::Return(temp_value);
        sub_instructions.push(return_instruction);

        TackyFunction {
            name: function.name.clone(),
            instructions: sub_instructions,
            pop_context: function.pop_context.clone()
        }
    }
    pub fn name_to_string(&self) -> String {
        self.name.name_to_string()
    }
}
impl PrintableTacky for TackyFunction {
    fn print_tacky_code(&self, depth: u64) -> String {
        let mut result = String::new();
        let indent = TAB.repeat(depth as usize);
        result.push_str(&format!("{}TackyFunction:\n", indent));
        result.push_str(&format!("{}{TAB}Name: {}\n", indent, self.name_to_string()));
        result.push_str(&format!("{}{TAB}Instructions:\n", indent));
        for instruction in &self.instructions {
            result.push_str(&instruction.print_tacky_code(depth + 2));
        }
        result
    }
}

pub struct TackyProgram {
    pub function: TackyFunction,
    pop_context: Option<PoppedTokenContext>
}
impl TackyProgram {
    pub fn from_program(program: &ASTProgram) -> TackyProgram {
        TackyProgram {
            pop_context: program.pop_context.clone(),
            function: TackyFunction::from_function(
                &program.function
            )
        }
    }
}
impl PrintableTacky for TackyProgram {
    fn print_tacky_code(&self, depth: u64) -> String {
        let mut result = String::new();
        let indent = TAB.repeat(depth as usize);
        result.push_str(&format!("{}TackyProgram:\n", indent));
        result.push_str(&*self.function.print_tacky_code(depth + 1));
        result
    }
}

pub fn tacky_gen_from_filepath(
    file_path: &str, verbose: bool
) -> Result<TackyProgram, ParseError> {
    let parse_result = parse_from_filepath(file_path, verbose);
    if parse_result.is_err() {
        return Err(parse_result.err().unwrap());
    }
    let program = parse_result?;
    let tacky_program = TackyProgram::from_program(&program);
    Ok(tacky_program)
}

