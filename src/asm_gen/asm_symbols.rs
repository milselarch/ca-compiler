use std::collections::HashMap;
use crate::parser::parse::{Expression, ExpressionVariant, Identifier, Statement};
use helpers::ToStackAllocated;
use crate::constants::{STACK_VARIABLE_SIZE, TAB};
use crate::asm_gen::binary_instruction::{AsmBinaryInstruction};
use crate::asm_gen::cmp_instruction::{AsmCompareInstruction, AsmJumpConditionalInstruction, AsmSetConditionalInstruction, ConditionalCompareTypes};
use crate::asm_gen::helpers;
use crate::asm_gen::helpers::{
    AppendOnlyHashMap, BufferedHashMap, DiffableHashMap, StackAllocationResult
};
use crate::asm_gen::registers::{BASE_REGISTER, STACK_REGISTER};
use crate::asm_gen::integer_division::AsmIntegerDivision;
use crate::asm_gen::mov_instruction::MovInstruction;
use crate::asm_gen::unary_instruction::AsmUnaryInstruction;
use crate::parser::parser_helpers::{ParseError, PoppedTokenContext};
use crate::tacky::tacky_symbols::{tacky_gen_from_filepath, JumpInstruction, LabelInstruction, TackyFunction, TackyInstruction, TackyProgram, TackyValue, TackyVariable};

#[derive(Debug)]
pub enum AsmGenError {
    InvalidInstructionType(String),
    UnsupportedInstruction(String),
    ParseError(ParseError)
}

pub trait AsmSymbol {
    fn to_asm_code(self) -> Result<String, AsmGenError>;
}
pub trait HasPopContexts: Clone {
    fn _get_pop_contexts(&self) -> &Vec<PoppedTokenContext>;
    fn _add_pop_context(&mut self, pop_context: PoppedTokenContext);
    fn _add_pop_context_opt(&mut self, pop_context: Option<PoppedTokenContext>) {
        if let Some(pop_context) = pop_context {
            self._add_pop_context(pop_context);
        }
    }
    fn with_added_pop_context(self, pop_context: Option<PoppedTokenContext>) -> Self {
        let mut new = self.clone();
        new._add_pop_context_opt(pop_context);
        new
    }

    fn contexts_to_string(&self) -> String {
        let contexts = self._get_pop_contexts();
        contexts.iter().map(|c| {
            format!(
                "// TOKEN_RANGE[{}, {}], SOURCE_RANGE[{}, {}]",
                c.start_token_position, c.end_token_position,
                c.start_source_position, c.end_source_position
            )
        }).collect::<Vec<String>>().join("\n") + "\n"
    }
}

pub struct AsmProgram {
    pub(crate) function: AsmFunction,
}
impl AsmProgram {
    pub fn new(function: AsmFunction) -> AsmProgram {
        AsmProgram { function }
    }
    pub fn from_tacky_program(
        tacky_program: TackyProgram
    ) -> Self {
        Self::new(AsmFunction::from_tacky_function(tacky_program.function))
    }
    fn _to_asm_code(self) -> Result<String, AsmGenError> {
        let mut code = self.function.to_asm_code()?;
        code.push_str(".section .note.GNU-stack,\"\",@progbits\n");
        Ok(code)
    }
}
impl AsmSymbol for AsmProgram {
    fn to_asm_code(self) -> Result<String, AsmGenError> {
        let stack_alloc_map: AppendOnlyHashMap<u64, u64> =
            AppendOnlyHashMap::new();
        let stack_allocated_program =
            self.to_stack_allocated(0, &stack_alloc_map).0;
        Ok(stack_allocated_program._to_asm_code()?)
    }
}
impl ToStackAllocated for AsmProgram {
    fn to_stack_allocated(
        &self, stack_value: u64,
        allocations: &dyn DiffableHashMap<u64, u64>
    ) -> (Self, StackAllocationResult) {
        let (new_function, alloc_result) =
            self.function.to_stack_allocated(stack_value, allocations);
        let new_program = AsmProgram {
            function: new_function,
        };

        (new_program, alloc_result)
    }}

#[derive(Clone, Debug)]
pub struct AsmFunction {
    pub(crate) name: String,
    pub(crate) instructions: Vec<AsmInstruction>,
    pub(crate) pop_contexts: Vec<PoppedTokenContext>,
}
impl AsmFunction {
    pub fn new(name: String) -> AsmFunction {
        AsmFunction {
            name,
            instructions: vec![],
            pop_contexts: vec![],
        }
    }
    pub fn add_instruction(&mut self, instruction: AsmInstruction) {
        self.instructions.push(instruction);
    }
    pub fn add_instructions(
        mut self, instructions: Vec<AsmInstruction>
    ) -> AsmFunction {
        self.instructions = instructions;
        self
    }
    pub fn from_tacky_function(
        tacky_function: TackyFunction
    ) -> AsmFunction {
        let mut asm_function = AsmFunction::new(tacky_function.name_to_string());
        for tacky_instruction in tacky_function.instructions {
            let asm_instructions =
                AsmInstruction::from_tacky_instruction(tacky_instruction);
            asm_function.instructions.extend(asm_instructions);
        }
        asm_function
    }
}
impl HasPopContexts for AsmFunction {
    fn _get_pop_contexts(&self) -> &Vec<PoppedTokenContext> {
        &self.pop_contexts
    }
    fn _add_pop_context(&mut self, pop_context: PoppedTokenContext) {
        self.pop_contexts.push(pop_context);
    }
}
impl AsmSymbol for AsmFunction {
    fn to_asm_code(self) -> Result<String, AsmGenError> {
        /*
        TODO: Should there be an extra layer for abstracted
            assembly instructions to architecturally specific ones?
        */
        let mut code = "".to_string();
        // println!("ASM_INSTRUCTIONS: {:?}", self.instructions);

        code.push_str(&format!("{TAB}.globl {}", self.name));
        code.push_str(&*self.contexts_to_string());
        code.push_str(&format!("{}:\n", self.name));

        code.push_str(&format!("{TAB}pushq {BASE_REGISTER}\n"));
        code.push_str(&format!("{TAB}movq {STACK_REGISTER}, {BASE_REGISTER}\n"));

        for instruction in self.instructions {
            let inner_code = &instruction.to_asm_code()?;
            let indented_inner_code = indent::indent_all_with(TAB, inner_code);
            // println!("Indented inner code: {}", indented_inner_code);
            code.push_str(&*indented_inner_code);
            code.push_str("\n");
        }

        Ok(code)
    }
}
impl ToStackAllocated for AsmFunction {
    fn to_stack_allocated(
        &self, stack_value: u64,
        allocations: &dyn DiffableHashMap<u64, u64>
    ) -> (Self, StackAllocationResult) {
        let mut alloc_buffer = BufferedHashMap::new(allocations);
        let mut new_instructions = vec![];
        let mut new_stack_value = stack_value;

        for instruction in &self.instructions {
            let (new_instruction, instruction_alloc_result) =
                instruction.to_stack_allocated(new_stack_value, &alloc_buffer);
            new_instructions.push(new_instruction);

            new_stack_value = instruction_alloc_result.new_stack_value;
            alloc_buffer.apply_changes(
                instruction_alloc_result.new_stack_allocations
            ).unwrap();
        }

        let new_function = AsmFunction {
            name: self.name.clone(),
            instructions: new_instructions,
            pop_contexts: self.pop_contexts.clone(),
        };
        let new_stack_allocations =
            alloc_buffer.build_changes().to_hash_map();
        let func_alloc_result =
            StackAllocationResult::new_with_allocations(new_stack_value, new_stack_allocations);

        (new_function, func_alloc_result)
    }
}

#[derive(Clone, Debug)]
pub enum Register {
    EAX, // division quotient register 1 + division result register
    EDX, // division quotient register 2 + division remainder register
    R10D, // scratch register
    R11D,
}
impl AsmSymbol for Register {
    fn to_asm_code(self) -> Result<String, AsmGenError> {
        match self {
            Register::EAX => Ok("%eax".to_string()),
            Register::R10D => Ok("%r10d".to_string()),
            Register::EDX => Ok("%edx".to_string()),
            Register::R11D => Ok("%r11d".to_string()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct PseudoRegister {
    pub(crate) id: u64,
    pub(crate) name: String,
    pub(crate) pop_contexts: Vec<PoppedTokenContext>,
    pub(crate) tacky_var: Option<TackyVariable>,
}
impl PartialEq for PseudoRegister {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl HasPopContexts for PseudoRegister {
    fn _get_pop_contexts(&self) -> &Vec<PoppedTokenContext> {
        &self.pop_contexts
    }
    fn _add_pop_context(&mut self, pop_context: PoppedTokenContext) {
        self.pop_contexts.push(pop_context);
    }
}
impl PseudoRegister {
    pub fn new(id: u64, name: String) -> PseudoRegister {
        PseudoRegister {
            id,
            name,
            pop_contexts: vec![],
            tacky_var: None,
        }
    }
    pub fn set_tacky_var(
        &mut self, tacky_var: TackyVariable
    ) {
        self.tacky_var = Some(tacky_var);
    }
    pub fn with_tacky_var_context(
        mut self, tacky_var: TackyVariable
    ) -> PseudoRegister {
        let mut cloned = self.clone();
        cloned.set_tacky_var(tacky_var);
        cloned
    }

    pub fn from_tacky_var(tacky_var: TackyVariable) -> PseudoRegister {
        let cloned_var = tacky_var.clone();
        let mut pseudo_register = PseudoRegister::new(tacky_var.id, tacky_var.name);
        pseudo_register.set_tacky_var(cloned_var);
        pseudo_register
    }
}

#[derive(Clone, Debug)]
pub struct AnnotationInstruction {
    pub label: String,
    pub pop_context: Option<PoppedTokenContext>
}
impl AnnotationInstruction {
    pub fn new(
        label: String,
        pop_context: Option<PoppedTokenContext>
    ) -> Self {
        AnnotationInstruction { label, pop_context }
    }
    pub fn to_asm_instruction(self) -> AsmInstruction {
        AsmInstruction::Annotation(self)
    }
    pub fn to_asm_code(self) -> Result<String, AsmGenError> {
        let mut code = String::new();
        // TODO: support multi-line labels?
        assert!(!self.label.contains('\n'), "Annotation label cannot contain newlines");
        code.push_str(format!("# {}", self.label).as_str());

        if let Some(pop_context) = self.pop_context {
            code.push_str("\n");
            let pop_context_str = pop_context.format_as_string();
            code.push_str(format!("# {}", pop_context_str).as_str());
        }
        Ok(code)
    }
}

#[derive(Clone, Debug)]
pub enum AsmInstruction {
    Mov(MovInstruction),
    Unary(AsmUnaryInstruction),
    Binary(AsmBinaryInstruction),
    Compare(AsmCompareInstruction),
    IntegerDivision(AsmIntegerDivision), // CDQ in textbook
    Jump(AsmJumpInstruction),
    JumpConditional(AsmJumpConditionalInstruction),
    SetConditional(AsmSetConditionalInstruction),
    SignExtension,
    AllocateStack(StackAllocation),
    LabelInstruction(AsmLabelInstruction),
    Annotation(AnnotationInstruction),
    Ret,
}
impl AsmSymbol for AsmInstruction {
    fn to_asm_code(self) -> Result<String, AsmGenError> {
        match self {
            AsmInstruction::Mov(mov_instruction) => {
                Ok(mov_instruction.to_asm_code()?)
            },
            AsmInstruction::Unary(unary_instruction) => {
                Ok(unary_instruction.to_asm_code()?)
            },
            AsmInstruction::Binary(binary_instruction) => {
                Ok(binary_instruction.to_asm_code()?)
            }
            AsmInstruction::AllocateStack(stack_allocation) => {
                Ok(stack_allocation.to_asm_code()?)
            },
            AsmInstruction::IntegerDivision(int_div_instruction) => {
                Ok(int_div_instruction.to_asm_code()?)
            },
            AsmInstruction::SignExtension => {
                Ok("cdq".parse().unwrap())
            }
            AsmInstruction::Ret => {
                let mut code = String::new();
                code.push_str(&format!("movq {BASE_REGISTER}, {STACK_REGISTER}\n"));
                code.push_str(&format!("popq {BASE_REGISTER}\n"));
                code.push_str("ret\n");
                Ok(code.to_string())
            },
            AsmInstruction::Annotation(annotation) => {
                Ok(annotation.to_asm_code()?)
            },
            _ => {
                Err(AsmGenError::InvalidInstructionType(
                    format!(
                        "AsmInstruction variant not implemented for to_asm_code: {:?}",
                        self
                    )
                ))
            }
        }
    }
}
impl AsmInstruction {
    pub fn from_tacky_instruction(
        tacky_instruction: TackyInstruction
    ) -> Vec<Self> {
        match tacky_instruction {
            TackyInstruction::Return(tacky_value) => {
                let src_operand = match tacky_value {
                    TackyValue::Constant(ast_constant) => {
                        let value = ast_constant.to_u64().unwrap();
                        let asm_value = AsmImmediateValue::new(value)
                            .with_added_pop_context(ast_constant.pop_context.clone());
                        AsmOperand::ImmediateValue(asm_value)
                    },
                    TackyValue::Var(tacky_var) => {
                        // Handle variable return case
                        AsmOperand::Pseudo(PseudoRegister::from_tacky_var(tacky_var))
                    },
                };
                let dst_operand = AsmOperand::Register(Register::EAX);
                let mov_instruction = MovInstruction::new(src_operand, dst_operand);
                vec![
                    AsmInstruction::Mov(mov_instruction),
                    AsmInstruction::Ret
                ]
            },
            TackyInstruction::UnaryInstruction(unary_instruction) => {
                let src_operand = AsmOperand::from_tacky_value(unary_instruction.src);
                let dst_operand = AsmOperand::from_tacky_value(
                    TackyValue::Var(unary_instruction.dst)
                );
                let asm_mov_instruction = MovInstruction::new(
                    src_operand, dst_operand.clone()
                );
                let asm_unary_instruction = AsmUnaryInstruction {
                    operator: unary_instruction.operator,
                    destination: dst_operand
                };
                vec![
                    AsmInstruction::Mov(asm_mov_instruction),
                    AsmInstruction::Unary(asm_unary_instruction)
                ]
            },
            TackyInstruction::BinaryInstruction(binary_instruction) => {
                AsmBinaryInstruction::unpack_from_tacky(binary_instruction)
            },
            TackyInstruction::LabelInstruction(label_instruction) => {
                AsmLabelInstruction::unpack_from_tacky(label_instruction)
            }
            TackyInstruction::CopyInstruction(copy_instruction) => {
                let src_operand =
                    AsmOperand::from_tacky_value(copy_instruction.src);
                let dst_operand =
                    AsmOperand::from_tacky_value(copy_instruction.dst.to_tacky_value());

                let asm_mov_instruction = MovInstruction::new(src_operand, dst_operand);
                vec![AsmInstruction::Mov(asm_mov_instruction)]
            }
            TackyInstruction::JumpInstruction(jump_instruction) => {
                AsmJumpInstruction::unpack_from_tacky(jump_instruction)
            },
            TackyInstruction::JumpIfZeroInstruction(jump_if_zero_instruction) => {
                // check if 0 == condition's value
                let cmp_instruction = AsmCompareInstruction::new(
                    AsmOperand::ImmediateValue(AsmImmediateValue::new(0)),
                    AsmOperand::from_tacky_value(jump_if_zero_instruction.condition)
                );
                let cond_jump_instruction = AsmJumpConditionalInstruction::new(
                    jump_if_zero_instruction.target,
                    ConditionalCompareTypes::Equal
                );
                vec![
                    AsmInstruction::Compare(cmp_instruction),
                    AsmInstruction::JumpConditional(cond_jump_instruction)
                ]
            }
            TackyInstruction::JumpIfNotZeroInstruction(jump_if_not_zero_instruction) => {
                // check if 0 == condition's value
                let cmp_instruction = AsmCompareInstruction::new(
                    AsmOperand::ImmediateValue(AsmImmediateValue::new(0)),
                    AsmOperand::from_tacky_value(jump_if_not_zero_instruction.condition)
                );
                let cond_jump_instruction = AsmJumpConditionalInstruction::new(
                    jump_if_not_zero_instruction.target,
                    ConditionalCompareTypes::NotEqual
                );
                vec![
                    AsmInstruction::Compare(cmp_instruction),
                    AsmInstruction::JumpConditional(cond_jump_instruction)
                ]
            }
            TackyInstruction::AnnotationStartInstruction(annotation_start) => {
                let label = format!(">>> {}", annotation_start.label.name);
                let asm_instruction = AnnotationInstruction::new(
                    label, annotation_start.pop_context.clone()
                ).to_asm_instruction();
                vec![asm_instruction]
            },
            TackyInstruction::AnnotationEndInstruction(annotation_end) => {
                let label = format!("<<< {}", annotation_end.label.name);
                let asm_instruction =
                    AnnotationInstruction::new(label, None).to_asm_instruction();
                vec![asm_instruction]
            }
        }
    }
}
impl ToStackAllocated for AsmInstruction {
    fn to_stack_allocated(
        &self, stack_value: u64,
        allocations: &dyn DiffableHashMap<u64, u64>
    ) -> (Self, StackAllocationResult) {
        match self {
            AsmInstruction::Mov(mov_instruction) => {
                let (new_mov_instruction, alloc_result) =
                    mov_instruction.to_stack_allocated(stack_value, allocations);
                (AsmInstruction::Mov(new_mov_instruction), alloc_result)
            },
            AsmInstruction::Unary(unary_instruction) => {
                let (new_unary_instruction, alloc_result) =
                    unary_instruction.to_stack_allocated(stack_value, allocations);
                (AsmInstruction::Unary(new_unary_instruction), alloc_result)
            },
            AsmInstruction::Binary(binary_instruction) => {
                let (new_binary_instruction, alloc_result) =
                    binary_instruction.to_stack_allocated(stack_value, allocations);
                (AsmInstruction::Binary(new_binary_instruction), alloc_result)
            },
            AsmInstruction::IntegerDivision(int_div_instruction) => {
                let (new_int_div_instruction, alloc_result) =
                    int_div_instruction.to_stack_allocated(stack_value, allocations);
                (AsmInstruction::IntegerDivision(new_int_div_instruction), alloc_result)
            },
            AsmInstruction::AllocateStack(stack_allocation) => {
                // Stack allocation is not needed, pass through
                let clone = AsmInstruction::AllocateStack(stack_allocation.clone());
                (clone, StackAllocationResult::new(stack_value))
            }
            AsmInstruction::SignExtension => {
                // Sign extension does not affect stack allocations
                (self.clone(), StackAllocationResult::new(stack_value))
            },
            AsmInstruction::Ret => {
                // Return does not affect stack allocations
                (self.clone(), StackAllocationResult::new(stack_value))
            },
            AsmInstruction::Compare(cmp_instruction) => {
                // TODO: refactor this out to its own ToStackAllocated impl
                let (left, left_alloc_result) =
                    cmp_instruction.left.to_stack_allocated(stack_value, allocations);
                let stack_value = left_alloc_result.new_stack_value;
                let (right, right_alloc_result) =
                    cmp_instruction.right.to_stack_allocated(stack_value, allocations);
                let stack_value = right_alloc_result.new_stack_value;

                let new_cmp_instruction = AsmCompareInstruction { left, right };
                let mut alloc_buffer = BufferedHashMap::new(allocations);
                alloc_buffer.apply_changes(left_alloc_result.new_stack_allocations).unwrap();
                alloc_buffer.apply_changes(right_alloc_result.new_stack_allocations).unwrap();

                let alloc_result = StackAllocationResult::new_from_buffered(
                    stack_value, alloc_buffer
                );
                (AsmInstruction::Compare(new_cmp_instruction), alloc_result)
            }
            AsmInstruction::Jump(_jump_instruction) => {
                // Jump does not affect stack allocations
                (self.clone(), StackAllocationResult::new(stack_value))
            }
            AsmInstruction::JumpConditional(_cond_jump_instruction) => {
                // Jump conditional does not affect stack allocations
                (self.clone(), StackAllocationResult::new(stack_value))
            }
            AsmInstruction::SetConditional(set_cond_instruction) => {
                // TODO: refactor this out to its own ToStackAllocated impl
                let (destination, dest_alloc_result) =
                    set_cond_instruction.destination.to_stack_allocated(
                        stack_value, allocations
                    );
                let stack_value = dest_alloc_result.new_stack_value;

                let new_set_cond_instruction = AsmSetConditionalInstruction {
                    destination,
                    condition: set_cond_instruction.condition.clone(),
                };
                let mut alloc_buffer = BufferedHashMap::new(allocations);
                alloc_buffer.apply_changes(dest_alloc_result.new_stack_allocations).unwrap();
                let alloc_result = StackAllocationResult::new_from_buffered(
                    stack_value, alloc_buffer
                );
                (AsmInstruction::SetConditional(new_set_cond_instruction), alloc_result)
            }
            AsmInstruction::LabelInstruction(_label_instruction) => {
                (self.clone(), StackAllocationResult::new(stack_value))
            },
            AsmInstruction::Annotation(_annotation_instruction) => {
                (self.clone(), StackAllocationResult::new(stack_value))
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct AsmLabelInstruction {
    pub identifier: Identifier
}
impl AsmLabelInstruction {
    pub fn new(identifier: Identifier) -> Self {
        AsmLabelInstruction { identifier }
    }
    pub fn unpack_from_tacky(
        label_instruction: LabelInstruction
    ) -> Vec<AsmInstruction> {
        let label_instruction = AsmLabelInstruction::new(label_instruction.label);
        vec![AsmInstruction::LabelInstruction(label_instruction)]
    }
}

#[derive(Clone, Debug)]
pub struct AsmJumpInstruction {
    pub identifier: Identifier
}
impl AsmJumpInstruction {
    pub fn new(identifier: Identifier) -> Self {
        AsmJumpInstruction { identifier }
    }
    pub fn unpack_from_tacky(
        jump_instruction: JumpInstruction
    ) -> Vec<AsmInstruction> {
        let asm_jump_instruction = AsmJumpInstruction::new(jump_instruction.target);
        vec![AsmInstruction::Jump(asm_jump_instruction)]
    }
}

#[derive(Clone, Debug)]
pub struct StackAllocation {
    pub(crate) offset: u64,
    pub(crate) offset_size: u64,
    pub(crate) pop_contexts: Vec<PoppedTokenContext>,
    pub(crate) tacky_var: Option<TackyVariable>,
}
impl AsmSymbol for StackAllocation {
    fn to_asm_code(self) -> Result<String, AsmGenError> {
        Ok(format!("subq ${}, {STACK_REGISTER}", self.offset))
    }
}

#[derive(Clone, Debug)]
pub struct FromPseudoRegisterResult {
    pub stack_address: StackAddress,
    pub newly_allocated: bool,
}

#[derive(Clone, Debug)]
pub struct StackAddress {
    pub(crate) offset: u64,
    pub(crate) offset_size: u64,
    pub(crate) pop_contexts: Vec<PoppedTokenContext>,
    pub(crate) tacky_var: Option<TackyVariable>,
}
impl StackAddress {
    pub fn new(offset: u64, offset_size: u64) -> Self {
        StackAddress {
            offset, offset_size, pop_contexts: vec![],
            tacky_var: None
        }
    }
    pub fn from_pseudo_register(
        pseudo_register: &PseudoRegister, current_stack_offset: u64,
        offset_size: u64, existing_allocations: &dyn DiffableHashMap<u64, u64>
    ) -> FromPseudoRegisterResult {
        /*
        Returns the StackAddress ASM Symbol and a boolean indicating
        whether the pseudo register has been newly allocated to the stack in the current call.
        */
        let existing_allocation = existing_allocations.get(&pseudo_register.id);
        let stack_value = match existing_allocation {
            Some(&addr) => addr,
            None => current_stack_offset
        };

        let stack_address = StackAddress {
            offset: stack_value,
            offset_size,
            pop_contexts: pseudo_register.pop_contexts.clone(),
            tacky_var: pseudo_register.tacky_var.clone(),
        };
        let newly_allocated = existing_allocation.is_none();
        FromPseudoRegisterResult {
            stack_address,
            newly_allocated
        }
    }
}
impl AsmSymbol for StackAddress {
    fn to_asm_code(self) -> Result<String, AsmGenError> {
        Ok(format!("-{}({BASE_REGISTER})", self.offset))
    }
}


#[derive(Clone, Debug)]
pub enum AsmOperand {
    ImmediateValue(AsmImmediateValue),
    Register(Register),
    Pseudo(PseudoRegister),
    Stack(StackAddress)
}
impl AsmSymbol for AsmOperand {
    fn to_asm_code(self) -> Result<String, AsmGenError> {
        match self {
            AsmOperand::ImmediateValue(value) => {
                Ok(value.to_asm_code()?)
            },
            AsmOperand::Register(register) => {
                Ok(register.to_asm_code()?)
            },
            AsmOperand::Pseudo(pseudo_register) => {
                Err(AsmGenError::InvalidInstructionType(
                    format!(
                        "Pseudo register [{:?}] not supported in assembly",
                        pseudo_register
                    )
                ))
            },
            AsmOperand::Stack(stack_address) => {
                Ok(stack_address.to_asm_code()?)
            }
        }
    }
}
impl AsmOperand {
    pub fn is_stack_address(&self) -> bool {
        matches!(self, AsmOperand::Stack(_))
    }
    pub fn is_constant(&self) -> bool {
        matches!(self, AsmOperand::ImmediateValue(_))
    }
    pub fn from_tacky_value(tacky_value: TackyValue) -> Self {
        match tacky_value {
            TackyValue::Constant(ast_constant) => {
                let value = ast_constant.to_u64().unwrap();
                AsmOperand::ImmediateValue(AsmImmediateValue::new(value)
                    .with_added_pop_context(ast_constant.pop_context.clone()))
            },
            TackyValue::Var(tacky_var) => {
                AsmOperand::Pseudo(PseudoRegister::from_tacky_var(tacky_var))
            },
        }
    }
}
impl ToStackAllocated for AsmOperand {
    fn to_stack_allocated(
        &self, stack_value: u64,
        allocations: &dyn DiffableHashMap<u64, u64>
    ) -> (Self, StackAllocationResult) {
        /*
        Converts the AsmOperand to a stack allocation if it is a pseudo register.
        returns a tuple containing the new AsmOperand and a boolean indicating
        whether it was converted to a stack allocation.
        */
        match self {
            AsmOperand::Pseudo(pseudo_register) => {
                let conversion = StackAddress::from_pseudo_register(
                    pseudo_register, stack_value, STACK_VARIABLE_SIZE,
                    allocations
                );

                let mut new_stack_value = stack_value;
                let mut new_allocations: HashMap<u64, u64> = HashMap::new();
                if conversion.newly_allocated {
                    new_stack_value += STACK_VARIABLE_SIZE;
                    new_allocations.insert(pseudo_register.id, stack_value);
                }
                let stack_alloc_result = StackAllocationResult::new_with_allocations(
                    new_stack_value, new_allocations
                );

                let new_instruction = AsmOperand::Stack(conversion.stack_address);
                (new_instruction, stack_alloc_result)
            },
            other => {
                (other.clone(), StackAllocationResult::new(stack_value) )
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct AsmImmediateValue {
    pub(crate) value: u64,
    pub(crate) pop_contexts: Vec<PoppedTokenContext>
}
impl AsmImmediateValue {
    pub fn new(value: u64) -> AsmImmediateValue {
        AsmImmediateValue {
            value,
            pop_contexts: vec![]
        }
    }

    pub fn from_expression(expr: Expression) -> Self {
        match expr.expr_item {
            ExpressionVariant::Constant(ref constant) => {
                let value = constant.to_u64().unwrap();
                AsmImmediateValue::new(value).with_added_pop_context(
                    expr.pop_context.clone()
                )
            },
            ExpressionVariant::UnaryOperation(_, _) => {
                panic!("Unary operations not implemented yet");
            },
            _ => {
                panic!("Unsupported expression type for AsmImmediateValue");
            }
        }
    }

    pub fn from_statement(statement: Statement) -> Self {
        Self::from_expression(statement.expression)
    }
}
impl HasPopContexts for AsmImmediateValue {
    fn _get_pop_contexts(&self) -> &Vec<PoppedTokenContext> {
        &self.pop_contexts
    }
    fn _add_pop_context(&mut self, pop_context: PoppedTokenContext) {
        self.pop_contexts.push(pop_context);
    }
}
impl AsmSymbol for AsmImmediateValue {
    fn to_asm_code(self) -> Result<String, AsmGenError> {
        Ok(format!("${}", self.value))
    }
}

pub fn asm_gen_from_filepath(
    file_path: &str, verbose: bool
) -> Result<AsmProgram, ParseError> {
    let tacky_program = tacky_gen_from_filepath(file_path, verbose)?;
    let asm_program = AsmProgram::from_tacky_program(tacky_program);
    Ok(asm_program)
}

#[cfg(test)]
mod tests {
    use crate::asm_gen::asm_symbols::AsmSymbol;

    #[test]
    fn test_chapter_3_valid_sub() {
        let file_path = "./writing-a-c-compiler-tests/tests/chapter_3/valid/sub_neg.c";
        let asm_program = super::asm_gen_from_filepath(file_path, true).unwrap();
        let _asm_code = asm_program.to_asm_code().unwrap();
    }
    #[test]
    fn test_chapter_3_valid_precedence() {
        let file_path = "./writing-a-c-compiler-tests/tests/chapter_3/valid/precedence.c";
        let asm_program = super::asm_gen_from_filepath(file_path, true).unwrap();
        let _asm_code = asm_program.to_asm_code().unwrap();
    }
    #[test]
    fn test_chapter_3_sub_neg() {
        let file_path = "./writing-a-c-compiler-tests/tests/chapter_3/valid/sub_neg.c";
        let asm_program = super::asm_gen_from_filepath(file_path, true).unwrap();
        let _asm_code = asm_program.to_asm_code().unwrap();
    }
}
