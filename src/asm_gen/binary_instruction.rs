use std::cmp::PartialEq;
use crate::asm_gen::asm_symbols::{AsmGenError, AsmImmediateValue, AsmInstruction, AsmOperand, AsmSymbol, Register};
use crate::asm_gen::cmp_instruction::{AsmCompareInstruction, AsmSetConditionalInstruction, ConditionalCompareTypes};
use crate::asm_gen::registers::{DST_SCRATCH_REGISTER, SCRATCH_REGISTER};
use crate::asm_gen::helpers::{
    BufferedHashMap, DiffableHashMap, StackAllocationResult,
    ToStackAllocated
};
use crate::asm_gen::integer_division::AsmIntegerDivision;
use crate::asm_gen::mov_instruction::MovInstruction;
use crate::parser::parse::SupportedBinaryOperators;
use crate::tacky::tacky_symbols::{BinaryInstruction, TackyValue};

// Note: only handles binary operations that can be represented in 1 line
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AsmDirectBinaryOperators {
    Add,
    Subtract,
    Multiply
}
impl AsmDirectBinaryOperators {
    pub fn to_asm_string(&self) -> String {
        match self {
            AsmDirectBinaryOperators::Add => "addl".to_string(),
            AsmDirectBinaryOperators::Subtract => "subl".to_string(),
            AsmDirectBinaryOperators::Multiply => "imull".to_string(),
        }
    }
    pub fn from_supported(op: SupportedBinaryOperators) -> Result<Self, AsmGenError> {
        match op {
            SupportedBinaryOperators::Add => Ok(AsmDirectBinaryOperators::Add),
            SupportedBinaryOperators::Subtract => Ok(AsmDirectBinaryOperators::Subtract),
            SupportedBinaryOperators::Multiply => Ok(AsmDirectBinaryOperators::Multiply),
            _ => Err(AsmGenError::UnsupportedInstruction(
                format!("Unsupported binary operator: {:?}", op))
            ),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum DivisionOutputs {
    Quotient,
    Remainder
}

impl DivisionOutputs {
    pub fn from_binary_operator(
        op: SupportedBinaryOperators
    ) -> Result<Self, AsmGenError> {
        match op {
            SupportedBinaryOperators::Divide => Ok(DivisionOutputs::Quotient),
            SupportedBinaryOperators::Modulo => Ok(DivisionOutputs::Remainder),
            _ => Err(AsmGenError::UnsupportedInstruction(
                format!("Unsupported division output operator: {:?}", op)
            )),
        }
    }
}

#[derive(Clone, Debug)]
pub struct AsmBinaryInstruction {
    pub(crate) operator: AsmDirectBinaryOperators,
    pub(crate) source: AsmOperand,
    pub(crate) destination: AsmOperand,
}
impl AsmBinaryInstruction {
    pub fn build_divide_instructions(
        left_operand: AsmOperand,
        right_operand: AsmOperand,
        dst_operand: AsmOperand,
        desired_output: DivisionOutputs
    ) -> Vec<AsmInstruction> {
        // Move left operand into EAX (division input register)
        let move_into_instruction = MovInstruction::new(
            left_operand.clone(), AsmOperand::Register(Register::EAX)
        );
        let output_register = match desired_output {
            DivisionOutputs::Quotient => AsmOperand::Register(Register::EAX),
            DivisionOutputs::Remainder => AsmOperand::Register(Register::EDX),
        };
        // move division output into dst operand
        let move_out_instruction = MovInstruction::new(
            output_register, dst_operand.clone()
        );
        vec![
            AsmInstruction::Mov(move_into_instruction),
            AsmInstruction::SignExtension,
            AsmInstruction::IntegerDivision(
                AsmIntegerDivision::new(right_operand.clone())
            ),
            AsmInstruction::Mov(move_out_instruction)
        ]
    }
    pub fn build_comparison_instructions(
        left_operand: AsmOperand,
        right_operand: AsmOperand,
        dst_operand: AsmOperand,
        comparison_flag: ConditionalCompareTypes
    ) -> Vec<AsmInstruction> {
        /*
        Binary(CMP_FLAG, left_operand, right_operand, dst_operand)
        translates to
        ----------------------------
        Cmp(right_operand, left_operand)
        // clear all bits in dst_operand (SetCC does not zero all bits)
        Mov(0, dst_operand)
        // set comparison flag in dst_operand (does not write all bits)
        SetCC(CMP_FLAG, dst_operand)
        */
        let cmp_instruction = AsmInstruction::Compare(AsmCompareInstruction::new(
            right_operand, left_operand
        ));
        let clear_dst_instruction = MovInstruction::new(
            AsmOperand::ImmediateValue(AsmImmediateValue::new(0)), dst_operand.clone()
        );
        let set_cc_instruction = AsmInstruction::SetConditional(
            AsmSetConditionalInstruction::new(dst_operand, comparison_flag)
        );
        vec![
            cmp_instruction,
            AsmInstruction::Mov(clear_dst_instruction),
            set_cc_instruction
        ]
    }

    pub fn unpack_from_tacky(binary_instruction: BinaryInstruction) -> Vec<AsmInstruction> {
        /*
        TACKY:
        ----------------------------
        Binary(op, src1, src2, dst)
        ----------------------------
        */
        let operator = binary_instruction.operator;
        let left_operand = AsmOperand::from_tacky_value(binary_instruction.left);
        let right_operand = AsmOperand::from_tacky_value(binary_instruction.right.clone());
        let dst_operand = AsmOperand::from_tacky_value(
            TackyValue::Var(binary_instruction.dst)
        );

        match DivisionOutputs::from_binary_operator(operator) {
            Ok(division_operator) => {
                return Self::build_divide_instructions(
                    left_operand, right_operand, dst_operand,
                    division_operator
                );
            }
            _ => {}
        }

        match ConditionalCompareTypes::convert_from(operator) {
            Ok(comparison_operator) => {
                return Self::build_comparison_instructions(
                    left_operand, right_operand, dst_operand,
                    comparison_operator
                );
            }
            _ => {}
        }

        /*
        ASM:
        ----------------------------
        Mov(src1, dst)
        Binary(op, src2, dst)

        ASM instruction applies op to dst using src2
        and stores result in dst
        */
        let asm_binary_operator =
            AsmDirectBinaryOperators::from_supported(operator).unwrap();
        let asm_mov_instruction = MovInstruction::new(
            left_operand.clone(), dst_operand.clone()
        );

        let asm_binary_instruction = AsmBinaryInstruction {
            operator: asm_binary_operator,
            source: right_operand,
            destination: dst_operand
        };
        vec![
            AsmInstruction::Mov(asm_mov_instruction),
            AsmInstruction::Binary(asm_binary_instruction)
        ]
    }
}
impl ToStackAllocated for AsmBinaryInstruction {
    fn to_stack_allocated(
        &self, stack_value: u64,
        allocations: &dyn DiffableHashMap<u64, u64>
    ) -> (Self, StackAllocationResult) {
        let mut alloc_buffer = BufferedHashMap::new(allocations);

        let (source, src_alloc_result) =
            self.source.to_stack_allocated(stack_value, allocations);
        let stack_value = src_alloc_result.new_stack_value;
        alloc_buffer.apply_changes(src_alloc_result.new_stack_allocations).unwrap();

        let (destination, dest_alloc) =
            self.destination.to_stack_allocated(stack_value, allocations);
        let stack_value = dest_alloc.new_stack_value;
        alloc_buffer.apply_changes(dest_alloc.new_stack_allocations).unwrap();

        let new_instruction = AsmBinaryInstruction {
            operator: self.operator.clone(),
            source,
            destination,
        };
        let alloc_result =
            StackAllocationResult::new_from_buffered(stack_value, alloc_buffer);
        (new_instruction, alloc_result)
    }
}

fn generate_multiply_asm(src_asm: String, dst_asm: String) -> String {
    /*
    multiplication operations in x86-64 assembly will
    modify the destination operand inplace.
    */
    let mut asm_code: String = String::new();
    // move destination to multiply scratch register first
    asm_code.push_str(&format!("movl {dst_asm}, {DST_SCRATCH_REGISTER}\n"));

    let operator_asm = AsmDirectBinaryOperators::Multiply.to_asm_string();
    asm_code.push_str(&format!(
        "{} {}, {}\n",
        operator_asm, src_asm, DST_SCRATCH_REGISTER
    ));

    // move multiply scratch register (modified inplace) back to destination
    asm_code.push_str(&format!("movl {DST_SCRATCH_REGISTER}, {dst_asm}"));
    asm_code
}

impl AsmSymbol for AsmBinaryInstruction {
    fn to_asm_code(self) -> Result<String, AsmGenError> {
        /*
        e.g. addl -4(%rbp), -8(%rbp)
        */
        let operator_asm = self.operator.to_asm_string();
        let is_src_stack_addr = self.source.is_stack_address();
        let src_asm = self.source.to_asm_code()?;
        let is_dst_stack_addr = self.destination.is_stack_address();
        let dst_asm = self.destination.to_asm_code()?;

        if is_src_stack_addr && is_dst_stack_addr {
            /*
            binary asm instructions where both source and destination
            operands are stack allocated are not allowed in x86-64 assembly.
            So we move the value to a scratch register first,
            then move it to the stack address.
            */
            // TODO: maybe a new layer for asm rewrites would be cleaner
            let mut asm_code: String = String::new();
            asm_code.push_str(&format!("movl {src_asm}, {SCRATCH_REGISTER}\n"));

            if self.operator == AsmDirectBinaryOperators::Multiply {
                asm_code.push_str(generate_multiply_asm(
                    SCRATCH_REGISTER.to_string(), dst_asm
                ).as_str())
            } else {
                asm_code.push_str(&format!(
                    "{} {}, {}",
                    operator_asm, SCRATCH_REGISTER, dst_asm
                ));
            }
            Ok(asm_code)
        } else {
            if self.operator == AsmDirectBinaryOperators::Multiply {
                Ok(generate_multiply_asm(src_asm, dst_asm))
            } else {
                Ok(format!("{} {}, {}", operator_asm, src_asm, dst_asm))
            }
        }
    }
}
