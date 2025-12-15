use crate::asm_gen::asm_symbols::{AsmGenError, AsmImmediateValue, AsmInstruction, AsmOperand, AsmSymbol};
use crate::asm_gen::cmp_instruction::{AsmCompareInstruction, AsmSetConditionalInstruction, ConditionalCompareTypes};
use crate::asm_gen::helpers::{DiffableHashMap, StackAllocationResult, ToStackAllocated};
use crate::asm_gen::mov_instruction::MovInstruction;
use crate::parser::parse::SupportedUnaryOperators;
use crate::tacky::tacky_symbols::{TackyValue, UnaryInstruction};

#[derive(Clone, Debug)]
pub struct AsmUnaryInstruction {
    pub(crate) operator: SupportedUnaryOperators,
    pub(crate) destination: AsmOperand,
}
impl AsmUnaryInstruction {
    fn operator_to_asm_string(
        operator: SupportedUnaryOperators
    ) -> Result<String, AsmGenError> {
        match operator {
            SupportedUnaryOperators::Subtract => Ok("negl".to_string()),
            SupportedUnaryOperators::BitwiseNot => Ok("notl".to_string()),
            _ => Err(AsmGenError::UnsupportedInstruction(
                format!("Unsupported unary operator: {:?}", operator)
            )),
        }
    }
    pub fn unpack_from_tacky(
        unary_instruction: UnaryInstruction
    ) -> Vec<AsmInstruction> {
        let src_operand = AsmOperand::from_tacky_value(unary_instruction.src);
        let dst_operand = AsmOperand::from_tacky_value(
            TackyValue::Var(unary_instruction.dst)
        );

        match unary_instruction.operator {
            SupportedUnaryOperators::Not => {
                /*
                Unary(Not, src, dst)
                ------------------------------
                translates to:
                ------------------------------
                Cmp(Imm(0), src)
                Mov(Imm(0), dst) // SetCC doesn't clear all bits (only a nibble lol)
                SetCC(E, dst)
                */
                let cmp_instruction = AsmCompareInstruction::new(
                    AsmImmediateValue::new(0).to_operand(), src_operand
                );
                let mov_instruction = MovInstruction::new(
                    AsmImmediateValue::new(0).to_operand(), dst_operand.clone()
                );
                let set_cc_instruction = AsmSetConditionalInstruction::new(
                    dst_operand, ConditionalCompareTypes::Equal
                );
                vec![
                    AsmInstruction::Compare(cmp_instruction),
                    AsmInstruction::Mov(mov_instruction),
                    AsmInstruction::SetConditional(set_cc_instruction)
                ]
            }
            SupportedUnaryOperators::BitwiseNot | SupportedUnaryOperators::Subtract => {
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
            }
        }
    }
}
impl ToStackAllocated for AsmUnaryInstruction {
    fn to_stack_allocated(
        &self, stack_value: u64,
        allocations: &dyn DiffableHashMap<u64, u64>
    ) -> (Self, StackAllocationResult) {
        let (operand, alloc_result) =
            self.destination.to_stack_allocated(stack_value, allocations);
        let new_instruction = AsmUnaryInstruction {
            operator: self.operator.clone(),
            destination: operand,
        };
        (new_instruction, alloc_result)
    }
}
impl AsmSymbol for AsmUnaryInstruction {
    fn to_asm_code(self) -> Result<String, AsmGenError> {
        let operand_asm = self.destination.to_asm_code()?;
        let operator_asm = Self::operator_to_asm_string(self.operator)?;
        Ok(format!("{} {}", operator_asm, operand_asm))
    }
}