use crate::asm_gen::asm_symbols::{AsmOperand, AsmSymbol};
use crate::asm_gen::registers::SCRATCH_REGISTER;
use crate::asm_gen::helpers::{
    DiffableHashMap, StackAllocationResult, ToStackAllocated
};

#[derive(Clone, Debug)]
pub struct AsmIntegerDivision {
    operand: AsmOperand,
}
impl AsmIntegerDivision {
    pub fn new(operand: AsmOperand) -> AsmIntegerDivision {
        AsmIntegerDivision { operand }
    }
}
impl ToStackAllocated for AsmIntegerDivision {
    fn to_stack_allocated(
        &self, stack_value: u64,
        allocations: &dyn DiffableHashMap<u64, u64>
    ) -> (Self, StackAllocationResult) {
        let (operand, alloc_result) =
            self.operand.to_stack_allocated(stack_value, allocations);
        let new_instruction = AsmIntegerDivision {
            operand,
        };
        (new_instruction, alloc_result)
    }
}
impl AsmSymbol for AsmIntegerDivision {
    fn to_asm_code(self) -> Result<String, crate::asm_gen::asm_symbols::AsmGenError> {
        let is_constant = self.operand.is_constant();
        let operand_asm = self.operand.to_asm_code()?;

        if is_constant {
            let mut asm_code: String = String::new();
            asm_code.push_str(&format!("movl {operand_asm}, {SCRATCH_REGISTER}\n"));
            asm_code.push_str(&format!("idivl {SCRATCH_REGISTER}"));
            Ok(asm_code)
        } else {
            Ok(format!("idivl {}", operand_asm))
        }
    }
}