use crate::asm_gen::asm_symbols::{AsmGenError, AsmOperand, AsmSymbol};
use crate::asm_gen::registers::SCRATCH_REGISTER;
use crate::asm_gen::helpers::{
    BufferedHashMap, DiffableHashMap, StackAllocationResult, ToStackAllocated
};

#[derive(Clone, Debug)]
pub struct MovInstruction {
    pub(crate) source: AsmOperand,
    pub(crate) destination: AsmOperand,
}
impl MovInstruction {
    pub fn new(source: AsmOperand, destination: AsmOperand) -> Self {
        MovInstruction { source, destination }
    }
}
impl AsmSymbol for MovInstruction {
    fn to_asm_code(self) -> Result<String, AsmGenError> {
        let is_src_stack_addr = self.source.is_stack_address();
        let is_src_constant = self.source.is_constant();
        let is_dst_stack_addr = self.destination.is_stack_address();
        // println!("MOV_PRE {}", format!("{:?}, {:?}", &self.source, &self.destination));

        let src_asm = self.source.to_asm_code()?;
        let dst_asm = self.destination.to_asm_code()?;

        if (is_src_stack_addr || is_src_constant) && is_dst_stack_addr {
            /*
            Apparently moving stack allocated values and constants
            directly to stack addresses is not allowed in x86-64 assembly.

            So we move the value to a scratch register first,
            then move it to the stack address.
            */
            let mut asm_code: String = String::new();
            asm_code.push_str(&format!("movl {src_asm}, {SCRATCH_REGISTER}\n"));
            asm_code.push_str(&format!("movl {SCRATCH_REGISTER}, {dst_asm}"));
            Ok(asm_code)
        } else {
            Ok(format!("mov {}, {}", src_asm, dst_asm))
        }
    }
}
impl ToStackAllocated for MovInstruction {
    fn to_stack_allocated(
        &self, stack_value: u64,
        allocations: &dyn DiffableHashMap<u64, u64>
    ) -> (Self, StackAllocationResult) {
        let mut alloc_buffer = BufferedHashMap::new(allocations);

        let (source, src_alloc_result) =
            self.source.to_stack_allocated(stack_value, alloc_buffer.get_source_ref());
        let stack_value = src_alloc_result.new_stack_value;
        alloc_buffer.apply_changes(src_alloc_result.new_stack_allocations).unwrap();

        let (destination, dst_alloc_result) =
            self.destination.to_stack_allocated(stack_value, alloc_buffer.get_source_ref());
        let stack_value = dst_alloc_result.new_stack_value;
        alloc_buffer.apply_changes(dst_alloc_result.new_stack_allocations).unwrap();

        let new_instruction = MovInstruction { source, destination };
        let alloc_result = StackAllocationResult::new_with_allocations(
            stack_value,
            alloc_buffer.build_changes().to_hash_map()
        );

        (new_instruction, alloc_result)
    }
}
