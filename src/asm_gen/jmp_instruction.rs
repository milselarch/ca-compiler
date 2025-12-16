use crate::asm_gen::asm_symbols::{AsmGenError, AsmInstruction};
use crate::parser::parse::Identifier;
use crate::tacky::tacky_symbols::JumpInstruction;

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
    pub fn to_asm_code(self) -> Result<String, AsmGenError> {
        Ok(format!("jmp .L{}", self.identifier.name_to_string()))
    }
}