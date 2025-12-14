use crate::asm_gen::registers::{SCRATCH_REGISTER, DST_SCRATCH_REGISTER};
use crate::asm_gen::asm_symbols::{AsmGenError, AsmSymbol};
use crate::asm_gen::asm_symbols::AsmOperand;
use crate::parser::parse::{Identifier, SupportedBinaryOperators};

#[derive(Clone, Debug)]
pub enum ConditionalCompareTypes {
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
}
impl ConditionalCompareTypes {
    pub fn convert_from(
        op: SupportedBinaryOperators
    ) -> Result<Self, AsmGenError> {
        match op {
            SupportedBinaryOperators::CheckEqual => Ok(ConditionalCompareTypes::Equal),
            SupportedBinaryOperators::NotEqual => Ok(ConditionalCompareTypes::NotEqual),
            SupportedBinaryOperators::GreaterThan => Ok(ConditionalCompareTypes::GreaterThan),
            SupportedBinaryOperators::GreaterOrEqual => {
                Ok(ConditionalCompareTypes::GreaterThanOrEqual)
            }
            SupportedBinaryOperators::LessThan => Ok(ConditionalCompareTypes::LessThan),
            SupportedBinaryOperators::LessOrEqual => {
                Ok(ConditionalCompareTypes::LessThanOrEqual)
            }
            _ => Err(AsmGenError::UnsupportedInstruction(
                format!("Unsupported comparison output operator: {:?}", op)
            )),
        }
    }
}

#[derive(Clone, Debug)]
pub struct AsmCompareInstruction {
    // Cmp in the textbook
    pub left: AsmOperand,
    pub right: AsmOperand,
}
impl AsmCompareInstruction {
    pub fn new(left: AsmOperand, right: AsmOperand) -> Self {
        AsmCompareInstruction { left, right }
    }
}
impl AsmSymbol for AsmCompareInstruction {
    fn to_asm_code(self) -> Result<String, crate::asm_gen::asm_symbols::AsmGenError> {
        let is_left_stack_addr = self.left.is_stack_address();
        let is_right_stack_addr = self.right.is_stack_address();
        let is_right_constant = self.right.is_constant();

        let left_asm = self.left.to_asm_code()?;
        let right_asm = self.right.to_asm_code()?;

        if is_left_stack_addr && is_right_stack_addr {
            /*
            Apparently directly comparing one stack allocated value
            to another stack allocated value is not allowed in x86-64 assembly.

            So we move the left value to a scratch register first,
            then compare it to the right stack address.
            */
            let mut asm_code: String = String::new();
            asm_code.push_str(&format!("movl {left_asm}, {SCRATCH_REGISTER}\n"));
            asm_code.push_str(&format!("cmpl {SCRATCH_REGISTER}, {right_asm}"));
            Ok(asm_code)
        } else if is_right_constant {
            /*
            Apparently directly comparing a stack allocated value
            to a constant is not allowed in x86-64 assembly.

            So we move the constant to a scratch register first,
            then compare it to the left operand.
            */
            let mut asm_code: String = String::new();
            asm_code.push_str(&format!("movl {right_asm}, {DST_SCRATCH_REGISTER}\n"));
            asm_code.push_str(&format!("cmpl {left_asm}, {DST_SCRATCH_REGISTER}"));
            Ok(asm_code)
        } else {
            Ok(format!("cmpl {}, {}", left_asm, right_asm))
        }
    }
}
#[derive(Clone, Debug)]
pub struct AsmJumpConditionalInstruction {
    // JmpCC in the textbook
    identifier: Identifier,
    condition: ConditionalCompareTypes
}
impl AsmJumpConditionalInstruction {
    pub fn new(
        identifier: Identifier,
        condition: ConditionalCompareTypes
    ) -> Self {
        AsmJumpConditionalInstruction {
            identifier,
            condition
        }
    }
}
#[derive(Clone, Debug)]
pub struct AsmSetConditionalInstruction {
    // SetCC in the textbook
    pub(crate) destination: AsmOperand,
    pub(crate) condition: ConditionalCompareTypes
}
impl AsmSetConditionalInstruction {
    pub fn new(
        destination: AsmOperand,
        condition: ConditionalCompareTypes
    ) -> Self {
        AsmSetConditionalInstruction {
            destination,
            condition
        }
    }
}