use crate::asm_gen::asm_symbols::{AnnotationInstruction, FromPseudoRegisterResult, PseudoRegister, StackAddress};
use crate::asm_gen::helpers::DiffableHashMap;
use crate::parser::parser_helpers::PoppedTokenContext;
use crate::potato_cpu::bit_allocation::GrowableBitAllocation;
use crate::potato_cpu::potato_cpu::{PotatoCPU, PotatoCodes, PotatoSpec, Registers};
use crate::tacky::tacky_symbols::{TackyFunction, TackyInstruction, TackyProgram, TackyValue, TackyVariable};

pub struct PotatoProgram {
    // Define the structure of a Potato assembly program
    // TODO: asm instructions should be before raw cpu codes
    function: PotatoFunction,
}
impl PotatoProgram {
    pub fn new(function: PotatoFunction) -> Self {
        PotatoProgram { function }
    }

    pub fn from_tacky_program(
        tacky_program: TackyProgram
    ) -> Self {
        Self::new(PotatoFunction::from_tacky_function(tacky_program.function))
    }
    pub fn execute(&self) -> i64 {
        let spec = PotatoSpec::new(
            self.function.instructions.clone(),
            4, 32,
        );
        let mut cpu = PotatoCPU::new(&spec);
        let run_result = cpu.run(10000);
        if !run_result.halted {
            panic!("Program did not halt within the time limit");
        }

        let return_register = cpu.read_register(Registers::FunctionReturn);
        let return_value = return_register.to_i64().unwrap();
        return_value
    }
}


#[derive(Clone, Debug)]
pub struct PotatoAnnotationInstruction {
    pub label: String,
    pub pop_context: Option<PoppedTokenContext>
}
impl PotatoAnnotationInstruction {
    pub fn new(
        label: String,
        pop_context: Option<PoppedTokenContext>
    ) -> Self {
        PotatoAnnotationInstruction {
            label,
            pop_context,
        }
    }
    pub fn to_potato_asm_instruction(&self) -> PotatoAsmInstruction {
        PotatoAsmInstruction::Annotation(self.clone())
    }
}

#[derive(Clone, Debug)]
pub struct PotatoStackAddress {
    // TODO: translate from StackAddress
    // TODO: is there a way / need to deduplicate this with StackAddress?
    pub(crate) offset: u64,
    pub(crate) offset_size: u64,
    pub(crate) pop_contexts: Vec<PoppedTokenContext>,
    pub(crate) tacky_var: Option<TackyVariable>,
}
impl PotatoStackAddress {
    pub fn new(
        offset: u64,
        offset_size: u64,
        pop_contexts: Vec<PoppedTokenContext>,
        tacky_var: Option<TackyVariable>,
    ) -> Self {
        PotatoStackAddress {
            offset,
            offset_size,
            pop_contexts,
            tacky_var,
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

pub struct PotatoAsmImmediateValue {
    pub(crate) value: GrowableBitAllocation,
    pub(crate) pop_contexts: Vec<PoppedTokenContext>
}
impl PotatoAsmImmediateValue {
    pub fn new(
        value: GrowableBitAllocation,
    ) -> Self {
        PotatoAsmImmediateValue {
            value,
            pop_contexts: vec![],
        }
    }
}

#[derive(Clone, Debug)]
pub enum PotatoAsmOperand {
    ImmediateValue,
    Register(Registers),
    Pseudo(PseudoRegister),
    Stack(PotatoStackAddress)
}
impl PotatoAsmOperand {
    pub fn is_stack_address(&self) -> bool {
        match self {
            PotatoAsmOperand::Stack(_) => true,
            _ => false
        }
    }
    pub fn is_constant(&self) -> bool {
        match self {
            PotatoAsmOperand::ImmediateValue => true,
            _ => false
        }
    }
}

#[derive(Clone, Debug)]
pub struct PotatoMovInstruction {
    pub(crate) source: PotatoAsmOperand,
    pub(crate) destination: PotatoAsmOperand,
}

#[derive(Clone, Debug)]
pub enum PotatoAsmInstruction {
    // tentatively use AsmInstruction for reference
    Mov(),
    Annotation(PotatoAnnotationInstruction),
    Ret
}

pub struct PotatoFunction {
    name: String,
    instructions: Vec<PotatoCodes>,
    pub(crate) pop_contexts: Vec<PoppedTokenContext>,
}
impl PotatoFunction {
    pub fn new(name: String) -> Self {
        PotatoFunction {
            name,
            instructions: vec![],
            pop_contexts: vec![],
        }
    }
    pub fn from_tacky_function(tacky_function: TackyFunction) -> Self {
        // TODO: this is about as barebones as it gets rn
        let mut asm_function = Self::new(tacky_function.name_to_string());

        for tacky_instruction in tacky_function.instructions {
            let return_val = match tacky_instruction {
                TackyInstruction::Return(tacky_value) => {
                    match tacky_value {
                        TackyValue::Constant(ast_constant) => {
                            // TODO have to deal with negative numbers at some point
                            let value = ast_constant.to_usize().unwrap();
                            value
                        }
                        _ => {
                            panic!(
                                "Unsupported return value type in \
                                PotatoFunction::from_tacky_function"
                            );
                        }
                    }
                },
                TackyInstruction::AnnotationStartInstruction(annotation_start) => {
                    // TODO: handle these properly
                    continue
                },
                TackyInstruction::AnnotationEndInstruction(annotation_end) => {
                    // TODO: handle these properly
                    continue
                },
                unsupported => {
                    let err_msg = format!(
                        "Unsupported instruction type {} in \
                        PotatoFunction::from_tacky_function",
                        unsupported
                    );
                    panic!("{}", err_msg);
                }
            };

            let data_value = GrowableBitAllocation::from_num(return_val);
            let instructions = vec![
                PotatoCodes::DataValue(data_value),
                PotatoCodes::MovDataValueToRegister(
                    0, Registers::FunctionReturn
                )
            ];
            asm_function.instructions.extend(instructions);
        }
        asm_function
    }
}

#[cfg(test)]
mod tests {
    use crate::potato_cpu::potato_asm::{PotatoFunction, PotatoProgram};
    use crate::tacky;
    use super::*;

    #[test]
    fn test_result_2_compilation() {
        let file_path = "./writing-a-c-compiler-tests/tests/chapter_1/valid/return_2.c";
        let tacky_gen_result =
            tacky::tacky_symbols::tacky_gen_from_filepath(file_path, false);
        let tacky_program = tacky_gen_result.unwrap();
        let potato_program = PotatoProgram::from_tacky_program(tacky_program);
        let return_value = potato_program.execute();
        assert_eq!(return_value, 2);
    }

    #[test]
    fn test_multi_digit_return() {
        let file_path = "./writing-a-c-compiler-tests/tests/chapter_1/valid/multi_digit.c";
        let tacky_gen_result =
            tacky::tacky_symbols::tacky_gen_from_filepath(file_path, false);
        let tacky_program = tacky_gen_result.unwrap();
        let potato_program = PotatoProgram::from_tacky_program(tacky_program);
        let return_value = potato_program.execute();
        assert_eq!(return_value, 100);
    }
}