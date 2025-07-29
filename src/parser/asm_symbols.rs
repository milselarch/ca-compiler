use crate::parser::parser_helpers::PoppedTokenContext;

const TAB: &str = "    ";

pub trait AsmSymbol {
    fn to_asm_code(self) -> String;
}

pub struct AsmProgram {
    pub(crate) function: AsmFunction,
}
impl AsmSymbol for AsmProgram {
    fn to_asm_code(self) -> String {
        let mut code = self.function.to_asm_code();
        code.push_str(".section .note.GNU-stack,\"\",@progbits\n");
        code
    }
}

pub struct AsmFunction {
    pub(crate) name: String,
    pub(crate) instructions: Vec<AsmInstruction>,
}
impl AsmSymbol for AsmFunction {
    fn to_asm_code(self) -> String {
        let mut code = "".to_string();
        code.push_str(&format!("{TAB}.globl {}\n", self.name));
        code.push_str(&format!("{}:\n", self.name));
        for instruction in self.instructions {
            let inner_code = &instruction.to_asm_code();
            let indented_inner_code = indent::indent_all_with(TAB, inner_code);
            // println!("Indented inner code: {}", indented_inner_code);
            code.push_str(&*indented_inner_code);
        }
        code
    }
}

pub enum AsmInstruction {
    Mov(MovInstruction),
    Ret,
}
impl AsmSymbol for AsmInstruction {
    fn to_asm_code(self) -> String {
        match self {
            AsmInstruction::Mov(mov_instruction) => mov_instruction.to_asm_code(),
            AsmInstruction::Ret => "ret\n".to_string(),
        }
    }
}

pub struct MovInstruction {
    pub(crate) destination: AsmOperand,
    pub(crate) source: AsmOperand,
}
impl AsmSymbol for MovInstruction {
    fn to_asm_code(self) -> String {
        format!(
            "mov {}, {}\n",
            self.source.to_asm_code(),
            self.destination.to_asm_code()
        )
    }
}

pub enum AsmOperand {
    ImmediateValue(AsmImmediateValue),
    Register,
}
impl AsmSymbol for AsmOperand {
    fn to_asm_code(self) -> String {
        match self {
            AsmOperand::ImmediateValue(value) => value.to_asm_code(),
            AsmOperand::Register => "%eax".to_string(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct AsmImmediateValue {
    pub(crate) value: i64,
    pub(crate) pop_contexts: Vec<PoppedTokenContext>
}
impl AsmImmediateValue {
    pub fn new(value: i64) -> AsmImmediateValue {
        AsmImmediateValue {
            value,
            pop_contexts: vec![]
        }
    }
    
    fn _add_pop_context(&mut self, pop_context: PoppedTokenContext) {
        self.pop_contexts.push(pop_context);
    }
    
    fn _add_pop_context_opt(
        &mut self, pop_context: Option<PoppedTokenContext>
    ) {
        if let Some(pop_context) = pop_context {
            self._add_pop_context(pop_context);
        }
    }

    pub fn with_added_pop_context(mut self, pop_context: Option<PoppedTokenContext>) -> Self {
        let mut new = self.clone();
        new._add_pop_context_opt(pop_context);
        new
    }
}
impl AsmSymbol for AsmImmediateValue {
    fn to_asm_code(self) -> String {
        format!("${}", self.value)
    }
}

