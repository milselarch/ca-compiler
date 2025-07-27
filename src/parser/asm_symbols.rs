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
        code.push_str(&format!(".globl {}\n", self.name));
        code.push_str(&format!("{}:\n", self.name));
        for instruction in self.instructions {
            code.push_str(&instruction.to_asm_code());
            code.push('\n');
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

pub struct AsmImmediateValue {
    value: i64,
}
impl AsmImmediateValue {
    pub fn new(value: i64) -> AsmImmediateValue {
        AsmImmediateValue { value }
    }
}
impl AsmSymbol for AsmImmediateValue {
    fn to_asm_code(self) -> String {
        format!("${}", self.value)
    }
}

