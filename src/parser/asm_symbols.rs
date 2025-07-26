pub struct AsmProgram {
    pub(crate) function: AsmFunction,
}

pub struct AsmFunction {
    pub(crate) name: String,
    pub(crate) instructions: Vec<AsmInstruction>,
}

pub enum AsmInstruction {
    Mov(MovInstruction),
    Ret,
}

pub struct MovInstruction {
    pub(crate) destination: AsmOperand,
    pub(crate) source: AsmOperand,
}
pub enum AsmOperand {
    ImmediateValue(AsmImmediateValue),
    Register,
}
pub struct AsmImmediateValue {
    value: i64,
}
impl AsmImmediateValue {
    pub fn new(value: i64) -> AsmImmediateValue {
        AsmImmediateValue { value }
    }
}
