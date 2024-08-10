use crate::codegen::asm::reg::Reg;

#[derive(Eq, PartialEq, Clone, Copy)]
pub enum RM {
    R(Reg),
    M(Mem),
}

impl From<Reg> for RM {
    fn from(reg: Reg) -> Self {
        Self::R(reg)
    }
}

impl From<Mem> for RM {
    fn from(mem: Mem) -> Self {
        Self::M(mem)
    }
}

/// https://wiki.osdev.org/X86-64_Instruction_Encoding
/// mod=01|10 + [base + (index * scale) + disp8/32]
/// with SP (index*scale) is ignored!
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Mem {
    pub base: Reg,
    pub index: Reg,
    pub scale: Scale,
    pub disp: i32,
}

impl Mem {
    pub const NO_INDEX: Reg = Reg::RSP;

    pub fn rbp(disp: i32) -> Self {
        Mem {
            base: Reg::RBP,
            index: Mem::NO_INDEX,
            scale: Scale::S1,
            disp,
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum Scale {
    S1 = 0b00,
    S2 = 0b01,
    S4 = 0b10,
    S8 = 0b11,
}
