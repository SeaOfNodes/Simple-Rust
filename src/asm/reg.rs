use std::ops::{Index, IndexMut};

// TODO how to represent other register sizes? this is currently also used for 32 bit.
/// page 104
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum Reg {
    RAX,
    RCX,
    RDX,
    RBX,
    RSP,
    RBP,
    RSI,
    RDI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl Reg {
    pub fn reg_field(self) -> u8 {
        self as u8 & 0b111
    }
    // TODO make two functions that return the the rex bits
    pub fn needs_rex_bit(self) -> bool {
        self as u8 > 0b111
    }

    pub fn preserved(self) -> bool {
        match self {
            Reg::RAX | Reg::RCX | Reg::RDX => false,
            Reg::RBX | Reg::RSP | Reg::RBP => true,
            Reg::RSI | Reg::RDI | Reg::R8 | Reg::R9 | Reg::R10 | Reg::R11 => false,
            Reg::R12 | Reg::R13 | Reg::R14 | Reg::R15 => true,
        }
    }

    pub const COUNT: usize = 16;

    pub const CALLER_SAVED: [Self; 7] = [
        // 1st return register
        // Self::RAX,
        Self::RCX,
        // 2nd return register
        // Self::RDX,
        Self::RSI,
        Self::RDI,
        Self::R8,
        Self::R9,
        Self::R10,
        Self::R11,
    ];

    // preserved except rsp and rbp
    pub const CALLEE_SAVED: [Self; 5] = [
        Self::RBX,
        // Self::RSP,
        // Self::RBP,
        Self::R12,
        Self::R13,
        Self::R14,
        Self::R15,
    ];

    pub const INTEGER_ARGUMENT_REGISTERS: [Self; 6] = [
        Self::RDI,
        Self::RSI,
        Self::RDX,
        Self::RCX,
        Self::R8,
        Self::R9,
    ];
}

impl From<usize> for Reg {
    fn from(value: usize) -> Self {
        match value {
            0 => Self::RAX,
            1 => Self::RCX,
            2 => Self::RDX,
            3 => Self::RBX,
            4 => Self::RSP,
            5 => Self::RBP,
            6 => Self::RSI,
            7 => Self::RDI,
            8 => Self::R8,
            9 => Self::R9,
            10 => Self::R10,
            11 => Self::R11,
            12 => Self::R12,
            13 => Self::R13,
            14 => Self::R14,
            15 => Self::R15,
            _ => unreachable!(),
        }
    }
}

pub struct RegSet {
    is_free: [bool; Reg::COUNT],
}

impl RegSet {
    pub fn all(free: bool) -> RegSet {
        RegSet {
            is_free: [free; 16]
        }
    }

    pub fn reserve_any(&mut self) -> Reg {
        for i in 0..self.is_free.len() {
            let reg = Reg::from(i);
            if self[reg] {
                self.reserve(reg);
                return reg;
            }
        }
        todo!("How do we avoid this case?")
    }

    pub fn reserve(&mut self, reg: Reg) {
        assert!(self[reg]);
        self[reg] = false;
    }

    pub fn release(&mut self, reg: Reg) {
        assert!(!self[reg]);
        self[reg] = true;
    }
}

impl Index<Reg> for RegSet {
    type Output = bool;

    fn index(&self, index: Reg) -> &Self::Output {
        &self.is_free[index as usize]
    }
}

impl IndexMut<Reg> for RegSet {
    fn index_mut(&mut self, index: Reg) -> &mut Self::Output {
        &mut self.is_free[index as usize]
    }
}
