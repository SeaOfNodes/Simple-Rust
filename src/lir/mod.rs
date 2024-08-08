use std::fmt::{Debug, Display, Formatter};
use std::mem;

use crate::asm::MachineCode;
use crate::asm::reg::Reg;
use crate::lir::stack_slots::StackSlots;
use crate::soup::location::Location;

mod stack_slots;

/// A function that is very similar to machine code.
pub struct Lir<'t> {
    stack_slots: StackSlots<'t>,
    blocks: Vec<BasicBlock<'t>>,
    next_reg: Register,
}

impl<'t> Lir<'t> {
    pub const RETURN_REGISTER: Register = Register { id: 0 };

    pub fn new() -> Self {
        Self {
            stack_slots: StackSlots::new(),
            blocks: vec![],
            next_reg: Register { id: 1 },
        }
    }

    fn new_reg(&mut self) -> Register {
        let new = Register {
            id: self.next_reg.id.checked_add(1).unwrap(),
        };
        mem::replace(&mut self.next_reg, new)
    }

    pub fn assemble(&self, code: &mut MachineCode) {
        // TODO do this in advance... also needs to be parameterized on time
        let allocation = vec![
            Reg::RAX,
            Reg::RCX,
            Reg::RDX,
            Reg::RBX,
            Reg::RSP,
            Reg::RBP,
            Reg::RSI,
            Reg::RDI,
            Reg::R8,
            Reg::R9,
            Reg::R10,
            Reg::R11,
            Reg::R12,
            Reg::R13,
            Reg::R14,
            Reg::R15,
        ];

        for bb in self.blocks.iter() {
            for x in bb.instructions.iter() {
                match &x.operation {
                    Operation::Ret => {
                        code.ret_near();
                    }
                    Operation::Imm64 { value, dst } => {
                        // TODO load 64 bit immediate instead
                        let dst = allocation[dst.id as usize];
                        code.mov_reg32_imm32(dst, *value as u32);
                    }
                    Operation::Mov { dst, src } => {
                        let dst = allocation[dst.id as usize];
                        let src = allocation[src.id as usize];
                        code.mov_r64_rm64(dst, src);
                    }
                    Operation::Unary { kind, src, dst } => {
                        let dst = allocation[dst.id as usize];
                        let src = allocation[src.id as usize];
                        match kind {
                            Unary::Negate => {
                                code.mov_reg32_imm32(dst, 0);
                                code.sub_rm64_r64(dst, src);
                            }
                        }
                    }
                    Operation::Binary {
                        kind,
                        first,
                        second,
                        dst,
                    } => {
                        let dst = allocation[dst.id as usize];
                        let first = allocation[first.id as usize];
                        let second = allocation[second.id as usize];
                        code.mov_r64_rm64(dst, first);
                        match kind {
                            Binary::Add => code.add_rm64_r64(first, second),
                            Binary::Sub => code.sub_rm64_r64(first, second),
                            Binary::Mul => code.imul_r64_rm64(first, second),
                        }
                    }
                }
            }
        }
    }

    pub fn new_block_id(&mut self) -> BlockId {
        let id = BlockId {
            id: u32::try_from(self.blocks.len()).unwrap(),
        };
        self.blocks.push(BasicBlock {
            id,
            instructions: vec![],
        });
        id
    }

    fn add(&mut self, block: BlockId, operation: Operation, location: Option<Location<'t>>) {
        self.blocks[block.id as usize]
            .instructions
            .push(Instruction {
                operation,
                location,
            })
    }

    pub fn ret(&mut self, block: BlockId, location: Option<Location<'t>>) {
        self.add(block, Operation::Ret, location)
    }

    pub fn imm64(
        &mut self,
        block: BlockId,
        value: i64,
        location: Option<Location<'t>>,
    ) -> Register {
        let dst = self.new_reg();
        self.add(block, Operation::Imm64 { value, dst }, location);
        dst
    }

    pub fn mov(
        &mut self,
        block: BlockId,
        dst: Register,
        src: Register,
        location: Option<Location<'t>>,
    ) {
        self.add(block, Operation::Mov { dst, src }, location)
    }

    pub fn binary(
        &mut self,
        block: BlockId,
        kind: Binary,
        first: Register,
        second: Register,
        location: Option<Location<'t>>,
    ) -> Register {
        let dst = self.new_reg();
        self.add(
            block,
            Operation::Binary {
                kind,
                first,
                second,
                dst,
            },
            location,
        );
        dst
    }
}

#[derive(Debug)]
pub struct BasicBlock<'t> {
    pub id: BlockId,
    instructions: Vec<Instruction<'t>>,
}

#[derive(Debug)]
struct Instruction<'t> {
    operation: Operation,
    location: Option<Location<'t>>,
}

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
pub struct BlockId {
    id: u32,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Register {
    id: u32,
}

impl Debug for Register {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "R{}", self.id)
    }
}

#[derive(Debug)]
enum Operation {
    Ret,
    Imm64 {
        dst: Register,
        value: i64,
    },
    Mov {
        dst: Register,
        src: Register,
    },
    Unary {
        kind: Unary,
        src: Register,
        dst: Register,
    },
    Binary {
        kind: Binary,
        first: Register,
        second: Register,
        dst: Register,
    },
}

#[derive(Debug)]
pub enum Unary {
    Negate,
}

#[derive(Debug)]
pub enum Binary {
    Add,
    Sub,
    Mul,
}

impl<'t> Display for Lir<'t> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Lir: {:?}", self.stack_slots)?;
        for bb in self.blocks.iter() {
            writeln!(f, "Block {}", bb.id.id)?;
            for i in bb.instructions.iter() {
                write!(f, "    {:?}", &i.operation)?;
                if let Some(l) = &i.location {
                    write!(f, " @ {l:?}")?;
                }
                writeln!(f)?;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lir() {
        let mut lir = Lir::new();
        let block = lir.new_block_id();

        let r1 = lir.imm64(block, 42, None);
        let r2 = lir.imm64(block, 43, None);
        let r3 = lir.binary(block, Binary::Add, r1, r2, None);
        lir.mov(block, Lir::RETURN_REGISTER, r3, None);
        lir.ret(block, None);

        assert_eq!(lir.blocks.len(), 1);
        assert_eq!(lir.blocks[0].instructions.len(), 5);
    }
}
