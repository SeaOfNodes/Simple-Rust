use std::convert::TryFrom;

use crate::asm::mem::{Mem, Scale, RM};
use crate::asm::reg::{Reg, RegSet};

pub mod mem;
pub mod reg;

pub struct MachineCode {
    pub bytes: Vec<u8>,
    pub relocations: Vec<RelocationKind>,
    pub function_symbols: Vec<FunctionSymbol>,
    pub ro_data: Vec<u8>,
}

pub enum RelocationKind {
    Fn32(usize, String),
    Rodata32(usize, i64),
}

pub struct FunctionSymbol {
    pub name: String,
    pub code_offset: u64,
    pub code_size: u64,
    pub is_extern: bool,
}

impl MachineCode {
    pub fn new() -> Self {
        Self {
            bytes: vec![],
            relocations: vec![],
            function_symbols: vec![],
            ro_data: vec![],
        }
    }

    pub fn push(&mut self, byte: u8) {
        self.bytes.push(byte);
    }

    pub fn push32(&mut self, value: u32) {
        self.bytes.extend_from_slice(&value.to_ne_bytes()); // TODO endianess?
    }

    pub fn build(self) -> Vec<u8> {
        self.bytes
    }
}

#[must_use]
pub struct ToPatch {
    rel32_index: usize,
}

impl MachineCode {
    /// REX: 0b0100_WRXB
    /// W = 64 bit mode
    /// R = Extension of the ModR/M reg field
    /// X = Extension of the SIB index field
    /// B = Extension of the ModR/M r/m field, SIB base field, or Opcode reg field
    pub fn rex(&mut self, w: bool, r: bool, x: bool, b: bool) {
        let mut byte = 0b0100_0000;
        if w {
            byte |= 0b1000;
        }
        if r {
            byte |= 0b100;
        }
        if x {
            byte |= 0b10;
        }
        if b {
            byte |= 0b1;
        }
        self.push(byte)
    }

    /// if mod != 11
    /// REX.X extends index
    /// REX.B extends base
    pub fn sib(&mut self, scale: Scale, index_3: u8, base_3: u8) {
        self.push(((scale as u8) << 6) | ((index_3 & 0b111) << 3) | (base_3 & 0b111) << 0);
    }

    pub fn mod_reg_rm(&mut self, mod_2: u8, reg_3: u8, rm_3: u8) {
        self.push(((mod_2 & 0b11) << 6) | ((reg_3 & 0b111) << 3) | (rm_3 & 0b111));
    }

    pub fn mov_reg32_imm32(&mut self, reg: Reg, imm: u32) {
        self.push(0xb8 + reg.reg_field());
        self.push(imm as u8);
        self.push((imm >> 8) as u8);
        self.push((imm >> 16) as u8);
        self.push((imm >> 24) as u8);
    }

    // TODO eliminate this function
    pub fn mov_reg_reg(&mut self, reg: Reg, reg2: Reg) {
        self.mov_rm64_r64(RM::R(reg), reg2);
    }

    pub fn mov_rm64_r64<T: Into<RM>>(&mut self, target: T, source: Reg) {
        self.op_rm64_r64(false, 0x89, target, source);
    }

    pub fn mov_r64_rm64<T: Into<RM>>(&mut self, target: Reg, source: T) {
        self.op_rm64_r64(false, 0x8B, source, target);
    }

    pub fn add_rm64_r64<T: Into<RM>>(&mut self, target: T, r: Reg) {
        self.op_rm64_r64(false, 0x01, target, r);
    }

    pub fn sub_rm64_r64<T: Into<RM>>(&mut self, target: T, r: Reg) {
        self.op_rm64_r64(false, 0x29, target, r);
    }

    pub fn imul_r64_rm64<T: Into<RM>>(&mut self, target: Reg, rm: T) {
        self.op_rm64_r64(true, 0xaf, rm, target);
    }

    // TODO test these
    pub fn lea(&mut self, reg: Reg, mem: Mem) {
        self.op_rm64_r64(false, 0x8d, mem, reg)
    }

    pub fn cmp_rm64_r64<T: Into<RM>>(&mut self, a: T, b: Reg) {
        self.op_rm64_r64(false, 0x39, a, b);
    }

    pub fn cmp_r64_rm64<T: Into<RM>>(&mut self, a: Reg, b: T) {
        self.op_rm64_r64(false, 0x3b, b, a);
    }

    // if ZF = 1 { 1 } else { 0 }
    pub fn sete(&mut self, reg: Reg) {
        if reg.needs_rex_bit() {
            self.rex(false, false, false, reg.needs_rex_bit());
        }
        self.push(0x0f);
        self.push(0x94);
        self.mod_reg_rm(0b11, 0, reg.reg_field());
    }

    // TODO reverse order of rm and reg parameters?
    pub fn op_rm64_r64<T: Into<RM>>(&mut self, op_prefix: bool, op: u8, rm: T, reg: Reg) {
        let rm = rm.into();
        match rm {
            RM::R(rm) => {
                self.rex(true, reg.needs_rex_bit(), false, rm.needs_rex_bit());
                if op_prefix {
                    self.push(0x0f);
                }
                self.push(op);
                self.mod_reg_rm(0b11, reg.reg_field(), rm.reg_field());
            }
            RM::M(mem) => {
                self.rex(
                    true,
                    reg.needs_rex_bit(),
                    mem.index.needs_rex_bit(),
                    mem.base.needs_rex_bit(),
                );
                if op_prefix {
                    self.push(0x0f);
                }
                self.push(op);
                if let Ok(disp) = i8::try_from(mem.disp) {
                    self.mod_reg_rm(0b01, reg.reg_field(), 100);
                    self.sib(mem.scale, mem.index.reg_field(), mem.base.reg_field());
                    self.push(disp as u8);
                } else {
                    self.mod_reg_rm(0b10, reg.reg_field(), 100);
                    self.sib(mem.scale, mem.index.reg_field(), mem.base.reg_field());
                    self.push32(mem.disp as u32)
                }
            }
        }
    }

    pub fn add_reg32_reg32(&mut self, reg: Reg, other: Reg) {
        self.push(0x01);
        self.push(0b1100_0000 | other.reg_field() << 3 | reg.reg_field());
    }

    pub fn sub_reg64_imm32(&mut self, reg: Reg, value: i32) {
        self.rex(true, false, false, reg.needs_rex_bit());
        self.push(0x81);
        self.push(0b1100_0000 | (5 << 3) | reg.reg_field());
        self.push32(value as u32);
    }

    pub fn add_reg64_imm32(&mut self, reg: Reg, value: i32) {
        self.rex(true, false, false, reg.needs_rex_bit());
        self.push(0x81);
        self.push(0b1100_0000 | (0 << 3) | reg.reg_field());
        self.push32(value as u32);
    }

    pub fn push_r64(&mut self, reg: Reg) {
        if reg.needs_rex_bit() {
            self.push(0b0100_0001);
        }
        self.push(0x50 + reg.reg_field());
    }

    pub fn pop_r64(&mut self, reg: Reg) {
        if reg.needs_rex_bit() {
            self.push(0b0100_0001);
        }
        self.push(0x58 + reg.reg_field());
    }

    pub fn lea_rip_offset32(&mut self, into: Reg, value: i32) {
        if into.needs_rex_bit() {
            self.push(0b0100_1100);
        } else {
            self.push(0b0100_1000);
        }
        self.push(0x8d);
        // 2.2.1.6 RIP-Relative Addressing: mod=00, r/m=101 (none)
        self.push((into.reg_field() << 3) | 0b101);
        self.push32(value as u32);
    }

    pub fn ret_near(&mut self) {
        self.push(0xc3);
    }

    pub fn call_rel32(&mut self, name: &str) {
        // TODO calls need to be aligned to 16 bit!
        self.push(0xe8);
        let offset = self.bytes.len();
        self.relocations
            .push(RelocationKind::Fn32(offset, name.to_string()));
        self.push(0);
        self.push(0);
        self.push(0);
        self.push(0);
    }

    pub fn jmp_rel32(&mut self) -> ToPatch {
        self.push(0xe9);
        let rel32_index = self.bytes.len();
        self.push32(0);
        ToPatch { rel32_index }
    }

    pub fn jz_rel32(&mut self) -> ToPatch {
        self.push(0x0f);
        self.push(0x84);
        let rel32_index = self.bytes.len();
        self.push32(0);
        ToPatch { rel32_index }
    }

    pub fn patch_to(&mut self, to_patch: ToPatch, target: usize) {
        // "Here, the EIP register contains the address of the instruction following the JMP instruction"
        let eip = to_patch.rel32_index + 4;
        let relative = target as i64 - eip as i64;
        let offset = i32::try_from(relative).unwrap();
        let bytes: [u8; 4] = offset.to_ne_bytes();
        self.bytes[to_patch.rel32_index] = bytes[0];
        self.bytes[to_patch.rel32_index + 1] = bytes[1];
        self.bytes[to_patch.rel32_index + 2] = bytes[2];
        self.bytes[to_patch.rel32_index + 3] = bytes[3];
    }

    pub fn test_r64_and_r64(&mut self, reg: Reg, reg2: Reg) {
        self.push(
            0b0100_1000
                | if reg2.needs_rex_bit() { 0b100 } else { 0 }
                | if reg.needs_rex_bit() { 0b1 } else { 0 },
        );
        self.push(0x85);
        self.push(0b1100_0000 | (reg2.reg_field() << 3) | reg.reg_field());
    }

    pub fn load_string_literal(&mut self, into: Reg, value: &str) {
        self.lea_rip_offset32(into, 0);
        let offset = self.bytes.len() - 4;
        let addend = self.ro_data.len() as i64;
        self.relocations
            .push(RelocationKind::Rodata32(offset, addend));
        self.ro_data.extend_from_slice(value.as_bytes());
        self.ro_data.push(0); // zero terminator
    }

    // pub fn add_function(&mut self, checked: &CheckedFunction, types: &Types) {
    //     let symbol = if let Some(body) = &checked.function.body {
    //         let code_offset = self.bytes.len() as u64;
    //
    //         let mut disp: i32 = -8; // first is RBP
    //         let to_save = Reg::CALLEE_SAVED;
    //         disp -= 8 * to_save.len() as i32;
    //
    //         let mut local_variables = LocalTable { entries: Vec::with_capacity(checked.locals.entries.len()) };
    //         // TODO correctly assign offsets for parameters that are passed on the stack!
    //         for local in checked.locals.entries.iter() {
    //             let size = types.byte_size(local.ty) as i32;
    //             let align = types.byte_align(local.ty) as i32;
    //
    //             disp -= size;
    //
    //             while disp % align != 0 {
    //                 disp -= 1;
    //             }
    //             local_variables.entries.push(Local {
    //                 name: local.name.clone(),
    //                 ty: local.ty,
    //             });
    //         }
    //
    //         // The stack is 16 byte aligned just before the call instruction is called.
    //         // -8 are for the return address.
    //         while (disp - 8) % 16 != 0 {
    //             disp -= 1;
    //         }
    //         let stack_size = -disp;
    //
    //         self.push_r64(Reg::RBP);
    //         self.mov_r64_rm64(Reg::RBP, Reg::RSP);
    //         self.sub_reg64_imm32(Reg::RSP, stack_size);
    //
    //         for reg in to_save.iter().cloned() {
    //             self.push_r64(reg);
    //         }
    //
    //         // TODO only do this for integer arguments
    //         for i in 0..checked.function.parameters.len().min(6) {
    //             let reg = Reg::INTEGER_ARGUMENT_REGISTERS[i];
    //             // let disp = local_variables.entries[i + 1].disp; // TODO this is wrong
    //             let disp = todo!();
    //             self.mov_rm64_r64(Mem::rbp(disp), reg);
    //         }
    //
    //         let mut free = RegSet::all(true);
    //         free.reserve(Reg::RAX);
    //         free.reserve(Reg::RDX);
    //         free.reserve(Reg::RSP);
    //         free.reserve(Reg::RBP);
    //
    //         let mut returns: Vec<ToPatch> = vec![];
    //         // self.block(body, &mut free, &mut returns, &local_variables, types);
    //
    //         let here = self.bytes.len();
    //         for to_patch in returns {
    //             self.patch_to(to_patch, here);
    //         }
    //
    //         self.add_reg64_imm32(Reg::RSP, stack_size);
    //         for reg in Reg::CALLEE_SAVED.iter().rev().cloned() {
    //             self.pop_r64(reg);
    //         }
    //         self.pop_r64(Reg::RBP);
    //         self.ret_near();
    //
    //         FunctionSymbol {
    //             name: checked.function.name.to_string(),
    //             code_offset,
    //             code_size: self.bytes.len() as u64 - code_offset,
    //             is_extern: false,
    //         }
    //     } else {
    //         FunctionSymbol {
    //             name: checked.function.name.to_string(),
    //             code_offset: 0,
    //             code_size: 0,
    //             is_extern: true,
    //         }
    //     };
    //
    //     self.function_symbols.push(symbol)
    // }
    //
    pub fn move_into<T, F>(&mut self, rm: RM, free: &mut RegSet, mut f: F) -> T
    where
        F: FnMut(&mut Self, &mut RegSet, Reg) -> T,
    {
        match rm {
            RM::R(r) => f(self, free, r),
            RM::M(m) => {
                let r = free.reserve_any();
                let t = f(self, free, r);
                self.mov_rm64_r64(m, r);
                free.release(r);
                t
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use iced_x86::code_asm::*;
    use iced_x86::{Decoder, DecoderOptions, Formatter, Instruction, NasmFormatter};

    use crate::asm::mem::{Mem, Scale};

    use super::*;

    #[test]
    fn assembler() {
        compare_with_iced(|code, i| {
            i.mov(rcx, rbx + rax * 1 + 0x4299).unwrap();
            code.mov_r64_rm64(
                Reg::RCX,
                RM::M(Mem {
                    base: Reg::RBX,
                    index: Reg::RAX,
                    scale: Scale::S1,
                    disp: 0x4299,
                }),
            );
        });

        compare_with_iced(|code, i| {
            i.mov(rcx + r11 * 4 + 0x42, r11).unwrap();
            code.mov_rm64_r64(
                RM::M(Mem {
                    base: Reg::RCX,
                    index: Reg::R11,
                    scale: Scale::S4,
                    disp: 0x42,
                }),
                Reg::R11,
            );
        });

        compare_with_iced(|code, i| {
            i.sub(rbx, 0x11223344).unwrap();
            code.sub_reg64_imm32(Reg::RBX, 0x11223344);
        });

        compare_with_iced(|code, i| {
            i.sub(r11, 0x11223344).unwrap();
            code.sub_reg64_imm32(Reg::R11, 0x11223344);
        });

        compare_with_iced(|code, i| {
            i.add(rbx, 0x11223344).unwrap();
            code.add_reg64_imm32(Reg::RBX, 0x11223344);
        });

        compare_with_iced(|code, i| {
            i.add(r11, 0x11223344).unwrap();
            code.add_reg64_imm32(Reg::R11, 0x11223344);
        });

        compare_with_iced(|code, i| {
            i.mov(eax, 0x11223344).unwrap();
            code.mov_reg32_imm32(Reg::RAX, 0x11223344);
        });

        compare_with_iced(|code, i| {
            i.mov(esi, 0x83223344u32).unwrap();
            code.mov_reg32_imm32(Reg::RSI, 0x83223344);
        });

        compare_with_iced(|code, i| {
            i.mov(rax, rsi).unwrap();
            code.mov_reg_reg(Reg::RAX, Reg::RSI);
        });

        compare_with_iced(|code, i| {
            i.mov(rax, r8).unwrap();
            code.mov_reg_reg(Reg::RAX, Reg::R8);
        });

        compare_with_iced(|code, i| {
            i.mov(r8, rax).unwrap();
            code.mov_reg_reg(Reg::R8, Reg::RAX);
        });

        compare_with_iced(|code, i| {
            i.mov(rcx, rsi).unwrap();
            code.mov_reg_reg(Reg::RCX, Reg::RSI);
        });

        compare_with_iced(|code, i| {
            i.push(rbp).unwrap();
            code.push_r64(Reg::RBP);
        });

        compare_with_iced(|code, i| {
            i.test(rbx, rcx).unwrap();
            code.test_r64_and_r64(Reg::RBX, Reg::RCX);
        });

        compare_with_iced(|code, i| {
            i.test(r12, r12).unwrap();
            code.test_r64_and_r64(Reg::R12, Reg::R12);
        });

        compare_with_iced(|code, i| {
            i.test(rbx, r10).unwrap();
            code.test_r64_and_r64(Reg::RBX, Reg::R10);
        });

        compare_with_iced(|code, i| {
            i.add(rax, r11).unwrap();
            code.add_rm64_r64(Reg::RAX, Reg::R11);
        });

        compare_with_iced(|code, i| {
            i.sub(rax, r11).unwrap();
            code.sub_rm64_r64(Reg::RAX, Reg::R11);
        });

        compare_with_iced(|code, i| {
            i.imul_2(rax, r11).unwrap();
            code.imul_r64_rm64(Reg::RAX, Reg::R11);
        });
    }

    fn compare_with_iced(x: fn(&mut MachineCode, &mut CodeAssembler)) {
        let mut code = MachineCode::new();
        let mut i = CodeAssembler::new(64).unwrap();

        x(&mut code, &mut i);

        let rip = 0;

        let code_bytes = code.build();
        println!("Code bytes: ");
        disassemble(&code_bytes, rip);

        let iced_bytes = i.assemble(rip).unwrap();
        println!("Iced bytes: ");
        disassemble(&iced_bytes, rip);

        assert_eq!(code_bytes, iced_bytes);
    }

    fn disassemble(bytes: &[u8], rip: u64) {
        let hex_column_byte_length = 10;
        let mut decoder = Decoder::with_ip(64, &bytes, rip, DecoderOptions::NONE);

        let mut formatter = NasmFormatter::new();
        formatter.options_mut().set_digit_separator("`");
        formatter.options_mut().set_first_operand_char_index(10);

        let mut output = String::new();
        let mut instruction = Instruction::default();

        while decoder.can_decode() {
            decoder.decode_out(&mut instruction);

            output.clear();
            formatter.format(&instruction, &mut output);

            // Eg. "00007FFAC46ACDB2 488DAC2400FFFFFF     lea       rbp, [rsp - 100h]"
            print!("{:016X} ", instruction.ip());
            let start_index = (instruction.ip() - rip) as usize;
            let instr_bytes = &bytes[start_index..start_index + instruction.len()];
            for b in instr_bytes.iter() {
                print!("{:02X}", b);
            }
            if instr_bytes.len() < hex_column_byte_length {
                for _ in 0..hex_column_byte_length - instr_bytes.len() {
                    print!("  ");
                }
            }
            println!(" {}", output);
        }
    }
}
