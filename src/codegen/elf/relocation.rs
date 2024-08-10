use crate::codegen::elf::data_types::*;

#[repr(C)]
pub struct Elf64_Rel {
    pub r_offset: Elf64_Addr,
    pub r_info: Elf64_Xword,
}

#[repr(C)]
pub struct Elf64_Rela {
    pub r_offset: Elf64_Addr,
    pub r_info: Elf64_Xword,
    pub r_addend: Elf64_Sxword,
}
