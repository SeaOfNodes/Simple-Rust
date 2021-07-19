use crate::elf::data_types::*;

//
// Special section indexes
//

pub const SHN_UNDEF: Elf64_Half = 0;
pub const SHN_LORESERVE: Elf64_Half = 0xff00;
pub const SHN_LOPROC: Elf64_Half = 0xff00;
pub const SHN_HIPROC: Elf64_Half = 0xff1f;
pub const SHN_LOOS: Elf64_Half = 0xff20;
pub const SHN_HIOS: Elf64_Half = 0xff3f;
pub const SHN_ABS: Elf64_Half = 0xfff1;
pub const SHN_COMMON: Elf64_Half = 0xfff2;
pub const SHN_XINDEX: Elf64_Half = 0xffff;
pub const SHN_HIRESERVE: Elf64_Half = 0xffff;

//
// Section header
//

#[repr(C)]
pub struct Elf64_Shdr {
    pub sh_name: Elf64_Word,
    pub sh_type: Elf64_Word,
    pub sh_flags: Elf64_Xword,
    pub sh_addr: Elf64_Addr,
    pub sh_offset: Elf64_Off,
    pub sh_size: Elf64_Xword,
    pub sh_link: Elf64_Word,
    pub sh_info: Elf64_Word,
    pub sh_addralign: Elf64_Xword,
    pub sh_entsize: Elf64_Xword,
}

//
// Section types
//

pub const SHT_NULL: Elf64_Word = 0;
pub const SHT_PROGBITS: Elf64_Word = 1;
pub const SHT_SYMTAB: Elf64_Word = 2;
pub const SHT_STRTAB: Elf64_Word = 3;
pub const SHT_RELA: Elf64_Word = 4;
pub const SHT_HASH: Elf64_Word = 5;
pub const SHT_DYNAMIC: Elf64_Word = 6;
pub const SHT_NOTE: Elf64_Word = 7;
pub const SHT_NOBITS: Elf64_Word = 8;
pub const SHT_REL: Elf64_Word = 9;
pub const SHT_SHLIB: Elf64_Word = 10;
pub const SHT_DYNSYM: Elf64_Word = 11;
pub const SHT_INIT_ARRAY: Elf64_Word = 14;
pub const SHT_FINI_ARRAY: Elf64_Word = 15;
pub const SHT_PREINIT_ARRAY: Elf64_Word = 16;
pub const SHT_GROUP: Elf64_Word = 17;
pub const SHT_SYMTAB_SHNDX: Elf64_Word = 18;
pub const SHT_LOOS: Elf64_Word = 0x60000000;
pub const SHT_HIOS: Elf64_Word = 0x6fffffff;
pub const SHT_LOPROC: Elf64_Word = 0x70000000;
pub const SHT_HIPROC: Elf64_Word = 0x7fffffff;
pub const SHT_LOUSER: Elf64_Word = 0x80000000;
pub const SHT_HIUSER: Elf64_Word = 0xffffffff;

//
// Section Attribute Flags
//

pub const SHF_WRITE: Elf64_Word = 0x1;
pub const SHF_ALLOC: Elf64_Word = 0x2;
pub const SHF_EXECINSTR: Elf64_Word = 0x4;
pub const SHF_MERGE: Elf64_Word = 0x10;
pub const SHF_STRINGS: Elf64_Word = 0x20;
pub const SHF_INFO_LINK: Elf64_Word = 0x40;
pub const SHF_LINK_ORDER: Elf64_Word = 0x80;
pub const SHF_OS_NONCONFORMING: Elf64_Word = 0x100;
pub const SHF_GROUP: Elf64_Word = 0x200;
pub const SHF_TLS: Elf64_Word = 0x400;
pub const SHF_COMPRESSED: Elf64_Word = 0x800;
pub const SHF_MASKOS: Elf64_Word = 0x0ff00000;
pub const SHF_MASKPROC: Elf64_Word = 0xf0000000;

//
// Compression
//

pub struct Elf64_Chdr {
    pub ch_type: Elf64_Word,
    pub ch_reserved: Elf64_Word,
    pub ch_size: Elf64_Xword,
    pub ch_addralign: Elf64_Xword,
}

pub const ELFCOMPRESS_ZLIB: Elf64_Word = 1;
pub const ELFCOMPRESS_LOOS: Elf64_Word = 0x60000000;
pub const ELFCOMPRESS_HIOS: Elf64_Word = 0x6fffffff;
pub const ELFCOMPRESS_LOPROC: Elf64_Word = 0x70000000;
pub const ELFCOMPRESS_HIPROC: Elf64_Word = 0x7fffffff;

//
// Section Group Flags
//

pub const GRP_COMDAT: u32 = 0x1;
pub const GRP_MASKOS: u32 = 0x0ff00000;
pub const GRP_MASKPROC: u32 = 0xf0000000;
