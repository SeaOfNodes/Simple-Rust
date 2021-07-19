use crate::elf::data_types::*;

#[repr(C)]
pub struct Elf64_Ehdr {
    pub e_ident: [unsigned_char; EI_NIDENT],
    pub e_type: Elf64_Half,
    pub e_machine: Elf64_Half,
    pub e_version: Elf64_Word,
    pub e_entry: Elf64_Addr,
    pub e_phoff: Elf64_Off,
    pub e_shoff: Elf64_Off,
    pub e_flags: Elf64_Word,
    pub e_ehsize: Elf64_Half,
    pub e_phentsize: Elf64_Half,
    pub e_phnum: Elf64_Half,
    pub e_shentsize: Elf64_Half,
    pub e_shnum: Elf64_Half,
    pub e_shstrndx: Elf64_Half,
}

pub const ET_NONE: u16 = 0;
pub const ET_REL: u16 = 1;
pub const ET_EXEC: u16 = 2;
pub const ET_DYN: u16 = 3;
pub const ET_CORE: u16 = 4;
pub const ET_LOOS: u16 = 0xfe00;
pub const ET_HIOS: u16 = 0xfeff;
pub const ET_LOPROC: u16 = 0xff00;
pub const ET_HIPROC: u16 = 0xffff;

pub const EM_X86_64: u16 = 62;

pub const EV_NONE: u8 = 0;
pub const EV_CURRENT: u8 = 1;

//
// ELF Identification
//

pub const EI_MAG0: usize = 0;
pub const EI_MAG1: usize = 1;
pub const EI_MAG2: usize = 2;
pub const EI_MAG3: usize = 3;
pub const EI_CLASS: usize = 4;
pub const EI_DATA: usize = 5;
pub const EI_VERSION: usize = 6;
pub const EI_OSABI: usize = 7;
pub const EI_ABIVERSION: usize = 8;
pub const EI_PAD: usize = 9;
pub const EI_NIDENT: usize = 16;

pub const ELFMAG0: u8 = 0x7f;
pub const ELFMAG1: u8 = 'E' as u8;
pub const ELFMAG2: u8 = 'L' as u8;
pub const ELFMAG3: u8 = 'F' as u8;

pub const ELFCLASSNONE: u8 = 0;
pub const ELFCLASS32: u8 = 1;
pub const ELFCLASS64: u8 = 2;

pub const ELFDATANONE: u8 = 0;
pub const ELFDATA2LSB: u8 = 1;
pub const ELFDATA2MSB: u8 = 2;

pub const ELFOSABI_NONE: u8 = 0;
pub const ELFOSABI_HPUX: u8 = 1;
pub const ELFOSABI_NETBSD: u8 = 2;
pub const ELFOSABI_GNU: u8 = 3;
pub const ELFOSABI_LINUX: u8 = 3;
pub const ELFOSABI_SOLARIS: u8 = 6;
pub const ELFOSABI_AIX: u8 = 7;
pub const ELFOSABI_IRIX: u8 = 8;
pub const ELFOSABI_FREEBSD: u8 = 9;
pub const ELFOSABI_TRU64: u8 = 10;
pub const ELFOSABI_MODESTO: u8 = 11;
pub const ELFOSABI_OPENBSD: u8 = 12;
pub const ELFOSABI_OPENVMS: u8 = 13;
pub const ELFOSABI_NSK: u8 = 14;
pub const ELFOSABI_AROS: u8 = 15;
pub const ELFOSABI_FENIXOS: u8 = 16;
pub const ELFOSABI_CLOUDABI: u8 = 17;
pub const ELFOSABI_OPENVOS: u8 = 18;
