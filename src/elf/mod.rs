//! See: http://www.sco.com/developers/gabi/latest/contents.html
//!
//! The submodules roughly correspond to the sections in the document and
//! the original names are used for types and constants.
//!

#![allow(non_camel_case_types)]

use std::collections::HashMap;
use std::fs::File;
use std::io::Write;

use header::Elf64_Ehdr;

use crate::asm::{MachineCode, RelocationKind};
use crate::elf::data_types::*;
use crate::elf::header::*;
use crate::elf::relocation::Elf64_Rela;
use crate::elf::section::*;
use crate::elf::string_table::*;
use crate::elf::symbol_table::*;

pub mod data_types;
pub mod header;
pub mod relocation;
pub mod section;
pub mod string_table;
pub mod symbol_table;

pub fn write_to_file(
    file: &mut File,
    source_file_name: &str,
    code: &MachineCode,
) -> std::io::Result<()> {
    let mut header = Elf64_Ehdr {
        e_ident: {
            let mut e_ident = [0 as u8; EI_NIDENT];
            e_ident[EI_MAG0] = ELFMAG0;
            e_ident[EI_MAG1] = ELFMAG1;
            e_ident[EI_MAG2] = ELFMAG2;
            e_ident[EI_MAG3] = ELFMAG3;
            e_ident[EI_CLASS] = ELFCLASS64;
            e_ident[EI_DATA] = if cfg!(target_endian = "little") {
                ELFDATA2LSB
            } else {
                ELFDATA2MSB
            };
            e_ident[EI_VERSION] = EV_CURRENT;
            e_ident[EI_OSABI] = ELFOSABI_NONE;
            e_ident[EI_ABIVERSION] = 0;
            e_ident
        },
        e_type: ET_REL,
        e_machine: EM_X86_64,
        e_version: EV_CURRENT as Elf64_Word,
        // TODO
        e_entry: 0,
        e_phoff: 0,
        e_shoff: 0,
        e_flags: 0,
        e_ehsize: std::mem::size_of::<Elf64_Ehdr>() as Elf64_Half,
        e_phentsize: 0,
        e_phnum: 0,
        e_shentsize: std::mem::size_of::<Elf64_Shdr>() as Elf64_Half,
        e_shnum: 0,
        e_shstrndx: 0,
    };

    let mut shstrtab = StringTableBuilder::new();
    let mut strtab = StringTableBuilder::new();

    let mut null_section = Elf64_Shdr {
        sh_name: 0,
        sh_type: 0,
        sh_flags: 0,
        sh_addr: 0,
        sh_offset: 0,
        sh_size: 0,
        sh_link: 0,
        sh_info: 0,
        sh_addralign: 0,
        sh_entsize: 0,
    };
    let mut shstrtab_section = Elf64_Shdr {
        sh_name: shstrtab.index_of(".shstrtab") as u32,
        sh_type: SHT_STRTAB,
        sh_flags: 0,
        sh_addr: 0,
        sh_offset: 0, // patched later
        sh_size: 0,   // patched later
        sh_link: 0,
        sh_info: 0,
        sh_addralign: 1,
        sh_entsize: 0,
    };
    let strtab_section_index = 2;
    let mut strtab_section = Elf64_Shdr {
        sh_name: shstrtab.index_of(".strtab") as u32,
        sh_type: SHT_STRTAB,
        sh_flags: 0,
        sh_addr: 0,
        sh_offset: 0, // patched later
        sh_size: 0,   // patched later
        sh_link: 0,
        sh_info: 0,
        sh_addralign: 1,
        sh_entsize: 0,
    };
    let symtab_section_index = 3;
    let mut symtab_section = Elf64_Shdr {
        sh_name: shstrtab.index_of(".symtab") as u32,
        sh_type: SHT_SYMTAB,
        sh_flags: 0,
        sh_addr: 0,
        sh_offset: 0, // patched later
        sh_size: 0,   // patched later
        sh_link: strtab_section_index,
        sh_info: 2, // TODO location of .rela.text? One greater than the symbol table index of the last local symbol (binding STB_LOCAL).
        sh_addralign: std::mem::align_of::<Elf64_Sym>() as u64,
        sh_entsize: std::mem::size_of::<Elf64_Sym>() as u64,
    };
    let text_bytes = code.bytes.as_slice();
    let text_section_index = 4;
    let mut text_section = Elf64_Shdr {
        sh_name: shstrtab.index_of(".text") as u32,
        sh_type: SHT_PROGBITS,
        sh_flags: (SHF_ALLOC | SHF_EXECINSTR) as u64,
        sh_addr: 0,
        sh_offset: 0, // patched later
        sh_size: text_bytes.len() as u64,
        sh_link: 0,
        sh_info: 0,
        sh_addralign: 1,
        sh_entsize: 0,
    };
    let mut rela_text_section = Elf64_Shdr {
        sh_name: shstrtab.index_of(".rela.text") as u32,
        sh_type: SHT_RELA,
        sh_flags: SHF_INFO_LINK as u64,
        sh_addr: 0,
        sh_offset: 0, // patched later
        sh_size: (code.relocations.len() * std::mem::size_of::<Elf64_Rela>()) as u64,
        sh_link: symtab_section_index,
        sh_info: text_section_index,
        sh_addralign: std::mem::align_of::<Elf64_Rela>() as u64,
        sh_entsize: std::mem::size_of::<Elf64_Rela>() as u64,
    };
    let rodata_section_index = 6;
    let rodata_bytes = code.ro_data.as_slice();
    let mut rodata_section = Elf64_Shdr {
        sh_name: shstrtab.index_of(".rodata") as u32,
        sh_type: SHT_PROGBITS,
        sh_flags: SHF_ALLOC as u64,
        sh_addr: 0,
        sh_offset: 0, // patched later
        sh_size: rodata_bytes.len() as u64,
        sh_link: 0,
        sh_info: 0,
        sh_addralign: std::mem::align_of::<u8>() as u64,
        sh_entsize: 0,
    };

    let section_count = 7;

    if section_count >= SHN_LORESERVE as usize {
        header.e_shnum = SHN_UNDEF;
        null_section.sh_size = section_count as u64;
    } else {
        header.e_shnum = section_count as u16;
    }

    let shstrndx: Elf64_Word = 1;
    if shstrndx >= SHN_LORESERVE as Elf64_Word {
        header.e_shstrndx = SHN_XINDEX;
        null_section.sh_link = shstrndx;
    } else {
        header.e_shstrndx = shstrndx as u16;
    }

    let mut function_symbtab_index = HashMap::new();
    let mut symbols = vec![
        // reserved
        Elf64_Sym {
            st_name: 0,
            st_info: 0,
            st_other: 0,
            st_shndx: 0,
            st_value: 0,
            st_size: 0,
        },
        Elf64_Sym {
            st_name: strtab.index_of(source_file_name) as u32,
            st_info: (STB_LOCAL << 4) | STT_FILE,
            st_other: STV_DEFAULT,
            st_shndx: SHN_ABS,
            st_value: 0,
            st_size: 0,
        },
    ];

    for symbol in &code.function_symbols {
        let sym = Elf64_Sym {
            // TODO use a different name and link to a c libary
            st_name: strtab.index_of(&symbol.name) as u32,
            // TODO local functions need to be added before global functions
            st_info: (STB_GLOBAL << 4) | STT_FUNC,
            st_other: STV_DEFAULT,
            st_shndx: if symbol.is_extern {
                0
            } else {
                text_section_index
            } as u16, // TODO points to a SHT_SYMTAB_SHNDX section if too large
            st_value: symbol.code_offset,
            st_size: symbol.code_size,
        };
        function_symbtab_index.insert(symbol.name.to_string(), symbols.len());
        symbols.push(sym);
    }
    let rodata_symtab_index = {
        let sym = Elf64_Sym {
            st_name: 0,
            // TODO local functions need to be added before global functions
            st_info: (STB_GLOBAL << 4) | STT_FUNC,
            st_other: STV_DEFAULT,
            st_shndx: rodata_section_index,
            st_value: 0,
            st_size: 0,
        };
        let index = symbols.len();
        symbols.push(sym);
        index
    };
    symtab_section.sh_size = (symbols.len() * std::mem::size_of::<Elf64_Sym>()) as u64;

    let relocations = {
        let mut relocations = vec![];
        for relocation_kind in &code.relocations {
            const R_X86_64_PC32: usize = 2;
            const R_X86_64_PLT32: usize = 4;

            let rela = match relocation_kind {
                RelocationKind::Fn32(offset, name) => {
                    Elf64_Rela {
                        r_offset: *offset as u64,
                        r_info: (function_symbtab_index[name] << 32 | R_X86_64_PLT32) as u64,
                        r_addend: -4, // TODO why -4?
                    }
                }
                RelocationKind::Rodata32(offset, addend) => {
                    Elf64_Rela {
                        r_offset: *offset as u64,
                        r_info: (rodata_symtab_index << 32 | R_X86_64_PC32) as u64,
                        r_addend: addend - 4, // TODO why -4? should it be subtracted here?
                    }
                }
            };
            relocations.push(rela);
        }
        relocations
    };

    let shstrtab_bytes = shstrtab.build();
    shstrtab_section.sh_size = shstrtab_bytes.len() as u64;
    let strtab_bytes = strtab.build();
    strtab_section.sh_size = strtab_bytes.len() as u64;

    // TODO this should be a loop
    // patch remaining offsets
    {
        let mut offset = header.e_ehsize as Elf64_Off;
        header.e_shoff = offset;
        offset += section_count as Elf64_Off * header.e_shentsize as Elf64_Off;
        shstrtab_section.sh_offset = offset;
        offset += shstrtab_section.sh_size;
        strtab_section.sh_offset = offset;
        offset += strtab_section.sh_size;
        symtab_section.sh_offset = offset;
        offset += symtab_section.sh_size;
        text_section.sh_offset = offset;
        offset += text_section.sh_size;
        rela_text_section.sh_offset = offset;
        offset += rela_text_section.sh_size;
        rodata_section.sh_offset = offset;
        offset += rodata_section.sh_size;
        let _ = offset;
    }

    // write to file

    file.write_all(&unsafe {
        std::mem::transmute::<Elf64_Ehdr, [u8; std::mem::size_of::<Elf64_Ehdr>()]>(header)
    })?;

    let sections = vec![
        null_section,
        shstrtab_section,
        strtab_section,
        symtab_section,
        text_section,
        rela_text_section,
        rodata_section,
    ];
    assert_eq!(sections.len(), section_count);
    for shdr in sections {
        file.write_all(unsafe {
            &std::mem::transmute::<Elf64_Shdr, [u8; std::mem::size_of::<Elf64_Shdr>()]>(shdr)
        })?;
    }
    file.write_all(&shstrtab_bytes)?;
    file.write_all(&strtab_bytes)?;
    for symbol in symbols {
        file.write_all(unsafe {
            &std::mem::transmute::<Elf64_Sym, [u8; std::mem::size_of::<Elf64_Sym>()]>(symbol)
        })?;
    }
    file.write_all(text_bytes)?;
    for rela in relocations {
        file.write_all(unsafe {
            &std::mem::transmute::<Elf64_Rela, [u8; std::mem::size_of::<Elf64_Rela>()]>(rela)
        })?;
    }
    file.write_all(rodata_bytes)?;

    Ok(())
}

#[cfg(test)]
pub mod tests {
    use super::*;

    pub struct ViewElfFile<'a> {
        pub bytes: &'a [u8],
        pub header: &'a Elf64_Ehdr,
        pub sections: Vec<ViewSection<'a>>,
    }

    pub struct ViewSection<'a> {
        pub header: &'a Elf64_Shdr,
        pub data: &'a [u8],
    }

    pub fn view_elf_file(bytes: &[u8]) -> ViewElfFile {
        assert!(bytes.len() > std::mem::size_of::<Elf64_Ehdr>());
        let header_pointer =
            bytes[0..std::mem::size_of::<Elf64_Ehdr>()].as_ptr() as *const Elf64_Ehdr;
        let header = unsafe { &*header_pointer };

        assert_eq!(header.e_ehsize as usize, std::mem::size_of::<Elf64_Ehdr>());
        assert_eq!(
            header.e_shentsize as usize,
            std::mem::size_of::<Elf64_Shdr>()
        );

        let sections = {
            let mut sections = Vec::with_capacity(header.e_shnum as usize);
            let offset = header.e_shoff as usize;
            let count = header.e_shnum as usize;
            assert!(offset + count * std::mem::size_of::<Elf64_Shdr>() <= bytes.len());
            for i in 0..count {
                let section = unsafe {
                    let section_pointer = bytes
                        .as_ptr()
                        .add(offset)
                        .add(i * std::mem::size_of::<Elf64_Shdr>())
                        as *const Elf64_Shdr;
                    &*section_pointer
                };

                assert!((section.sh_offset + section.sh_size) as usize <= bytes.len());
                let data: &[u8] = unsafe {
                    std::slice::from_raw_parts(
                        bytes.as_ptr().add(section.sh_offset as usize),
                        section.sh_size as usize,
                    )
                };

                sections.push(ViewSection {
                    header: section,
                    data,
                });
            }
            sections
        };

        ViewElfFile {
            bytes,
            header,
            sections,
        }
    }
}
