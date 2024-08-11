//! Useful links:
//! - x86\_64
//! - Intel Manual: https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf
//! - encoding: http://www.c-jump.com/CIS77/CPU/x86/lecture.html
//! - System V ABI:
//! - ELF Files: https://www.sco.com/developers/gabi/latest/contents.html
//! - 2012: https://refspecs.linuxbase.org/elf/x86_64-abi-0.99.pdf
//! - 2018: https://raw.githubusercontent.com/wiki/hjl-tools/x86-psABI/x86-64-psABI-1.0.pdf
//!
pub mod asm;
pub mod elf;
pub mod lir;
