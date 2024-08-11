#![feature(const_option)]
#![feature(hash_raw_entry)]
#![feature(get_many_mut)]

use std::path::PathBuf;

pub mod datastructures;
pub mod sea_of_nodes;

#[derive(Debug, Eq, PartialEq)]
pub enum ExitCode {
    Success = 0,
    Failure,
    Usage,
    InvalidFile,
}

pub fn run(main: PathBuf, _build_args: Vec<String>) -> ExitCode {
    if !main.is_file() || main.extension().map(|it| it != "ro").unwrap_or(true) {
        eprintln!("Not a .ro file: '{}'", main.to_string_lossy());
        return ExitCode::InvalidFile;
    }

    match compile(main, _build_args) {
        Ok(_) => ExitCode::Success,
        Err(_) => ExitCode::Failure,
    }
}

pub fn compile(_build_file: PathBuf, _build_args: Vec<String>) -> Result<PathBuf, ()> {
    todo!()
}
