#![feature(const_option)]
#![feature(hash_raw_entry)]
#![feature(get_many_mut)]

use std::collections::VecDeque;
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;
use std::sync::Arc;

use crate::arena::Arena;
use crate::asm::{FunctionSymbol, MachineCode};
use crate::modules::{Modules, ParsedModule};
use crate::soup::soup::Soup;
use crate::soup::types::{Ty, Types};
use crate::syntax::ast::Item;

pub mod arena;
pub mod asm;
pub mod bit_set;
pub mod elf;
pub mod lir;
mod modules;
pub mod soup;
pub mod syntax;
pub mod id_vec;

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

pub fn compile(build_file: PathBuf, _build_args: Vec<String>) -> Result<PathBuf, ()> {
    // worklist
    let mut unique_worklist_tasks = 0;
    let max_unique_worklist_tasks = 1000;
    let max_retries = 100;
    let mut worklist = VecDeque::with_capacity(max_unique_worklist_tasks);
    let mut iteration = 0;
    let mut last_iteration_with_progress = 0;

    let mut modules = Modules::new();

    let type_arena = Arena::new();
    let mut types = Types::new(&type_arena);

    {
        let module = modules
            .read_and_parse(Arc::new(build_file.clone()))
            .map_err(|e| {
                eprintln!("Could not read file {build_file:?}: {e}");
            })?;

        // if path and source are identical the old ast will be used
        let ty = types.get_module(module.clone());

        for i in 0..module.ast.items.len() {
            if unique_worklist_tasks < max_unique_worklist_tasks {
                unique_worklist_tasks += 1;
                worklist.push_back((0, module.clone(), ty, i));
            } else {
                eprintln!("Too many items");
                return Err(());
            }
        }
    }

    let mut hirs = Vec::<Hir>::new();

    let mut progress = true;
    while let Some((retries, module, ty, item)) = worklist.pop_front() {
        progress = true;

        match compile_item(&mut types, &module, ty, item) {
            Outcome::Done => {}
            Outcome::Hir(hir) => {
                hirs.push(hir);
            }
            Outcome::Retry => {
                if retries + 1 > max_retries {
                    eprintln!("Too many retries for task");
                    return Err(());
                }
                worklist.push_back((retries + 1, module, ty, item));
                progress = false;
            }
            Outcome::ParseImport(path) => {
                let path = Arc::new(path);
                let module = modules.read_and_parse(path.clone()).map_err(|e| {
                    eprintln!("Could not read file {path:?}: {e}");
                })?;
                let ty = types.get_module(module.clone());

                for i in 0..module.ast.items.len() {
                    if unique_worklist_tasks < max_unique_worklist_tasks {
                        unique_worklist_tasks += 1;
                        worklist.push_back((0, module.clone(), ty, i));
                    } else {
                        eprintln!("Too many items");
                        return Err(());
                    }
                }
            }
        }
        if progress {
            last_iteration_with_progress = iteration;
        } else if last_iteration_with_progress < iteration - worklist.len() {
            break;
        }
        iteration += 1;
    }

    if !worklist.is_empty() {
        if !progress {
            eprintln!("Couldn't make progress. Probably cyclic dependencies.");
        } else {
            eprintln!("Not enough steps.");
        }
        return Err(());
    }

    let mut code = MachineCode::new();
    for hir in hirs.iter() {
        let symbol = match hir {
            Hir::Soup(_soup) => {
                todo!()
                // let lir = hir.lower();
                //
                // let code_offset = code.bytes.len() as u64;
                // lir.assemble(&mut code);
                // FunctionSymbol {
                //     name: hir.symbol.clone(),
                //     code_offset,
                //     code_size: code.bytes.len() as u64 - code_offset,
                //     is_extern: false,
                // }
            }
            Hir::ExternFunction { symbol } => FunctionSymbol {
                name: symbol.clone(),
                code_offset: 0,
                code_size: 0,
                is_extern: true,
            },
        };
        code.function_symbols.push(symbol);
    }

    let main = build_file;
    let object_path = main.with_extension("o");
    let executable_path = main.with_extension("");

    // I used the following command as a reference, but a few sections are missing:
    //     gcc main.c -O -c -fno-asynchronous-unwind-tables
    {
        let mut file = std::fs::File::create(&object_path).unwrap();
        let source_file_name = main.file_name().unwrap().to_str().unwrap();
        elf::write_to_file(&mut file, source_file_name, &code).unwrap();
        file.flush().unwrap();
    }

    // use gcc instead of ld because it knows where files like crt1.o are
    let output = Command::new("gcc")
        .arg("-o")
        .arg(&executable_path)
        .arg(&object_path)
        .output()
        .unwrap();
    if !output.status.success() {
        panic!("{:?}", output);
    }
    std::fs::remove_file(object_path).unwrap();

    Ok(executable_path)
}

enum Hir<'t> {
    Soup(Soup<'t>),
    ExternFunction { symbol: String },
}

enum Outcome<'c> {
    Done,
    Hir(Hir<'c>),
    Retry,
    ParseImport(PathBuf),
}

fn compile_item<'c>(
    types: &mut Types<'c>,
    module: &Arc<ParsedModule>,
    _module_ty: Ty<'c>,
    item: usize,
) -> Outcome<'c> {
    let item = &module.ast.as_ref().items[item];

    match item {
        Item::Function(f) => match f.body {
            None => Outcome::Hir(Hir::ExternFunction {
                symbol: f.name.value.clone(),
            }),
            Some(_) => {
                let mut soup = Soup::new();
                match soup.compile_function(f, types) {
                    Ok(_) => Outcome::Hir(Hir::Soup(soup)),
                    Err(_) => todo!(),
                }
            }
        },
        Item::Enum(_) => {
            // let parent = if let Some(parent) = &e.parent {
            //     todo!()
            //     // let ty = scopes.lookup_type(None, types, parent);
            //     // if ty.is_none() {
            //     //     return Outcome::Retry;
            //     // }
            //     // ty
            // } else {
            //     None
            // };
            //
            // let location = e.name.location.with_module(module_ty);
            // let members = e.members.as_ref().map(|o| {
            //     o.iter()
            //         .enumerate()
            //         .map(|(i, m)| (m.name.value.clone(), i as i64))
            //         .collect()
            // });
            // // types.get_enum(location, parent, members);
            //
            Outcome::Done
        }
        Item::Struct(_) => {
            // let location = s.name.location.with_module(module_ty);
            //
            // // let struct_ty = types.get_struct(location);
            //
            // if let Some(members) = &s.members {
            //     for member in members {
            //         // let ty = scopes.lookup_type(None, types, &member.ty);
            //         // if ty.is_none() {
            //         //     return Outcome::Retry;
            //         // }
            //         todo!()
            //     }
            // }
            //
            Outcome::Done
        }
    }
}
