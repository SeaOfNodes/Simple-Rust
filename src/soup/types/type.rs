use std::hash::Hash;
use std::path::PathBuf;

use crate::soup::types::Ty;

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub enum Type<'a> {
    Bot,
    Top,
    Int { value: i64, constant: bool },
    Module(TyModule),
    Todo(Ty<'a>),
}

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub struct TyModule {
    pub name: String,
    pub path: PathBuf,
}

impl<'t> Type<'t> {
    pub fn is_constant(&'t self) -> bool {
        match self {
            Type::Bot => false,
            Type::Top => true,
            Type::Int { constant, .. } => *constant,
            Type::Module { .. } => todo!(),
            Type::Todo(_) => unreachable!(),
        }
    }
    pub fn unwrap_int(&'t self) -> i64 {
        match self {
            Type::Int { value, .. } => *value,
            _ => unreachable!(),
        }
    }

    pub fn unwrap_module(&'t self) -> &'t TyModule {
        match self {
            Type::Module(m) => m,
            _ => unreachable!(),
        }
    }
}
