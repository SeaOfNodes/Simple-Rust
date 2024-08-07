use std::hash::Hash;

use crate::soup::types::Ty;

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub enum Type<'a> {
    Bot,
    Top,
    Int { value: i64, constant: bool },
    Todo(Ty<'a>),
}

impl<'t> Type<'t> {
    pub fn is_constant(&'t self) -> bool {
        match self {
            Type::Bot => false,
            Type::Top => true,
            Type::Int { constant, .. } => *constant,
            Type::Todo(_) => unreachable!(),
        }
    }
    pub fn unwrap_int(&'t self) -> i64 {
        match self {
            Type::Int { value, .. } => *value,
            _ => unreachable!(),
        }
    }
}
