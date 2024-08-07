use std::hash::Hash;

use crate::soup::types::Ty;

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub enum Type<'a> {
    Bot,
    Top,
    Int(i64),
    Todo(Ty<'a>),
}

impl<'t> Type<'t> {
    pub fn unwrap_int(&'t self) -> i64 {
        match self {
            Type::Int(i) => *i,
            _ => unreachable!(),
        }
    }
}
