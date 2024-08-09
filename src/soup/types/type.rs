use std::fmt;
use std::fmt::Display;
use std::hash::Hash;
use std::sync::Arc;

use crate::modules::ParsedModule;
use crate::soup::types::Ty;

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub enum Type<'a> {
    Bot,  // all
    Top,  // any
    Ctrl, // control flow bottom
    Int(Int),
    Tuple { types: Vec<Ty<'a>> },
    Module(Arc<ParsedModule>),
}

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub enum Int {
    Bot,
    Top,
    Constant(i64),
}

impl<'t> Type<'t> {
    pub fn is_constant(&'t self) -> bool {
        match self {
            Type::Bot => false,
            Type::Top => true,
            Type::Ctrl => false,
            Type::Int(i) => matches!(i, Int::Constant(_)),
            Type::Tuple { .. } => false,
            Type::Module { .. } => todo!(),
        }
    }

    pub fn unwrap_int(&'t self) -> i64 {
        match self {
            Type::Int(Int::Constant(value)) => *value,
            _ => unreachable!(),
        }
    }

    pub fn unwrap_module(&'t self) -> &'t ParsedModule {
        match self {
            Type::Module(m) => m,
            _ => unreachable!(),
        }
    }
}

impl<'t> Display for Type<'t> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Type::Bot => "Bot",
            Type::Top => "Top",
            Type::Ctrl => "Ctrl",
            Type::Int(Int::Bot) => "IntBot",
            Type::Int(Int::Top) => "IntTop",
            Type::Int(Int::Constant(c)) => return write!(f, "{c}"),
            Type::Tuple { types } => {
                write!(f, "[")?;
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{ty}")?;
                }
                return write!(f, "]");
            }
            Type::Module(_) => todo!(),
        })
    }
}
