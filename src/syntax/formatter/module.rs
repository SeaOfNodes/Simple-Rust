use crate::syntax::ast::{Item, ModuleAst};
use crate::syntax::formatter::{FormatCode, Formatter};

impl FormatCode for ModuleAst {
    fn fmt(&self, f: &mut Formatter<'_, '_>) -> std::fmt::Result {
        for (i, item) in self.items.iter().enumerate() {
            if i > 0 {
                f.next_line()?;
                f.next_line()?;
            }
            item.fmt(f)?;
        }
        f.next_line()
    }
}

impl FormatCode for Item {
    fn fmt(&self, f: &mut Formatter<'_, '_>) -> std::fmt::Result {
        match self {
            Item::Function(fun) => fun.fmt(f),
            Item::Enum(enu) => enu.fmt(f),
            Item::Struct(stru) => stru.fmt(f),
        }
    }
}