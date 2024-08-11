use std::fmt::{Display, Formatter};

use crate::sea_of_nodes::types::Ty;

pub struct ParserLocation {
    pub line: u32,
    pub column: u32,
}

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub struct Location<'t> {
    pub module: Ty<'t>,
    pub line: u32,
    pub column: u32,
}

impl Display for Location<'_> {
    fn fmt(&self, _f: &mut Formatter<'_>) -> std::fmt::Result {
        // let module = self.module.unwrap_module();
        // let file = module.file.to_string_lossy();
        // write!(
        //     f,
        //     "{} {:?}:{}:{}",
        //     module.ast.name, file, self.line, self.column
        // )
        todo!()
    }
}

impl<'t> Location<'t> {
    pub fn new(location: ParserLocation, module: Ty<'t>) -> Self {
        // assert!(matches!(*module, Type::Module(_)));
        Self {
            module,
            line: location.line,
            column: location.column,
        }
    }
}

impl ParserLocation {
    pub fn with_module(self, module: Ty) -> Location {
        Location::new(self, module)
    }
}
