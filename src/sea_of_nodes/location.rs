use std::fmt::{Display, Formatter};

use crate::sea_of_nodes::types::{Ty, Type};
use crate::syntax::ast;

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub struct Location<'t> {
    pub module: Ty<'t>,
    pub line: u32,
    pub column: u32,
}

impl Display for Location<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let module = self.module.unwrap_module();
        let file = module.file.to_string_lossy();
        write!(
            f,
            "{} {:?}:{}:{}",
            module.ast.name, file, self.line, self.column
        )
    }
}

impl<'t> Location<'t> {
    pub fn new(location: ast::Location, module: Ty<'t>) -> Self {
        assert!(matches!(*module, Type::Module(_)));
        Self {
            module,
            line: location.line,
            column: location.column,
        }
    }
}

impl ast::Location {
    pub fn with_module(self, module: Ty) -> Location {
        Location::new(self, module)
    }
}