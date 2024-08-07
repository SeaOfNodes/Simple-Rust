use std::fmt;

use crate::syntax::ast;
use crate::syntax::formatter::{FormatCode, Formatter};
use crate::syntax::tokenizer::Kind;

impl FormatCode for ast::Type {
    fn fmt(&self, f: &mut Formatter<'_, '_>) -> fmt::Result {
        match self {
            ast::Type::Identifier(i) => f.write_identifier(i),
            ast::Type::Pointer(t) => {
                f.write_token(Kind::Circumflex)?;
                t.fmt(f)
            }
        }
    }
}
