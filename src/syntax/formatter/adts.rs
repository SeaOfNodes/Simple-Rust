use std::fmt;

use crate::syntax::ast;
use crate::syntax::formatter::{FormatCode, Formatter};
use crate::syntax::tokenizer::Kind;

impl FormatCode for ast::Struct {
    fn fmt(&self, f: &mut Formatter<'_, '_>) -> fmt::Result {
        f.write_token(Kind::Struct)?;
        f.write_space()?;
        f.write_identifier(&self.name)?;
        if let Some(members) = &self.members {
            f.write_space()?;
            f.write_token(Kind::OpenBrace)?;
            if !members.is_empty() {
                f.indent();
                for member in members {
                    f.next_line()?;
                    member.fmt(f)?;
                    f.write_token(Kind::Comma)?;
                }
                f.dedent();
                f.next_line()?;
            }
            f.write_token(Kind::CloseBrace)?;
        }
        Ok(())
    }
}

impl FormatCode for ast::StructMember {
    fn fmt(&self, f: &mut Formatter<'_, '_>) -> fmt::Result {
        f.write_identifier(&self.name)?;
        f.write_token(Kind::Colon)?;
        f.write_space()?;
        self.ty.fmt(f)
    }
}
impl FormatCode for ast::Enum {
    fn fmt(&self, f: &mut Formatter<'_, '_>) -> fmt::Result {
        f.write_token(Kind::Enum)?;
        f.write_space()?;
        f.write_identifier(&self.name)?;
        if let Some(parent) = &self.parent {
            f.write_token(Kind::Colon)?;
            f.write_space()?;
            parent.fmt(f)?;
        }
        if let Some(members) = &self.members {
            f.write_space()?;
            f.write_token(Kind::OpenBrace)?;
            if !members.is_empty() {
                f.indent();
                for member in members {
                    f.next_line()?;
                    member.fmt(f)?;
                    f.write_token(Kind::Comma)?;
                }
                f.dedent();
                f.next_line()?;
            }
            f.write_token(Kind::CloseBrace)?;
        }
        Ok(())
    }
}

impl FormatCode for ast::EnumMember {
    fn fmt(&self, f: &mut Formatter<'_, '_>) -> fmt::Result {
        f.write_identifier(&self.name)
    }
}
