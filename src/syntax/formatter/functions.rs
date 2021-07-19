use crate::syntax::ast::{Function, Parameter};
use crate::syntax::formatter::{FormatCode, Formatter};
use crate::syntax::tokenizer::Kind;

impl FormatCode for Function {
    fn fmt(&self, f: &mut Formatter<'_, '_>) -> std::fmt::Result {
        f.write_token(Kind::Fun)?;
        f.write_space()?;
        f.write_identifier(&self.name)?;
        f.write_token(Kind::OpenParenthesis)?;
        for (i, param) in self.parameters.iter().enumerate() {
            if i > 0 {
                f.write_token(Kind::Comma)?;
                f.write_space()?;
            }
            param.fmt(f)?;
        }
        f.write_token(Kind::CloseParenthesis)?;

        if let Some(return_type) = &self.return_type {
            f.write_space()?;
            f.write_token(Kind::SingleArrowRight)?;
            f.write_space()?;
            return_type.fmt(f)?;
        }

        if let Some(body) = &self.body {
            f.write_space()?;
            body.fmt(f)?;
        } else {
            f.write_token(Kind::Semicolon)?;
        }

        Ok(())
    }
}

impl FormatCode for Parameter {
    fn fmt(&self, f: &mut Formatter<'_, '_>) -> std::fmt::Result {
        f.write_identifier(&self.name)?;
        f.write_token(Kind::Colon)?;
        f.write_space()?;
        self.typ.fmt(f)
    }
}