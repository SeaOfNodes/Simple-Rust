use std::fmt;
use std::fmt::{Display, Write};

use crate::syntax::ast;
use crate::syntax::tokenizer::Kind;

mod tokens;
mod types;
mod adts;
mod expressions;
mod functions;
mod module;

#[derive(Copy, Clone)]
pub struct CodeStyle {
    indent_char: char,
    indent_char_count: usize,
    indent_level: usize,
    clrf: bool,
}

impl CodeStyle {
    pub const DEFAULT: CodeStyle = CodeStyle {
        indent_char: ' ',
        indent_char_count: 4,
        indent_level: 0,
        clrf: false,
    };
}

pub struct Formatter<'a, 'fmt> {
    style: CodeStyle,
    f: &'a mut fmt::Formatter<'fmt>,
}

impl Formatter<'_, '_> {
    fn write_newline(&mut self) -> fmt::Result {
        if self.style.clrf {
            self.f.write_str("\r\n")
        } else {
            self.f.write_str("\n")
        }
    }

    fn next_line(&mut self) -> fmt::Result {
        self.write_newline()?;
        self.write_indentation()
    }

    fn indent(&mut self) {
        self.style.indent_level += 1;
    }
    fn dedent(&mut self) {
        self.style.indent_level -= 1;
    }

    fn write_indentation(&mut self) -> fmt::Result {
        for _ in 0..self.style.indent_char_count * self.style.indent_level {
            self.f.write_char(self.style.indent_char)?;
        }
        Ok(())
    }

    fn write_string_literal(&mut self, s: &str) -> fmt::Result {
        self.f.write_str("\"")?;
        for c in s.chars() {
            match c {
                '"' => self.f.write_str("\\\"")?,
                '\\' => self.f.write_str("\\\\")?,
                '\n' => self.f.write_str("\\n")?,
                '\r' => self.f.write_str("\\r")?,
                '\t' => self.f.write_str("\\t")?,
                _ => self.f.write_char(c)?,
            }
        }
        self.f.write_str("\"")
    }

    fn write_identifier(&mut self, identifier: &ast::Identifier) -> fmt::Result {
        self.f.write_str(&identifier.value)
    }

    fn write_token(&mut self, kind: Kind) -> fmt::Result {
        self.f.write_str(kind.str_value().unwrap())
    }

    fn write_space(&mut self) -> fmt::Result {
        self.f.write_str(" ")
    }
}


pub trait FormatCode {
    fn format<'s>(&self, style: &'s CodeStyle) -> Displayable<'_, 's, Self> {
        Displayable { ast: self, style }
    }

    fn fmt(&self, f: &mut Formatter<'_, '_>) -> fmt::Result;
}

pub struct Displayable<'a, 's, Ast: FormatCode + ?Sized> {
    ast: &'a Ast,
    style: &'s CodeStyle,
}

impl<'a, 's, Ast: FormatCode + ?Sized> Display for Displayable<'a, 's, Ast> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut formatter = Formatter {
            style: self.style.clone(),
            f,
        };
        <Ast as FormatCode>::fmt(self.ast, &mut formatter)
    }
}

