use std::iter::Peekable;
use std::mem;
use std::path::Path;

pub use expressions::{Associativity, infix_precedence, Precedence};

use crate::syntax::ast::{Identifier, Item, Location, ModuleAst};
use crate::syntax::tokenizer::{Kind, Token, Tokenizer};

mod adts;
mod expressions;
mod functions;
mod types;

pub struct Parser<'a, 'b> {
    // TODO maybe collect to a vector instead of peekable
    tokenizer: Peekable<Tokenizer<'a>>,
    source: &'a str,
    path: &'b Path,
    errors: Vec<String>,
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(source: &'a str, path: &'b Path) -> Self {
        Self {
            tokenizer: Tokenizer::new(source).peekable(),
            source,
            path,
            errors: vec![],
        }
    }

    #[cfg(test)]
    fn test<Ast: super::formatter::FormatCode, E: std::fmt::Debug>(
        source: &str,
        action: impl FnOnce(&mut Parser) -> Result<Ast, E>,
    ) {
        let mut parser = Parser::new(source, "dummy-path.ro".as_ref());
        let ast = action(&mut parser);
        match ast {
            Ok(ast) => {
                let formatted = format!("{}", ast.format(&super::formatter::CodeStyle::DEFAULT));
                assert_eq!((source, parser.peek()), (formatted.as_str(), None));
            }
            Err(e) => {
                assert!(
                    false,
                    "parse error: {:?}  next_token: {:?}",
                    e,
                    parser.peek()
                );
            }
        }
    }

    #[cfg(test)]
    fn test_fails<Ast: super::formatter::FormatCode + std::fmt::Debug, E: std::fmt::Debug>(
        source: &str,
        action: impl FnOnce(&mut Parser) -> Result<Ast, E>,
    ) {
        let mut parser = Parser::new(source, "dummy-path.ro".as_ref());
        let ast = action(&mut parser);
        match ast {
            Ok(ast) => {
                assert!(
                    false,
                    "expected parse error, but got {:?}, next_token: {:?}",
                    ast,
                    parser.peek()
                );
            }
            Err(_) => {
                // task failed successfully
            }
        }
    }

    fn eat_unexpected_token(&mut self) {
        let message = match self.next() {
            Ok(token) => format!(
                "{}:{}:{}: Unexpected token '{}'",
                self.path.to_str().unwrap(),
                token.line,
                token.column,
                &self.source[token.start..token.end],
            ),
            Err(()) => format!("{}: Unexpected end of file", self.path.to_str().unwrap(),),
        };
        self.errors.push(message);
    }

    fn parse_internal(&mut self) -> Result<ModuleAst, ()> {
        let name = "TODO".to_string();
        let mut module = ModuleAst {
            name,
            items: vec![],
        };
        while let Some(next) = self.peek().cloned() {
            match next.kind {
                Kind::Fun => {
                    let Ok(function) = self.function() else {
                        self.eat_unexpected_token();
                        return Err(());
                    };
                    module.items.push(Item::Function(function));
                }
                Kind::Enum => {
                    let Ok(e) = self.parse_enum() else {
                        self.eat_unexpected_token();
                        return Err(());
                    };
                    module.items.push(Item::Enum(e));
                }
                Kind::Struct => {
                    let Ok(e) = self.parse_struct() else {
                        self.eat_unexpected_token();
                        return Err(());
                    };
                    module.items.push(Item::Struct(e));
                }
                _ => {
                    self.eat_unexpected_token();
                    return Err(());
                }
            }
        }
        Ok(module)
    }

    pub fn parse(mut self) -> Result<ModuleAst, Vec<String>> {
        let path = self.path;
        let module = self.parse_internal();

        #[cfg(debug_assertions)]
        if let Ok(module) = &module {
            use crate::syntax::formatter::{CodeStyle, FormatCode};
            let f = format!("{}", FormatCode::format(module, &CodeStyle::DEFAULT));
            let reparsed = Parser::new(&f, &path).parse_internal().unwrap();
            let f2 = format!("{}", FormatCode::format(&reparsed, &CodeStyle::DEFAULT));
            assert_eq!(f, f2);
            // TODO: also check that they result in equal intermediate representations
        }

        assert_eq!(self.errors.is_empty(), module.is_ok());
        module.map_err(|()| mem::take(&mut self.errors))
    }

    fn eat(&mut self, kind: Kind) -> Result<Token, ()> {
        if self.peek_kind(kind) {
            self.next()
        } else {
            Err(())
        }
    }

    fn next(&mut self) -> Result<Token, ()> {
        self.tokenizer.next().ok_or(())
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokenizer.peek()
    }

    // TODO make this accept an array of kinds
    fn peek_kind(&mut self, kind: Kind) -> bool {
        self.peek().map(|it| it.kind == kind).unwrap_or(false)
    }

    fn peek_not_kind(&mut self, kind: Kind) -> bool {
        self.peek().map(|it| it.kind != kind).unwrap_or(false)
    }

    fn eat_identifier(&mut self) -> Result<Identifier, ()> {
        let token = self.eat(Kind::Identifier)?;
        Ok(self.to_identifier(&token))
    }

    fn to_identifier(&self, token: &Token) -> Identifier {
        assert!(matches!(&token.kind, Kind::Identifier));
        Identifier {
            value: self.source[token.start..token.end].to_string(),
            location: self.location(token),
        }
    }

    fn location(&self, token: &Token) -> Location {
        Location {
            line: token.line,
            column: token.column,
        }
    }

    fn list<T, E: From<()>>(
        &mut self,
        open: Kind,
        parse: fn(&mut Parser<'a, 'b>) -> Result<T, E>,
        separator: Kind,
        close: Kind,
        allow_trailing_separator: bool,
    ) -> Result<Vec<T>, E> {
        self.eat(open)?;

        let mut result = vec![];
        if self.peek_not_kind(close) {
            loop {
                let element = parse(self)?;
                result.push(element);
                if self.peek_kind(close) {
                    break;
                } else {
                    self.eat(separator)?;
                    if self.peek_kind(close) {
                        if allow_trailing_separator {
                            break;
                        } else {
                            todo!("report error")
                        }
                    }
                }
            }
        }
        self.eat(close)?;

        Ok(result)
    }
}
