use crate::syntax::ast::Type;
use crate::syntax::parser::Parser;
use crate::syntax::tokenizer::Kind;

impl<'a, 'b> Parser<'a, 'b> {
    pub(super) fn parse_type(&mut self) -> Result<Type, ()> {
        let result = match self.peek().ok_or(())?.kind {
            Kind::Identifier => {
                let identifier = self.eat(Kind::Identifier)?;
                Ok(Type::Identifier(self.to_identifier(&identifier)))
            }
            Kind::Circumflex => {
                self.eat(Kind::Circumflex)?;
                let t = self.parse_type()?;
                Ok(Type::Pointer(Box::from(t)))
            }
            _ => todo!("handle error")
        };
        result
    }
}

#[cfg(test)]
mod tests {
    use crate::syntax::parser::Parser;

    #[test]
    fn test_format_type() {
        Parser::test("int", |p| p.parse_type());
        Parser::test("^int", |p| p.parse_type());
        Parser::test("^^int", |p| p.parse_type());
        Parser::test("^^^int", |p| p.parse_type());
    }
}
