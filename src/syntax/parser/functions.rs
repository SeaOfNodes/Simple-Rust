use crate::syntax::ast::{Function, Parameter};
use crate::syntax::parser::Parser;
use crate::syntax::tokenizer::Kind;

impl<'a, 'b> Parser<'a, 'b> {
    fn parameter(&mut self) -> Result<Parameter, ()> {
        let name = self.eat_identifier()?;
        self.eat(Kind::Colon)?;
        let typ = self.parse_type()?;
        Ok(Parameter { name, typ })
    }

    pub(super) fn function(&mut self) -> Result<Function, ()> {
        self.eat(Kind::Fun)?;
        let name = self.eat_identifier()?;

        let parameters = self.list(
            Kind::OpenParenthesis,
            Parser::parameter,
            Kind::Comma,
            Kind::CloseParenthesis,
            false,
        )?;

        let return_type = if self.peek_kind(Kind::SingleArrowRight) {
            self.eat(Kind::SingleArrowRight)?;
            Some(self.parse_type()?)
        } else {
            None
        };

        let body = if self.peek_kind(Kind::Semicolon) {
            self.eat(Kind::Semicolon).unwrap();
            None
        } else {
            Some(self.block()?)
        };

        Ok(Function {
            name,
            body,
            parameters,
            return_type,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::syntax::parser::Parser;

    #[test]
    fn test_parameter() {
        Parser::test("foo: Int", |parser| parser.parameter());
    }

    #[test]
    fn test_empty_function() {
        Parser::test("fun foo() {}", |parser| parser.function());
    }

    #[test]
    fn test_function() {
        Parser::test(
            "fun foo(a: Int, b: Int) -> Int {\n    return a + b;\n}",
            |parser| parser.function(),
        );
    }
}
