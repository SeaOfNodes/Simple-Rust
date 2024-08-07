use crate::syntax::ast::{Enum, EnumMember, Struct, StructMember};
use crate::syntax::parser::Parser;
use crate::syntax::tokenizer::Kind;

impl<'a, 'b> Parser<'a, 'b> {
    fn parse_enum_member(&mut self) -> Result<EnumMember, ()> {
        let name = self.eat_identifier()?;
        Ok(EnumMember { name })
    }

    pub(super) fn parse_enum(&mut self) -> Result<Enum, ()> {
        self.eat(Kind::Enum)?;
        let name = self.eat_identifier()?;

        let parent = if self.peek_kind(Kind::Colon) {
            self.eat(Kind::Colon).unwrap();
            let ty = self.parse_type()?;
            Some(ty)
        } else {
            None
        };

        let members = if self.peek_kind(Kind::OpenBrace) {
            Some(self.list(
                Kind::OpenBrace,
                Self::parse_enum_member,
                Kind::Comma,
                Kind::CloseBrace,
                true,
            )?)
        } else {
            None
        };

        Ok(Enum {
            name,
            parent,
            members,
        })
    }

    fn parse_struct_member(&mut self) -> Result<StructMember, ()> {
        let name = self.eat_identifier()?;

        self.eat(Kind::Colon)?;
        let ty = self.parse_type()?;

        Ok(StructMember { name, ty })
    }

    pub(super) fn parse_struct(&mut self) -> Result<Struct, ()> {
        self.eat(Kind::Struct)?;
        let name = self.eat_identifier()?;

        let members = if self.peek_kind(Kind::OpenBrace) {
            Some(self.list(
                Kind::OpenBrace,
                Self::parse_struct_member,
                Kind::Comma,
                Kind::CloseBrace,
                true,
            )?)
        } else {
            None
        };

        Ok(Struct { name, members })
    }
}

#[cfg(test)]
mod tests {
    use crate::syntax::parser::Parser;

    #[test]
    fn test_parse_struct() {
        Parser::test("struct Point {\n    x: int,\n    y: int,\n}", |p| {
            p.parse_struct()
        });
    }

    #[test]
    fn test_parse_enum() {
        Parser::test("enum Color {\n    Red,\n    Green,\n}", |p| p.parse_enum());
        Parser::test("enum Empty: Int {}", |p| p.parse_enum());
    }
}
