use std::cell::Cell;

use crate::syntax::ast::{
    BinaryOperator, Block, Call, Expression, If, PrefixOperator, Return, Statement, Var,
};
use crate::syntax::parser::Parser;
use crate::syntax::tokenizer::{Kind, Operator, Value};

#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone)]
pub enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl Precedence {
    fn next(self) -> Option<Precedence> {
        Some(match self {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => return None,
        })
    }
}

pub enum Associativity {
    Left,
    Right,
}

fn prefix_precedence(token: Kind) -> Option<Precedence> {
    match token {
        Kind::Op(Operator::Plus) | Kind::Op(Operator::Minus) => Some(Precedence::Unary),
        Kind::Op(Operator::Multiply) | Kind::Ampersand => Some(Precedence::Unary),
        _ => None,
    }
}

fn postfix_precedence(token: Kind) -> Option<Precedence> {
    Some(match token {
        Kind::Dot => Precedence::Call,
        Kind::OpenParenthesis => Precedence::Call,
        _ => return None,
    })
}

pub fn infix_precedence(token: Kind) -> Option<(Precedence, Associativity)> {
    let res = match token {
        Kind::Op(Operator::Equal) => (Precedence::Assignment, Associativity::Right),
        Kind::Or => (Precedence::Or, Associativity::Left),
        Kind::And => (Precedence::And, Associativity::Left),
        Kind::OpEq(Operator::Equal) | Kind::OpEq(Operator::Not) => {
            (Precedence::Equality, Associativity::Left)
        }
        Kind::Op(Operator::Less)
        | Kind::Op(Operator::Greater)
        | Kind::OpEq(Operator::Less)
        | Kind::OpEq(Operator::Greater) => (Precedence::Comparison, Associativity::Left),
        Kind::Op(Operator::Plus) | Kind::Op(Operator::Minus) => {
            (Precedence::Term, Associativity::Left)
        }
        Kind::Op(Operator::Multiply) | Kind::Op(Operator::Divide) => {
            (Precedence::Factor, Associativity::Left)
        }
        _ => return None,
    };
    Some(res)
}

impl<'a, 'b> Parser<'a, 'b> {
    /// https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    pub fn parse_expression(&mut self, min_precedence: Precedence) -> Result<Expression, ()> {
        // primary
        let mut result = match self.peek().ok_or(())?.kind {
            Kind::Identifier => {
                let name = self.eat_identifier().unwrap();
                Expression::Identifier(name)
            }
            Kind::Integer => {
                if let Value::Integer(v) = self.eat(Kind::Integer).unwrap().value.unwrap() {
                    Expression::Immediate(v)
                } else {
                    return Err(());
                }
            }
            Kind::Str => {
                let string = self.eat(Kind::Str).unwrap();
                let value = match string.value {
                    Some(Value::Str(s)) => s,
                    _ => unreachable!(),
                };
                Expression::String(value)
            }
            Kind::False => {
                self.eat(Kind::False).unwrap();
                Expression::Boolean(false)
            }
            Kind::True => {
                self.eat(Kind::True).unwrap();
                Expression::Boolean(true)
            }
            Kind::OpenParenthesis => {
                self.eat(Kind::OpenParenthesis).unwrap();
                let expression = self.parse_expression(Precedence::None)?;
                self.eat(Kind::CloseParenthesis)?;
                Expression::Parenthesized(Box::from(expression))
            }
            token_kind => {
                let p = prefix_precedence(token_kind).ok_or(())?;
                let op = match token_kind {
                    Kind::Ampersand => PrefixOperator::AddressOf,
                    Kind::Op(Operator::Multiply) => PrefixOperator::Dereference,
                    Kind::Op(Operator::Plus) => PrefixOperator::Plus,
                    Kind::Op(Operator::Minus) => PrefixOperator::Minus,
                    _ => unreachable!("prefix_precedence returned Some"),
                };
                self.eat(token_kind).unwrap();
                let expression = self.parse_expression(p)?;
                Expression::Prefix {
                    operator: op,
                    operand: Box::from(expression),
                }
            }
        };

        while let Some(token) = self.peek() {
            // postfix
            if let Some(p) = postfix_precedence(token.kind) {
                if p < min_precedence {
                    break;
                }
                result = match token.kind {
                    Kind::Dot => {
                        self.eat(Kind::Dot).unwrap();
                        let right = self.eat_identifier()?;
                        Expression::Dot {
                            left: Box::from(result),
                            right,
                            enum_value_hack: Cell::new(0),
                        }
                    }
                    Kind::OpenParenthesis => {
                        let arguments = self.list(
                            Kind::OpenParenthesis,
                            |p| p.parse_expression(Precedence::None),
                            Kind::Comma,
                            Kind::CloseParenthesis,
                            true,
                        )?;
                        Expression::Call(Box::from(Call {
                            callee: result,
                            arguments,
                        }))
                    }
                    _ => unreachable!("postfix_precedence returned Some"),
                };
                continue;
            }

            // infix
            if let Some((p, a)) = infix_precedence(token.kind) {
                if p < min_precedence {
                    break;
                }
                let right_precedence = match a {
                    Associativity::Left => {
                        p.next().expect("not maximum because p < min_precedence")
                    }
                    Associativity::Right => p,
                };

                result = match self.peek().expect("infix_precedence was Some").kind {
                    binary_op => {
                        let token = self.eat(binary_op).unwrap();
                        let operator = match binary_op {
                            Kind::Op(Operator::Equal) => BinaryOperator::Assign,
                            Kind::Or => BinaryOperator::Or,
                            Kind::And => BinaryOperator::And,
                            Kind::OpEq(Operator::Equal) => BinaryOperator::Equal,
                            Kind::OpEq(Operator::Not) => BinaryOperator::NotEqual,
                            Kind::Op(Operator::Less) => BinaryOperator::LessThan,
                            Kind::Op(Operator::Greater) => BinaryOperator::GreaterThan,
                            Kind::OpEq(Operator::Less) => BinaryOperator::LessOrEqual,
                            Kind::OpEq(Operator::Greater) => BinaryOperator::GreaterOrEqual,
                            Kind::Op(Operator::Plus) => BinaryOperator::Plus,
                            Kind::Op(Operator::Minus) => BinaryOperator::Minus,
                            Kind::Op(Operator::Multiply) => BinaryOperator::Multiply,
                            Kind::Op(Operator::Divide) => BinaryOperator::Divide,
                            _ => unreachable!("infix_precedence was not None"),
                        };
                        let rhs = self.parse_expression(right_precedence)?;
                        Expression::Binary {
                            operator,
                            location: self.location(&token),
                            left: Box::from(result),
                            right: Box::from(rhs),
                        }
                    }
                };
                continue;
            }
            break;
        }

        Ok(result)
    }

    pub(super) fn block(&mut self) -> Result<Block, ()> {
        let mut statements = vec![];

        self.eat(Kind::OpenBrace)?;

        while self.peek_not_kind(Kind::CloseBrace) {
            let statement = self.statement()?;
            statements.push(statement);
        }

        self.eat(Kind::CloseBrace)?;

        Ok(Block { statements })
    }

    fn statement(&mut self) -> Result<Statement, ()> {
        let result = match self.peek().ok_or(())?.kind {
            Kind::Return => {
                self.eat(Kind::Return)?;
                let value = self.parse_expression(Precedence::None)?;
                self.eat(Kind::Semicolon)?;
                Statement::Return(Return { value })
            }
            Kind::Var => {
                self.eat(Kind::Var).unwrap();
                let name = self.eat_identifier()?;

                let ty = if self.peek_kind(Kind::Colon) {
                    self.eat(Kind::Colon).unwrap();
                    Some(self.parse_type()?)
                } else {
                    None
                };
                self.eat(Kind::Op(Operator::Equal))?;
                let expression = self.parse_expression(Precedence::None)?;
                self.eat(Kind::Semicolon)?;

                Statement::Var(Var {
                    name,
                    ty,
                    expression,
                })
            }
            Kind::If => {
                let mut i = If {
                    cases: vec![],
                    else_block: None,
                };

                loop {
                    if self.peek_kind(Kind::If) {
                        self.eat(Kind::If).unwrap();
                        let condition = self.parse_expression(Precedence::None)?;
                        let block = self.block()?;
                        i.cases.push((condition, block));

                        if self.peek_kind(Kind::Else) {
                            self.eat(Kind::Else).unwrap();
                        } else {
                            break;
                        }
                    } else {
                        i.else_block = Some(self.block()?);
                        break;
                    }
                }

                Statement::If(i)
            }
            _ => {
                let expression = self.parse_expression(Precedence::None)?;
                self.eat(Kind::Semicolon)?;
                Statement::Expression(expression)
            }
        };

        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use crate::syntax::ast::{BinaryOperator, Expression};
    use crate::syntax::parser::expressions::Precedence;
    use crate::syntax::parser::Parser;

    #[test]
    fn test_expression_immediate() {
        Parser::test("42", |p| p.parse_expression(Precedence::None));
    }

    #[test]
    fn test_expression_boolean() {
        Parser::test("true", |p| p.parse_expression(Precedence::None));
        Parser::test("false", |p| p.parse_expression(Precedence::None));
    }

    #[test]
    fn test_expression_plus() {
        Parser::test("1 + 2", |p| p.parse_expression(Precedence::None));
        Parser::test("1 + 2 + 3", |p| p.parse_expression(Precedence::None));
        Parser::test("1 + (2 + 3) + 4", |p| p.parse_expression(Precedence::None));
    }

    #[test]
    fn test_expression_terms_and_factors() {
        Parser::test("1 + 2 * 3", |p| p.parse_expression(Precedence::None));
        Parser::test("1 * 2 + 3", |p| p.parse_expression(Precedence::None));
        Parser::test("1 + 2 + 3 * 4 * 5 + 6", |p| {
            p.parse_expression(Precedence::None)
        });
        Parser::test("1 * 2 * 3 + 4 + 5 * 6", |p| {
            p.parse_expression(Precedence::None)
        });
    }

    #[test]
    fn test_expression_equal() {
        Parser::test("1 == 2", |p| p.parse_expression(Precedence::None));
        Parser::test("true == (2 == 3)", |p| p.parse_expression(Precedence::None));
    }

    #[test]
    fn test_expression_dot() {
        Parser::test("foo.bar", |p| p.parse_expression(Precedence::None));
        Parser::test("foo.bar.baz", |p| p.parse_expression(Precedence::None));
    }

    #[test]
    fn test_expression_call() {
        Parser::test("foo()", |p| p.parse_expression(Precedence::None));
        Parser::test("foo(1)", |p| p.parse_expression(Precedence::None));
        Parser::test("foo(1, 2)", |p| p.parse_expression(Precedence::None));
    }

    #[test]
    fn test_expression_identifier() {
        Parser::test("identifier", |p| p.parse_expression(Precedence::None));
        Parser::test("Identifier_2", |p| p.parse_expression(Precedence::None));
    }

    #[test]
    fn test_expression_string() {
        Parser::test("\"string\"", |p| p.parse_expression(Precedence::None));
        Parser::test("\"\\\"\\\\\\n\\r\\t\"", |p| {
            p.parse_expression(Precedence::None)
        });

        let test_char = |c: char| {
            let s = format!("\"\\{c}\""); // "\{c}"
            Parser::test(&s, |p| p.parse_expression(Precedence::None));

            let s = format!("\"(\\{c})\""); // "(\{c})"
            Parser::test(&s, |p| p.parse_expression(Precedence::None));
        };
        test_char('"');
        test_char('\\');
        test_char('n');
        test_char('r');
        test_char('t');
    }

    #[test]
    fn test_expression_address_of() {
        Parser::test("&address_of", |p| p.parse_expression(Precedence::None));
    }

    #[test]
    fn test_expression_dereference() {
        Parser::test("*dereference", |p| p.parse_expression(Precedence::None));
    }

    #[test]
    fn test_expression_parenthesized() {
        Parser::test("(1)", |p| p.parse_expression(Precedence::None));
        Parser::test("((1))", |p| p.parse_expression(Precedence::None));
        Parser::test("(((1)))", |p| p.parse_expression(Precedence::None));
    }

    #[test]
    fn test_block() {
        Parser::test("{}", |p| p.block());
        Parser::test("{\n    foo();\n}", |p| p.block());
    }

    #[test]
    fn test_statement_return() {
        Parser::test("return 42;", |p| p.statement());
    }

    #[test]
    fn test_statement_expression() {
        Parser::test("foo(32);", |p| p.statement());
    }

    #[test]
    fn test_statement_var() {
        Parser::test("var foo = 42;", |p| p.statement());
        Parser::test("var foo: Int = 42;", |p| p.statement());
    }

    #[test]
    fn test_statement_assignment() {
        Parser::test("foo = 42;", |p| p.statement());
        Parser::test("foo.bar = 42;", |p| p.statement());
    }

    #[test]
    fn test_statement_if() {
        Parser::test("if true {}", |p| p.statement());
        Parser::test("if true {} else {}", |p| p.statement());
        Parser::test("if true {} else if false {} else {}", |p| p.statement());
    }

    #[test]
    fn test_comparisons() {
        Parser::test("2 >= (3 >= 4)", |p| p.parse_expression(Precedence::None));
        Parser::test("(2 >= 3) >= 4", |p| p.parse_expression(Precedence::None));

        Parser::test("2 == (3 == 4)", |p| p.parse_expression(Precedence::None));
        Parser::test("2 == (3 != 4)", |p| p.parse_expression(Precedence::None));
        Parser::test("(2 == 3) != 4", |p| p.parse_expression(Precedence::None));

        Parser::test("2 >= 3 >= 4", |p| p.parse_expression(Precedence::None));
        Parser::test("2 >= 3 > 4", |p| p.parse_expression(Precedence::None));

        Parser::test("2 == 3 == 4", |p| p.parse_expression(Precedence::None));
        Parser::test("2 == 3 != 4", |p| p.parse_expression(Precedence::None));
    }

    #[test]
    fn test_invalid() {
        Parser::test_fails("2-", |p| p.parse_expression(Precedence::None));
    }

    #[test]
    fn associativity_add() {
        Parser::test("1 + 2 + 3", |p| {
            let e = p.parse_expression(Precedence::None);
            let Ok(Expression::Binary {
                operator: BinaryOperator::Plus,
                location,
                left,
                right,
            }) = &e
            else {
                unreachable!()
            };
            assert!(matches!(**right, Expression::Immediate(3)));
            e
        });
    }
}
