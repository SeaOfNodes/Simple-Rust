use crate::syntax::ast::{
    Assignment, BinaryOperator, Block, Call, Expression, If, PrefixOperator, Return, Statement, Var,
};
use crate::syntax::formatter::{FormatCode, Formatter};
use crate::syntax::parser::{infix_precedence, Associativity};
use crate::syntax::tokenizer::{Kind, Operator};

fn binary_operator_token(op: BinaryOperator) -> Kind {
    match op {
        BinaryOperator::Assign => Kind::Op(Operator::Equal),
        BinaryOperator::Or => Kind::Or,
        BinaryOperator::And => Kind::And,
        BinaryOperator::Equal => Kind::OpEq(Operator::Equal),
        BinaryOperator::NotEqual => Kind::OpEq(Operator::Not),
        BinaryOperator::Plus => Kind::Op(Operator::Plus),
        BinaryOperator::Minus => Kind::Op(Operator::Minus),
        BinaryOperator::Multiply => Kind::Op(Operator::Multiply),
        BinaryOperator::Divide => Kind::Op(Operator::Divide),
        BinaryOperator::LessThan => Kind::Op(Operator::Less),
        BinaryOperator::LessOrEqual => Kind::OpEq(Operator::Less),
        BinaryOperator::GreaterThan => Kind::Op(Operator::Greater),
        BinaryOperator::GreaterOrEqual => Kind::OpEq(Operator::Greater),
    }
}

impl FormatCode for Expression {
    fn fmt(&self, f: &mut Formatter<'_, '_>) -> std::fmt::Result {
        match self {
            Expression::Immediate(value) => {
                write!(f.f, "{value}")
            }
            Expression::Boolean(v) => f.write_token(if *v { Kind::True } else { Kind::False }),
            Expression::Prefix { operator, operand } => {
                let token_kind = match operator {
                    PrefixOperator::Plus => Kind::Op(Operator::Plus),
                    PrefixOperator::Minus => Kind::Op(Operator::Minus),
                    PrefixOperator::AddressOf => Kind::Ampersand,
                    PrefixOperator::Dereference => Kind::Op(Operator::Multiply),
                };
                f.write_token(token_kind)?;
                // NOTE: add parentheses to avoid invalid syntax trees during fuzzing
                if matches!(**operand, Expression::Binary { .. }) {
                    f.write_token(Kind::OpenParenthesis)?;
                }
                operand.fmt(f)?;
                if matches!(**operand, Expression::Binary { .. }) {
                    f.write_token(Kind::CloseParenthesis)?;
                }
                Ok(())
            }
            Expression::Binary {
                operator,
                location: _,
                left,
                right,
            } => {
                // NOTE: add parentheses to avoid invalid syntax trees during fuzzing
                let token_kind = binary_operator_token(*operator);
                let (p_outer, a_outer) = infix_precedence(token_kind).unwrap();

                let should_parenthesize = |e: &Expression, is_left: bool| {
                    if let Expression::Binary {
                        operator: inner_operator,
                        ..
                    } = e
                    {
                        let (p_inner, a_inner) =
                            infix_precedence(binary_operator_token(*inner_operator)).unwrap();
                        p_inner < p_outer
                            || (p_inner == p_outer
                                && is_left
                                && matches!(a_outer, Associativity::Right))
                            || (p_inner == p_outer
                                && !is_left
                                && matches!(a_outer, Associativity::Left))
                    } else {
                        false
                    }
                };

                if should_parenthesize(left, true) {
                    f.write_token(Kind::OpenParenthesis)?;
                    left.fmt(f)?;
                    f.write_token(Kind::CloseParenthesis)?;
                } else {
                    left.fmt(f)?;
                }

                f.write_space()?;
                f.write_token(token_kind)?;
                f.write_space()?;

                if should_parenthesize(right, false) {
                    f.write_token(Kind::OpenParenthesis)?;
                    right.fmt(f)?;
                    f.write_token(Kind::CloseParenthesis)?;
                } else {
                    right.fmt(f)?;
                }
                Ok(())
            }
            Expression::Dot {
                left,
                right,
                enum_value_hack,
            } => {
                left.fmt(f)?;
                f.write_token(Kind::Dot)?;
                f.write_identifier(right)
            }
            Expression::Call(call) => call.fmt(f),
            Expression::Identifier(i) => f.write_identifier(i),
            Expression::String(s) => f.write_string_literal(s),
            Expression::Parenthesized(expr) => {
                f.write_token(Kind::OpenParenthesis)?;
                expr.fmt(f)?;
                f.write_token(Kind::CloseParenthesis)
            }
        }
    }
}

impl FormatCode for Call {
    fn fmt(&self, f: &mut Formatter<'_, '_>) -> std::fmt::Result {
        self.callee.fmt(f)?;
        f.write_token(Kind::OpenParenthesis)?;
        for (i, arg) in self.arguments.iter().enumerate() {
            if i > 0 {
                f.write_token(Kind::Comma)?;
                f.write_space()?;
            }
            arg.fmt(f)?;
        }
        f.write_token(Kind::CloseParenthesis)
    }
}

impl FormatCode for Block {
    fn fmt(&self, f: &mut Formatter<'_, '_>) -> std::fmt::Result {
        f.write_token(Kind::OpenBrace)?;
        if !self.statements.is_empty() {
            f.indent();
            for statement in &self.statements {
                f.next_line()?;
                statement.fmt(f)?;
            }
            f.dedent();
            f.next_line()?;
        }
        f.write_token(Kind::CloseBrace)
    }
}

impl FormatCode for Statement {
    fn fmt(&self, f: &mut Formatter<'_, '_>) -> std::fmt::Result {
        match self {
            Statement::Expression(e) => {
                e.fmt(f)?;
                f.write_token(Kind::Semicolon)
            }
            Statement::Return(r) => r.fmt(f),
            Statement::If(i) => i.fmt(f),
            Statement::Var(v) => v.fmt(f),
        }
    }
}

impl FormatCode for Return {
    fn fmt(&self, f: &mut Formatter<'_, '_>) -> std::fmt::Result {
        f.write_token(Kind::Return)?;
        f.write_space()?;
        self.value.fmt(f)?;
        f.write_token(Kind::Semicolon)
    }
}

impl FormatCode for Var {
    fn fmt(&self, f: &mut Formatter<'_, '_>) -> std::fmt::Result {
        f.write_token(Kind::Var)?;
        f.write_space()?;
        f.write_identifier(&self.name)?;
        if let Some(ty) = &self.ty {
            f.write_token(Kind::Colon)?;
            f.write_space()?;
            ty.fmt(f)?;
        }
        f.write_space()?;
        f.write_token(Kind::Op(Operator::Equal))?;
        f.write_space()?;
        self.expression.fmt(f)?;
        f.write_token(Kind::Semicolon)
    }
}

impl FormatCode for Assignment {
    fn fmt(&self, f: &mut Formatter<'_, '_>) -> std::fmt::Result {
        self.target.fmt(f)?;
        f.write_space()?;
        f.write_token(Kind::Op(Operator::Equal))?;
        f.write_space()?;
        self.expression.fmt(f)?;
        f.write_token(Kind::Semicolon)
    }
}

impl FormatCode for If {
    fn fmt(&self, f: &mut Formatter<'_, '_>) -> std::fmt::Result {
        for (i, case) in self.cases.iter().enumerate() {
            if i > 0 {
                f.write_space()?;
                f.write_token(Kind::Else)?;
                f.write_space()?;
            }
            f.write_token(Kind::If)?;
            f.write_space()?;
            case.0.fmt(f)?;
            f.write_space()?;
            case.1.fmt(f)?;
        }

        if let Some(else_block) = &self.else_block {
            f.write_space()?;
            f.write_token(Kind::Else)?;
            f.write_space()?;
            else_block.fmt(f)?;
        }
        Ok(())
    }
}
