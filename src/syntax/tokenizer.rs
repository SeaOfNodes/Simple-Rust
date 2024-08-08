use std::str::{Chars, FromStr};

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: Kind,
    pub line: u32,
    pub column: u32,
    pub start: usize,
    pub end: usize,
    pub value: Option<Value>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    // special
    Identifier,
    Comment,
    Error,

    // punctuation
    Ampersand,
    Circumflex,
    At,
    Hash,
    Colon,
    Comma,
    Dot,
    Semicolon,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    OpenParenthesis,
    CloseParenthesis,
    SingleArrowRight,
    DoubleArrowRight,

    // operators
    And,
    Or,
    In,
    Is,
    Op(Operator),
    OpEq(Operator),

    // control flow
    If,
    Else,
    Return,
    When,
    Defer,
    For,
    Foreach,
    Loop,
    While,
    Break,
    Continue,

    // literals
    Null,
    True,
    False,
    Integer,
    Float,
    Str,

    // declarations
    Fun,
    Enum,
    Struct,
    Union,
    Val,
    Var,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Not,
    Less,
    Greater,
    Equal,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Str(String),
    Error(String),
    Integer(i64),
    Float { float64: f64, float32: f32 },
}

pub struct Tokenizer<'a> {
    iter: Chars<'a>,
    source: &'a str,
    line: u32,
    column: u32,
}

impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            iter: source.chars(),
            source,
            line: 1,
            column: 0,
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.column += 1;
        self.iter.next()
    }

    // TODO is cloning efficient?
    fn peek(&mut self) -> Option<char> {
        self.iter.clone().next()
    }

    fn offset(&self) -> usize {
        self.source.len() - self.iter.as_str().len()
    }
}

macro_rules! switch {
    ($_self:ident, $default:expr, $($char:pat => $kind:expr),*) => {
        match $_self.peek() {
            $(Some($char) => {
                $_self.advance();
                $kind
            })*
            _ => $default,
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        use Kind::*;
        use Operator::*;
        loop {
            let start = self.offset();
            let mut value = None;
            let kind = match self.advance()? {
                ' ' | '\r' => continue,
                '\n' => {
                    self.line += 1;
                    self.column = 1;
                    continue;
                }
                '(' => OpenParenthesis,
                ')' => CloseParenthesis,
                '[' => OpenBracket,
                ']' => CloseBracket,
                '{' => OpenBrace,
                '}' => CloseBrace,
                '.' => Dot,
                ':' => Colon,
                ',' => Comma,
                ';' => Semicolon,
                '=' => switch!(self, Op(Equal), '=' => OpEq(Equal), '>' => DoubleArrowRight),
                '>' => switch!(self, Op(Greater), '=' => OpEq(Greater)),
                '<' => switch!(self, Op(Less), '=' => OpEq(Less)),
                '!' => switch!(self, Op(Not), '=' => OpEq(Not)),
                '+' => switch!(self, Op(Plus), '=' => OpEq(Plus)),
                '-' => switch!(self, Op(Minus), '=' => OpEq(Minus), '>' => SingleArrowRight),
                '*' => switch!(self, Op(Multiply), '=' => OpEq(Multiply)),
                '/' => match self.peek() {
                    Some('/') => {
                        self.advance();
                        while let Some(c) = self.peek() {
                            if c == '\r' || c == '\n' {
                                break;
                            }
                            self.advance();
                        }
                        Comment
                    }
                    _ => switch!(self, Op(Divide), '=' => OpEq(Divide)),
                },
                '^' => Circumflex,
                '&' => Ampersand,
                '@' => At,
                '#' => Hash,
                c if c.is_ascii_digit() => {
                    let mut k = Integer;
                    let mut clone = self.iter.clone();
                    while let Some(c) = clone.next() {
                        if c == '.' {
                            if let Some(c) = clone.next() {
                                if c.is_ascii_digit() {
                                    k = Float;
                                }
                            }
                            break;
                        } else if !c.is_ascii_digit() {
                            break;
                        }
                        self.advance();
                    }

                    if k == Float {
                        self.advance();
                        while let Some(c) = self.peek() {
                            if !c.is_ascii_digit() {
                                break;
                            }
                            self.advance();
                        }
                        let slice = &self.source[start..self.offset()];
                        value = Some(Value::Float {
                            float32: f32::from_str(slice).unwrap(),
                            float64: f64::from_str(slice).unwrap(),
                        })
                    } else {
                        let slice = &self.source[start..self.offset()];
                        value = Some(Value::Integer(i64::from_str(slice).unwrap()));
                    }
                    k
                }
                c if c == '_' || c.is_ascii_alphabetic() => {
                    while let Some(c) = self.peek() {
                        if !c.is_alphanumeric() && c != '_' {
                            break;
                        }
                        self.advance();
                    }
                    let slice = &self.source[start..self.offset()];
                    match slice {
                        "and" => And,
                        "break" => Break,
                        "continue" => Continue,
                        "defer" => Defer,
                        "else" => Else,
                        "enum" => Enum,
                        "false" => False,
                        "for" => For,
                        "foreach" => Foreach,
                        "fun" => Fun,
                        "if" => If,
                        "in" => In,
                        "is" => Is,
                        "loop" => Loop,
                        "null" => Null,
                        "or" => Or,
                        "return" => Return,
                        "struct" => Struct,
                        "true" => True,
                        "union" => Union,
                        "val" => Val,
                        "var" => Var,
                        "when" => When,
                        "while" => While,
                        _ => Identifier,
                    }
                }
                '"' => {
                    let mut terminated = false;
                    let mut invalid_escape = false;
                    let mut v = String::new();
                    while let Some(mut c) = self.peek() {
                        if c == '"' {
                            terminated = true;
                            self.advance();
                            break;
                        }
                        if c == '\r' || c == '\n' {
                            break;
                        }

                        // advance now, so that we can peek ahead for the escape sequence
                        self.advance();

                        if c == '\\' {
                            if let Some(valid) = match self.peek() {
                                Some('"') => Some('"'),
                                Some('\\') => Some('\\'),
                                Some('n') => Some('\n'),
                                Some('r') => Some('\r'),
                                Some('t') => Some('\t'),
                                _ => None,
                            } {
                                c = valid;
                                self.advance();
                            } else {
                                invalid_escape = true;
                            }
                        }

                        v.push(c);
                    }
                    if terminated {
                        if invalid_escape {
                            value = Some(Value::Error(
                                "string with invalid escape sequence".to_string(),
                            ));
                            Error
                        } else {
                            value = Some(Value::Str(v));
                            Str
                        }
                    } else {
                        value = Some(Value::Error("unterminated string literal".to_string()));
                        Error
                    }
                }
                c => {
                    value = Some(Value::Error(format!("unexpected character '{}'", c)));
                    Error
                }
            };

            let end = self.source.len() - self.iter.as_str().len();
            return Some(Token {
                kind,
                line: self.line,
                column: self.column,
                start,
                end,
                value,
            });
        }
    }
}

#[test]
fn tokenize() {
    let source = r#"
        // a comment
        hello
        + "unterminated
        +=
        "string literal"
        89439834 31.4159
        var if true
    "#;
    let mut tokenizer = Tokenizer::new(source);

    let t = tokenizer.next().unwrap();
    assert_eq!(t.kind, Kind::Comment);
    assert_eq!(t.line, 2);

    let t = tokenizer.next().unwrap();
    assert_eq!(t.kind, Kind::Identifier);
    assert_eq!(t.line, 3);

    let t = tokenizer.next().unwrap();
    assert_eq!(t.kind, Kind::Op(Operator::Plus));
    assert_eq!(t.line, 4);

    let t = tokenizer.next().unwrap();
    assert_eq!(t.kind, Kind::Error);
    assert_eq!(t.line, 4);

    let t = tokenizer.next().unwrap();
    assert_eq!(t.kind, Kind::OpEq(Operator::Plus));
    assert_eq!(t.line, 5);

    let t = tokenizer.next().unwrap();
    assert_eq!(t.kind, Kind::Str);
    assert_eq!(t.line, 6);

    let t = tokenizer.next().unwrap();
    assert_eq!(t.kind, Kind::Integer);
    assert_eq!(t.line, 7);
    assert_eq!(t.value.unwrap(), Value::Integer(89439834));

    let t = tokenizer.next().unwrap();
    assert_eq!(t.kind, Kind::Float);
    assert_eq!(t.line, 7);
    assert_eq!(
        t.value.unwrap(),
        Value::Float {
            float64: 31.4159,
            float32: 31.4159,
        }
    );

    assert_eq!(tokenizer.next().unwrap().kind, Kind::Var);
    assert_eq!(tokenizer.next().unwrap().kind, Kind::If);
    assert_eq!(tokenizer.next().unwrap().kind, Kind::True);
    assert_eq!(tokenizer.next(), None);
}
