#[derive(Debug)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct ModuleAst {
    pub name: String,
    pub items: Vec<Item>,
}

#[derive(Debug, Copy, Clone)]
pub struct Location {
    pub line: u32,
    pub column: u32,
}

#[derive(Debug)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum Item {
    Function(Function),
    Enum(Enum),
    Struct(Struct),
}

#[derive(Debug)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Function {
    pub name: Identifier,
    /// None for extern functions
    pub body: Option<Block>,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
}

#[derive(Debug)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Enum {
    pub name: Identifier,
    pub parent: Option<Type>,
    /// None for extern enums
    pub members: Option<Vec<EnumMember>>,
}

#[derive(Debug)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct EnumMember {
    pub name: Identifier,
}

#[derive(Debug)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Struct {
    pub name: Identifier,
    /// None for extern structs
    pub members: Option<Vec<StructMember>>,
}

#[derive(Debug)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct StructMember {
    pub name: Identifier,
    pub ty: Type,
}

#[derive(Debug)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Parameter {
    pub name: Identifier,
    pub typ: Type,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum BinaryOperator {
    Assign,
    Or,
    And,
    Equal,
    NotEqual,
    Plus,
    Minus,
    Multiply,
    Divide,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum PrefixOperator {
    Plus,
    Minus,
    AddressOf,
    Dereference,
}

#[derive(Debug)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum Expression {
    Immediate(i64),
    Boolean(bool),
    Prefix {
        operator: PrefixOperator,
        operand: Box<Expression>,
    },
    Binary {
        operator: BinaryOperator,
        location: Location,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Dot {
        left: Box<Expression>,
        right: Identifier,
    },
    Call(Box<Call>),
    Identifier(Identifier),
    String(String),
    Parenthesized(Box<Expression>),
    Block(Block),
}

#[derive(Debug)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum Type {
    Identifier(Identifier),
    // Call(Box<Call>),TODO generic types: List(I32); fun List(t: TypeId) -> TypeId { /* builtin */ }
    Pointer(Box<Type>),
}

#[derive(Debug)]
pub struct Identifier {
    pub value: String,
    pub location: Location,
}

#[derive(Debug)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Call {
    pub callee: Expression,
    pub arguments: Vec<Expression>,
}

#[derive(Debug)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum Statement {
    Expression(Expression),
    Return(Return),
    If(If),
    Var(Var),
    Meta(Identifier),
}

#[derive(Debug)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Return {
    pub value: Expression,
}

#[derive(Debug)]
pub struct If {
    // Unlike {condition, then_expr, optional_else_expr} this
    // representation enforces that both branches are blocks.
    // 
    // This might have been simpler:
    //     enum IfElse { If(Expression, Block, Option<Box<IfElse>>), Else(Block) }
    pub cases: Vec<(Expression, Block)>,
    pub else_block: Option<Block>,
}

#[derive(Debug)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Var {
    pub name: Identifier,
    pub ty: Option<Type>,
    pub expression: Expression,
}

#[derive(Debug)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Assignment {
    pub target: Expression,
    pub expression: Expression,
}

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for Identifier {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(Identifier {
            // TODO allow more variables?
            value: u.choose(&["a", "b", "c", "x", "y", "z"])?.to_string(),
            location: Location::arbitrary(u)?,
        })
    }
}

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for Location {
    fn arbitrary(_: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(Location { line: 0, column: 0 }) // we don't care about locations
    }
}

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for If {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let mut cases = Vec::arbitrary(u)?;
        cases.push(arbitrary::Arbitrary::arbitrary(u)?); // ensure non-empty
        Ok(If {
            cases,
            else_block: Block::arbitrary(u).ok(),
        })
    }
}
