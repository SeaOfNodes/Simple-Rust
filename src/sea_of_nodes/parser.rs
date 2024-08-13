use crate::sea_of_nodes::graph_visualizer;
use crate::sea_of_nodes::nodes::{BoolOp, Node, NodeCreation, NodeId, Nodes, ScopeNode};
use crate::sea_of_nodes::types::{Ty, Types};

/// Converts a Simple source program to the Sea of Nodes intermediate representation in one pass.
pub struct Parser<'s, 'mt, 't> {
    /// Tokenizes the source code
    lexer: Lexer<'s>,

    /// Manages interned types.
    types: &'mt mut Types<'t>,

    /// Manages all nodes and implements operations such as peephole.
    /// Keeps track of the current start for constant node creation.
    pub nodes: Nodes<'t>,

    /// returned after parsing
    pub(crate) stop: NodeId,

    /// The current scope changes as we parse.
    pub(crate) scope: NodeId,

    /// stack of scopes for graph visualization
    pub(crate) x_scopes: Vec<NodeId>,
}

type ParseErr = String;
type PResult<T> = Result<T, ParseErr>;

fn is_keyword(s: &str) -> bool {
    matches!(s, "else" | "false" | "if" | "int" | "return" | "true")
}

impl<'s, 'mt, 't> Parser<'s, 'mt, 't> {
    pub fn new(source: &'s str, types: &'mt mut Types<'t>) -> Self {
        Self::new_with_arg(source, types, types.ty_bot)
    }

    pub fn new_with_arg(source: &'s str, types: &'mt mut Types<'t>, arg: Ty<'t>) -> Self {
        let mut nodes = Nodes::new();

        let scope = nodes.create(Node::make_scope());
        nodes.ty[scope] = Some(types.ty_bot); // in java this is done by the constructor

        let args = types.get_tuple(vec![types.ty_ctrl, arg]);
        let start = nodes.create(Node::make_start(args));
        nodes.ty[start] = Some(args);
        nodes.start = start;

        let stop = nodes.create(Node::make_stop());
        nodes.ty[stop] = Some(types.ty_bot); // differs from java; ensures that it isn't dead

        Self {
            lexer: Lexer {
                source,
                remaining: source,
            },
            types,
            nodes,
            stop,
            scope,
            x_scopes: vec![],
        }
    }

    pub fn src(&self) -> &'s str {
        self.lexer.source
    }

    fn ctrl(&mut self) -> NodeId {
        self.nodes.inputs[self.scope][0].expect("has ctrl")
    }
    fn set_ctrl(&mut self, node: NodeId) {
        self.nodes.set_def(self.scope, 0, Some(node));
    }

    fn peephole(&mut self, c: NodeCreation<'t>) -> NodeId {
        self.nodes.create_peepholed(self.types, c)
    }

    pub fn parse(&mut self) -> PResult<NodeId> {
        self.x_scopes.push(self.scope);

        // Enter a new scope for the initial control and arguments
        self.nodes.scope_push(self.scope);

        // project ctrl and arg0 from start
        let start = self.nodes.start;
        let ctrl = self.peephole(Node::make_proj(start, 0, ScopeNode::CTRL.to_string()));
        self.nodes
            .scope_define(self.scope, ScopeNode::CTRL.to_string(), ctrl)
            .expect("not in scope");
        let arg0 = self.peephole(Node::make_proj(start, 1, ScopeNode::ARG0.to_string()));
        self.nodes
            .scope_define(self.scope, ScopeNode::ARG0.to_string(), arg0)
            .expect("not in scope");

        self.parse_block()?;

        self.nodes.scope_pop(self.scope);
        self.x_scopes.pop();

        if !self.lexer.is_eof() {
            Err(format!(
                "Syntax error, unexpected {}",
                self.lexer.get_any_next_token()
            ))
        } else {
            let p = self.nodes.peephole(self.stop, self.types);
            assert_eq!(self.stop, p);
            Ok(self.stop)
        }
    }

    /// <pre>
    ///     '{' statements '}'
    /// </pre>
    ///
    /// Does not parse the opening or closing `{}`
    fn parse_block(&mut self) -> PResult<()> {
        self.nodes.scope_push(self.scope);
        while !self.peek('}') && !self.lexer.is_eof() {
            self.parse_statement()?;
        }
        self.nodes.scope_pop(self.scope);
        Ok(())
    }

    /// <pre>
    ///     returnStatement | declStatement | blockStatement | ifStatement | expressionStatement
    /// </pre>
    fn parse_statement(&mut self) -> PResult<()> {
        if self.matchx("return") {
            self.parse_return()
        } else if self.matchx("int") {
            self.parse_decl()
        } else if self.match_("{") {
            self.parse_block()?;
            self.require("}")
        } else if self.matchx("if") {
            self.parse_if()
        } else if self.matchx("#showGraph") {
            self.show_graph();
            self.require(";")
        } else {
            self.parse_expression_statement()
        }
    }

    /// <pre>
    ///     if ( expression ) statement [else statement]
    /// </pre>
    fn parse_if(&mut self) -> PResult<()> {
        self.require("(")?;

        // Parse predicate
        let pred = self.parse_expression()?;
        self.require(")")?;

        // IfNode takes current control and predicate
        let if_ctrl = self.ctrl();
        let if_node = self.peephole(Node::make_if(if_ctrl, pred));
        self.nodes.keep(if_node);

        // Setup projection nodes
        let if_true = self.peephole(Node::make_proj(if_node, 0, "True".to_string()));
        self.nodes.unkeep(if_node);
        let if_false = self.peephole(Node::make_proj(if_node, 1, "False".to_string()));

        // In if true branch, the ifT proj node becomes the ctrl
        // But first clone the scope and set it as current
        let n_defs = self.nodes.inputs[self.scope].len();
        let mut false_scope = self.nodes.scope_dup(self.scope);
        self.x_scopes.push(false_scope);

        // Parse the true side
        self.set_ctrl(if_true);
        self.parse_statement()?;
        let true_scope = self.scope;

        // Parse the false side
        self.scope = false_scope;
        self.set_ctrl(if_false);
        if self.matchx("else") {
            self.parse_statement()?;
            false_scope = self.scope;
        }

        if self.nodes.inputs[true_scope].len() != n_defs
            || self.nodes.inputs[false_scope].len() != n_defs
        {
            return Err("Cannot define a new name on one arm of an if".to_string());
        }

        // Merge results
        self.scope = true_scope;
        self.x_scopes.pop();

        let region = self.nodes.scope_merge(true_scope, false_scope, self.types);
        self.set_ctrl(region);
        Ok(())
    }

    /// <pre>
    ///     'return' expr ;
    /// </pre>
    ///
    /// Parses a return statement; "return" already parsed.
    /// The $ctrl edge is killed.
    fn parse_return(&mut self) -> PResult<()> {
        let expr = self.parse_expression()?;
        self.require(";")?;
        let ctrl = self.ctrl();
        let ret = self.peephole(Node::make_return(ctrl, expr));
        self.nodes.add_def(self.stop, Some(ret));
        let ctrl = self.peephole(Node::make_constant(self.nodes.start, self.types.ty_xctrl));
        self.set_ctrl(ctrl);
        Ok(())
    }

    /// Dumps out the node graph
    pub fn show_graph(&mut self) {
        let dot = graph_visualizer::generate_dot_output(self).unwrap();
        println!("{dot}");
        // uncomment to open in browser:
        // graph_visualizer::run_graphviz_and_chromium(dot);
    }

    pub fn print(&mut self, node: NodeId) -> String {
        self.nodes.print(Some(node)).to_string()
    }
    pub fn print_stop(&mut self) -> String {
        self.nodes.print(Some(self.stop)).to_string()
    }

    /// <pre>
    ///      name '=' expression ';'
    /// </pre>
    fn parse_expression_statement(&mut self) -> PResult<()> {
        let name = self.require_id()?;
        self.require("=")?;
        let expr = self.parse_expression()?;
        self.require(";")?;

        match self.nodes.scope_update(self.scope, name, expr) {
            Ok(_) => Ok(()),
            Err(()) => Err(format!("Undefined name '{name}'")),
        }
    }

    /// <pre>
    ///     'int' name = expression ';'
    /// </pre>
    fn parse_decl(&mut self) -> PResult<()> {
        // Type is 'int' for now
        let name = self.require_id()?;
        self.require("=")?;
        let expr = self.parse_expression()?;
        self.require(";")?;

        self.nodes
            .scope_define(self.scope, name.to_string(), expr)
            .map_err(|()| format!("Redefining name '{name}'"))
    }

    /// <pre>
    ///     expr : compareExpr
    /// </pre>
    fn parse_expression(&mut self) -> PResult<NodeId> {
        self.parse_comparison()
    }

    /// <pre>
    ///     expr : additiveExpr op additiveExpr
    /// </pre>
    fn parse_comparison(&mut self) -> PResult<NodeId> {
        let lhs = self.parse_addition()?;
        let rhs = Self::parse_comparison;
        if self.match_("==") {
            rhs(self).map(|rhs| self.peephole(Node::make_bool([lhs, rhs], BoolOp::EQ)))
        } else if self.match_("!=") {
            rhs(self)
                .map(|rhs| self.peephole(Node::make_bool([lhs, rhs], BoolOp::EQ)))
                .map(|expr| self.peephole(Node::make_not(expr)))
        } else if self.match_("<=") {
            rhs(self).map(|rhs| self.peephole(Node::make_bool([lhs, rhs], BoolOp::LE)))
        } else if self.match_("<") {
            rhs(self).map(|rhs| self.peephole(Node::make_bool([lhs, rhs], BoolOp::LT)))
        } else if self.match_(">=") {
            rhs(self).map(|rhs| self.peephole(Node::make_bool([rhs, lhs], BoolOp::LE)))
        } else if self.match_(">") {
            rhs(self).map(|rhs| self.peephole(Node::make_bool([rhs, lhs], BoolOp::LT)))
        } else {
            Ok(lhs)
        }
    }

    /// <pre>
    ///     additiveExpr : multiplicativeExpr (('+' | '-') multiplicativeExpr)*
    /// </pre>
    fn parse_addition(&mut self) -> PResult<NodeId> {
        let lhs = self.parse_multiplication()?;
        let rhs = Self::parse_addition;
        if self.match_("+") {
            rhs(self).map(|rhs| self.peephole(Node::make_add([lhs, rhs])))
        } else if self.match_("-") {
            rhs(self).map(|rhs| self.peephole(Node::make_sub([lhs, rhs])))
        } else {
            Ok(lhs)
        }
    }

    /// <pre>
    ///     multiplicativeExpr : unaryExpr (('*' | '/') unaryExpr)*
    /// </pre>
    fn parse_multiplication(&mut self) -> PResult<NodeId> {
        let lhs = self.parse_unary()?;
        let rhs = Self::parse_multiplication;
        if self.match_("*") {
            rhs(self).map(|rhs| self.peephole(Node::make_mul([lhs, rhs])))
        } else if self.match_("/") {
            rhs(self).map(|rhs| self.peephole(Node::make_div([lhs, rhs])))
        } else {
            Ok(lhs)
        }
    }

    /// <pre>
    ///     unaryExpr : ('-') unaryExpr | primaryExpr
    /// </pre>
    fn parse_unary(&mut self) -> PResult<NodeId> {
        if self.match_("-") {
            self.parse_unary()
                .map(|expr| self.peephole(Node::make_minus(expr)))
        } else {
            self.parse_primary()
        }
    }

    /// <pre>
    ///     primaryExpr : integerLiteral | Identifier | true | false | '(' expression ')'
    /// </pre>
    fn parse_primary(&mut self) -> PResult<NodeId> {
        if self.lexer.peek_number() {
            self.parse_integer_literal()
        } else if self.match_("(") {
            let e = self.parse_expression()?;
            self.require(")")?;
            Ok(e)
        } else if self.matchx("true") {
            Ok(self.peephole(Node::make_constant(self.nodes.start, self.types.ty_one)))
        } else if self.matchx("false") {
            Ok(self.peephole(Node::make_constant(self.nodes.start, self.types.ty_zero)))
        } else if let Some(name) = self.lexer.match_id() {
            self.nodes
                .scope_lookup(self.scope, name)
                .map_err(|()| format!("Undefined name '{name}'"))
        } else {
            Err(self.error_syntax("an identifier or expression"))
        }
    }

    /// <pre>
    ///     integerLiteral: [1-9][0-9]* | [0]
    /// </pre>
    fn parse_integer_literal(&mut self) -> PResult<NodeId> {
        self.lexer
            .parse_number(self.types)
            .map(|ty| self.peephole(Node::make_constant(self.nodes.start, ty)))
    }

    //
    // Utilities for lexical analysis
    //

    /// Return true and skip if "syntax" is next in the stream.
    fn match_(&mut self, prefix: &str) -> bool {
        self.lexer.match_(prefix)
    }

    /// Match must be "exact", not be followed by more id letters
    fn matchx(&mut self, prefix: &str) -> bool {
        self.lexer.matchx(prefix)
    }

    /// Return true and do NOT skip if `c` is next
    fn peek(&mut self, c: char) -> bool {
        self.lexer.peek_char(c)
    }

    /// Require and return an identifier
    fn require_id(&mut self) -> PResult<&'s str> {
        let id = self.lexer.match_id();
        id.filter(|i| !is_keyword(i))
            .ok_or_else(|| format!("Expected an identifier, found '{}'", id.unwrap_or("")))
    }

    fn require(&mut self, syntax: &str) -> PResult<()> {
        self.lexer
            .match_(syntax)
            .then_some(())
            .ok_or_else(|| self.error_syntax(syntax))
    }

    fn error_syntax(&mut self, syntax: &str) -> ParseErr {
        format!(
            "Syntax error, expected {syntax}: {}",
            self.lexer.get_any_next_token()
        )
    }
}

struct Lexer<'a> {
    /// The entire input
    source: &'a str,
    /// The remaining input string
    remaining: &'a str,
}

fn is_number(c: char) -> bool {
    c.is_ascii_digit()
}

fn is_id_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_id_letter(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

fn is_punctuation(c: char) -> bool {
    "=;[]<>(){}+-/*!".contains(c)
}

impl<'a> Lexer<'a> {
    fn is_eof(&self) -> bool {
        self.remaining.is_empty()
    }

    fn peek(&self) -> Option<char> {
        self.remaining.chars().next()
    }

    fn next_char(&mut self) -> Option<char> {
        self.peek()
            .inspect(|c| self.remaining = &self.remaining[c.len_utf8()..])
    }

    fn is_whitespace(&self) -> bool {
        self.peek().as_ref().is_some_and(char::is_ascii_whitespace)
    }

    fn skip_whitespace(&mut self) {
        while self.is_whitespace() {
            self.next_char();
        }
    }

    /// skips a prefix if present
    fn skip(&mut self, prefix: &str) -> bool {
        self.remaining
            .strip_prefix(prefix)
            .map(|rest| self.remaining = rest)
            .is_some()
    }

    /// skip whitespace, then try to skip a prefix
    fn match_(&mut self, prefix: &str) -> bool {
        self.skip_whitespace();
        self.skip(prefix)
    }

    /// skip whitespace, then try to skip a prefix that is not followed by an id letter
    fn matchx(&mut self, prefix: &str) -> bool {
        self.skip_whitespace();
        let backup = self.remaining;
        if !self.skip(prefix) {
            return false;
        }
        if !self.peek().is_some_and(is_id_letter) {
            return true;
        }
        self.remaining = backup;
        false
    }

    fn peek_char(&mut self, c: char) -> bool {
        self.skip_whitespace();
        self.peek() == Some(c)
    }

    fn match_id(&mut self) -> Option<&'a str> {
        self.skip_whitespace();
        self.peek()
            .is_some_and(is_id_start)
            .then_some(self.parse_id())
    }

    // used for errors
    fn get_any_next_token(&mut self) -> &'a str {
        match self.peek() {
            None => "",
            Some(c) if is_id_start(c) => self.parse_id(),
            Some(c) if is_number(c) => self.parse_number_string(),
            Some(c) if is_punctuation(c) => self.parse_punctuation(),
            Some(c) => &self.remaining[..c.len_utf8()],
        }
    }

    fn peek_number(&self) -> bool {
        self.peek().is_some_and(is_number)
    }

    fn parse_number<'t>(&mut self, types: &mut Types<'t>) -> PResult<Ty<'t>> {
        let snum = self.parse_number_string();

        if snum.len() > 1 && snum.starts_with("0") {
            Err("Syntax error: integer values cannot start with '0'".to_string())
        } else {
            snum.parse().map(|i| types.get_int(i)).map_err(|_| {
                format!("{snum} could not be parsed to a positive signed 64 bit integer")
            })
        }
    }

    fn parse_number_string(&mut self) -> &'a str {
        let old = self.remaining;
        self.remaining = self.remaining.trim_start_matches(is_number);
        &old[..old.len() - self.remaining.len()]
    }

    fn parse_id(&mut self) -> &'a str {
        let old = self.remaining;
        self.remaining = self.remaining.trim_start_matches(is_id_letter);
        &old[..old.len() - self.remaining.len()]
    }

    fn parse_punctuation(&mut self) -> &'a str {
        let (p, i) = self.remaining.split_at(1);
        self.remaining = i;
        p
    }
}
