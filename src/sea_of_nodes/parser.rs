use crate::sea_of_nodes::graph_visualizer;
use crate::sea_of_nodes::nodes::{BoolOp, Node, NodeCreation, NodeId, Nodes, ScopeNode};
use crate::sea_of_nodes::types::{Ty, Types};

/// Converts a Simple source program to the Sea of Nodes intermediate representation in one pass.
pub struct Parser<'s, 't> {
    /// Tokenizes the source code
    lexer: Lexer<'s>,

    /// Manages interned types.
    types: &'t Types<'t>,

    /// Manages all nodes and implements operations such as peephole.
    /// Keeps track of the current start for constant node creation.
    pub nodes: Nodes<'t>,

    /// returned after parsing
    pub(crate) stop: NodeId,

    /// The current scope changes as we parse.
    pub(crate) scope: NodeId,

    /// stack of scopes for graph visualization
    pub(crate) x_scopes: Vec<NodeId>,

    continue_scope: Option<NodeId>,
    break_scope: Option<NodeId>,
    pub disable_show_graph_println: bool,
}

type ParseErr = String;
type PResult<T> = Result<T, ParseErr>;

pub fn is_keyword(s: &str) -> bool {
    matches!(
        s,
        "break" | "continue" | "else" | "false" | "if" | "int" | "return" | "true" | "while"
    )
}

impl<'s, 't> Parser<'s, 't> {
    pub fn new(source: &'s str, types: &'t Types<'t>) -> Self {
        Self::new_with_arg(source, types, types.ty_bot)
    }

    pub fn new_with_arg(source: &'s str, types: &'t Types<'t>, arg: Ty<'t>) -> Self {
        let mut nodes = Nodes::new(types);

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
            continue_scope: None,
            break_scope: None,
            disable_show_graph_println: false,
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
        self.nodes.create_peepholed(c)
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
            let p = self.nodes.peephole(self.stop);
            assert_eq!(self.stop, p);
            Ok(self.stop)
        }
    }

    pub fn iterate(&self, _stop: NodeId) -> NodeId {
        todo!("self.nodes.iterate(stop) should this really be here or on a ParseResult?")
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
        } else if self.matchx("while") {
            self.parse_while()
        } else if self.matchx("break") {
            self.parse_break()
        } else if self.matchx("continue") {
            self.parse_continue()
        } else if self.matchx("#showGraph") {
            self.show_graph();
            self.require(";")
        } else {
            self.parse_expression_statement()
        }
    }

    /// <pre>
    ///     while ( expression ) statement
    /// </pre>
    fn parse_while(&mut self) -> PResult<()> {
        let saved_continue_scope = self.continue_scope;
        let saved_break_scope = self.break_scope;

        self.require("(")?;

        // Loop region has two control inputs, the first is the entry
        // point, and second is back edge that is set after loop is parsed
        // (see end_loop() call below).  Note that the absence of back edge is
        // used as an indicator to switch off peepholes of the region and
        // associated phis; see {@code inProgress()}.

        let ctrl = self.ctrl();
        let ctrl = self.peephole(Node::make_loop(ctrl)); // Note we set back edge to null here
        self.set_ctrl(ctrl);

        // At loop head, we clone the current Scope (this includes all
        // names in every nesting level within the Scope).
        // We create phis eagerly for all the names we find, see dup().

        // Save the current scope as the loop head
        let head = self.scope;
        self.nodes.keep(head);

        // Clone the head Scope to create a new Scope for the body.
        // Create phis eagerly as part of cloning
        self.scope = self.nodes.scope_dup(self.scope, true); // The true argument triggers creating phis
        self.x_scopes.push(self.scope);

        // Parse predicate
        let pred = self.parse_expression()?;
        self.require(")")?;

        // IfNode takes current control and predicate
        let if_node = self.peephole(Node::make_if(ctrl, pred));

        // Setup projection nodes
        self.nodes.keep(if_node);
        let if_true = self.peephole(Node::make_proj(if_node, 0, "True".to_string()));
        self.nodes.unkeep(if_node);
        self.nodes.keep(if_true);
        let if_false = self.peephole(Node::make_proj(if_node, 1, "False".to_string()));

        // Clone the body Scope to create the break/exit Scope which accounts for any
        // side effects in the predicate.  The break/exit Scope will be the final
        // scope after the loop, and its control input is the False branch of
        // the loop predicate.  Note that body Scope is still our current scope.
        self.set_ctrl(if_false);
        let break_scope = self.nodes.scope_dup(self.scope, false);
        self.break_scope = Some(break_scope);
        self.x_scopes.push(break_scope);

        // No continues yet
        self.continue_scope = None;

        // Parse the true side, which corresponds to loop body
        // Our current scope is the body Scope
        self.nodes.unkeep(if_true);
        self.set_ctrl(if_true);
        self.parse_statement()?; // Parse loop body

        // Merge the loop bottom into other continue statements
        if self.continue_scope.is_some() {
            self.continue_scope = Some(self.jump_to(self.continue_scope));
            self.nodes.kill(self.scope);
            self.scope = self.continue_scope.unwrap();
        }

        // The true branch loops back, so whatever is current _scope.ctrl gets
        // added to head loop as input.  endLoop() updates the head scope, and
        // goes through all the phis that were created earlier.  For each phi,
        // it sets the second input to the corresponding input from the back
        // edge.  If the phi is redundant, it is replaced by its sole input.
        let exit = self.break_scope.unwrap();
        self.nodes.scope_end_loop(head, self.scope, exit);
        self.nodes.unkeep(head);
        self.nodes.kill(head);

        // Cleanup
        self.x_scopes.pop();
        self.x_scopes.pop();

        self.continue_scope = saved_continue_scope;
        self.break_scope = saved_break_scope;

        // At exit the false control is the current control, and
        // the scope is the exit scope after the exit test.
        self.scope = exit;
        *self.x_scopes.last_mut().unwrap() = exit; // differs from java

        Ok(())
    }

    fn jump_to(&mut self, to_scope: Option<NodeId>) -> NodeId {
        let cur = self.nodes.scope_dup(self.scope, false);

        let ctrl = self.peephole(Node::make_constant(self.nodes.start, self.types.ty_xctrl));
        self.set_ctrl(ctrl); // Kill current scope

        // Prune nested lexical scopes that have depth > than the loop head
        // We use _breakScope as a proxy for the loop head scope to obtain the depth
        let break_scopes_len = self.nodes.scope(self.break_scope.unwrap()).scopes.len();
        while self.nodes.scope(cur).scopes.len() > break_scopes_len {
            self.nodes.scope_pop(cur);
        }

        // If this is a continue then first time the target is null
        // So we just use the pruned current scope as the base for the
        // continue
        let Some(to_scope) = to_scope else {
            return cur;
        };

        // toScope is either the break scope, or a scope that was created here
        debug_assert!(self.nodes.scope(to_scope).scopes.len() <= break_scopes_len);
        let region = self.nodes.scope_merge(to_scope, cur);
        self.nodes.set_def(to_scope, 0, Some(region)); // set ctrl
        to_scope
    }

    fn check_loop_active(&self) -> PResult<()> {
        if self.break_scope.is_none() {
            Err("No active loop for a break or continue".to_string())
        } else {
            Ok(())
        }
    }

    fn parse_break(&mut self) -> PResult<()> {
        self.check_loop_active()?;
        self.break_scope = Some(self.jump_to(self.break_scope));
        self.require(";")
    }

    fn parse_continue(&mut self) -> PResult<()> {
        self.check_loop_active()?;
        self.continue_scope = Some(self.jump_to(self.continue_scope));
        self.require(";")
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

        // Setup projection nodes
        self.nodes.keep(if_node);
        let if_true = self.peephole(Node::make_proj(if_node, 0, "True".to_string()));
        self.nodes.unkeep(if_node);
        self.nodes.keep(if_true);
        let if_false = self.peephole(Node::make_proj(if_node, 1, "False".to_string()));
        self.nodes.keep(if_false);

        // In if true branch, the ifT proj node becomes the ctrl
        // But first clone the scope and set it as current
        let n_defs = self.nodes.inputs[self.scope].len();
        let mut false_scope = self.nodes.scope_dup(self.scope, false);
        self.x_scopes.push(false_scope);

        // Parse the true side
        self.nodes.unkeep(if_true);
        self.set_ctrl(if_true);
        self.parse_statement()?;
        let true_scope = self.scope;

        // Parse the false side
        self.scope = false_scope;
        self.nodes.unkeep(if_false);
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

        let region = self.nodes.scope_merge(true_scope, false_scope);
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
        let dot = graph_visualizer::generate_dot_output(self, false).unwrap();
        if !self.disable_show_graph_println {
            println!("{dot}");
            // uncomment to open in browser:
            // graph_visualizer::run_graphviz_and_chromium(dot);
        }
    }

    pub fn print(&mut self, node: NodeId) -> String {
        self.nodes.print(Some(node)).to_string()
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
        let mut lhs = self.parse_addition()?;
        loop {
            let mut negate = false;
            let (op, idx) = if self.match_("==") {
                (BoolOp::EQ, 2)
            } else if self.match_("!=") {
                negate = true;
                (BoolOp::EQ, 2)
            } else if self.match_("<=") {
                (BoolOp::LE, 2)
            } else if self.match_("<") {
                (BoolOp::LT, 2)
            } else if self.match_(">=") {
                (BoolOp::LE, 1)
            } else if self.match_(">") {
                (BoolOp::LT, 1)
            } else {
                return Ok(lhs);
            };
            // do it before parsing rhs to get same ids as java...
            lhs = self.nodes.create((
                Node::Bool(op),
                if idx == 2 {
                    vec![None, Some(lhs), None]
                } else {
                    vec![None, None, Some(lhs)]
                },
            ));
            let rhs = self.parse_addition()?;
            self.nodes.set_def(lhs, idx, Some(rhs));
            lhs = self.nodes.peephole(lhs);
            if negate {
                lhs = self.peephole(Node::make_not(lhs));
            }
        }
    }

    /// <pre>
    ///     additiveExpr : multiplicativeExpr (('+' | '-') multiplicativeExpr)*
    /// </pre>
    fn parse_addition(&mut self) -> PResult<NodeId> {
        let mut lhs = self.parse_multiplication()?;
        loop {
            let op = if self.match_("+") {
                Node::Add
            } else if self.match_("-") {
                Node::Sub
            } else {
                return Ok(lhs);
            };
            lhs = self.nodes.create((op, vec![None, Some(lhs), None]));
            let rhs = self.parse_multiplication()?;
            self.nodes.set_def(lhs, 2, Some(rhs));
            lhs = self.nodes.peephole(lhs);
        }
    }

    /// <pre>
    ///     multiplicativeExpr : unaryExpr (('*' | '/') unaryExpr)*
    /// </pre>
    fn parse_multiplication(&mut self) -> PResult<NodeId> {
        let mut lhs = self.parse_unary()?;
        loop {
            let op = if self.match_("*") {
                Node::Mul
            } else if self.match_("/") {
                Node::Div
            } else {
                return Ok(lhs);
            };
            lhs = self.nodes.create((op, vec![None, Some(lhs), None]));
            let rhs = self.parse_unary()?;
            self.nodes.set_def(lhs, 2, Some(rhs));
            lhs = self.nodes.peephole(lhs);
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

    fn parse_number<'t>(&mut self, types: &Types<'t>) -> PResult<Ty<'t>> {
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
