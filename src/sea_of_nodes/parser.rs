use crate::sea_of_nodes::graph_visualizer;
use crate::sea_of_nodes::nodes::index::{ScopeId, StopId};
use crate::sea_of_nodes::nodes::{BoolOp, Node, NodeCreation, NodeId, Nodes, ScopeNode};
use crate::sea_of_nodes::types::{Struct, Ty, Type, Types};
use std::collections::HashMap;

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
    pub(crate) stop: StopId,

    /// The current scope changes as we parse.
    pub(crate) scope: ScopeId,

    /// stack of scopes for graph visualization
    pub(crate) x_scopes: Vec<ScopeId>,

    continue_scope: Option<ScopeId>,
    break_scope: Option<ScopeId>,

    name_to_type: HashMap<&'t str, Ty<'t>>,

    pub disable_show_graph_println: bool,
}

type ParseErr = String;
type PResult<T> = Result<T, ParseErr>;

pub fn is_keyword(s: &str) -> bool {
    matches!(
        s,
        "break"
            | "continue"
            | "else"
            | "false"
            | "if"
            | "int"
            | "new"
            | "null"
            | "return"
            | "struct"
            | "true"
            | "while"
    )
}

impl<'s, 't> Parser<'s, 't> {
    pub fn new(source: &'s str, types: &'t Types<'t>) -> Self {
        Self::new_with_arg(source, types, types.ty_int_bot)
    }

    pub fn new_with_arg(source: &'s str, types: &'t Types<'t>, arg: Ty<'t>) -> Self {
        let mut nodes = Nodes::new(types);

        let scope = nodes.create(Node::make_scope());
        let scope = nodes.to_scope(scope).unwrap();
        nodes.ty[scope] = Some(types.ty_bot); // in java this is done by the constructor

        let args = types.get_tuple_from_array([types.ty_ctrl, arg]);
        let start = nodes.create(Node::make_start(args));
        nodes.ty[start] = Some(args);
        nodes.start = nodes.to_start(start).unwrap();

        let stop = nodes.create(Node::make_stop());
        let stop = nodes.to_stop(stop).unwrap();
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
            name_to_type: HashMap::new(),
            disable_show_graph_println: false,
        }
    }

    fn ctrl(&mut self) -> NodeId {
        self.nodes.inputs[self.scope][0].expect("has ctrl")
    }
    fn set_ctrl(&mut self, node: NodeId) {
        self.nodes.set_def(*self.scope, 0, Some(node));
    }

    fn peephole(&mut self, c: NodeCreation<'t>) -> NodeId {
        self.nodes.create_peepholed(c)
    }

    pub fn parse(&mut self) -> PResult<StopId> {
        self.x_scopes.push(self.scope);

        // Enter a new scope for the initial control and arguments
        self.scope.push(&mut self.nodes);

        // project ctrl and arg0 from start
        let start = self.nodes.start;
        let ctrl = self.peephole(Node::make_proj(start, 0, ScopeNode::CTRL));
        self.scope
            .define(ScopeNode::CTRL, self.types.ty_ctrl, ctrl, &mut self.nodes)
            .expect("not in scope");
        let arg0 = self.peephole(Node::make_proj(start, 1, ScopeNode::ARG0));
        self.scope
            .define(
                ScopeNode::ARG0,
                self.types.ty_int_bot,
                arg0,
                &mut self.nodes,
            )
            .expect("not in scope");

        self.parse_block()?;

        self.scope.pop(&mut self.nodes);
        self.x_scopes.pop();

        if !self.lexer.is_eof() {
            Err(format!(
                "Syntax error, unexpected {}",
                self.lexer.get_any_next_token()
            ))
        } else {
            let p = self.stop.peephole(&mut self.nodes);
            assert_eq!(*self.stop, p);
            Ok(self.stop)
        }
    }

    pub fn iterate(&mut self, stop: StopId) {
        self.nodes.iterate(stop)
    }

    /// <pre>
    ///     '{' statements '}'
    /// </pre>
    ///
    /// Does not parse the opening or closing `{}`
    fn parse_block(&mut self) -> PResult<()> {
        self.scope.push(&mut self.nodes);
        while !self.peek('}') && !self.lexer.is_eof() {
            self.parse_statement()?;
        }
        self.scope.pop(&mut self.nodes);
        Ok(())
    }

    /// <pre>
    ///     returnStatement | declStatement | blockStatement | ifStatement | expressionStatement
    /// </pre>
    fn parse_statement(&mut self) -> PResult<()> {
        if self.matchx("return") {
            self.parse_return()
        } else if self.matchx("int") {
            self.parse_decl(self.types.ty_int_bot)
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
        } else if self.matchx("struct") {
            self.parse_struct()
        } else if self.matchx("#showGraph") {
            self.show_graph();
            self.require(";")
        } else if self.matchx(";") {
            Ok(()) // Empty statement
        } else {
            // declarations of vars with struct type are handled in parseExpressionStatement due
            // to ambiguity
            self.parse_expression_statement()
        }
    }

    /// Parse a struct field.
    /// <pre>
    ///     int IDENTIFIER ;
    /// </pre>
    fn parse_field(&mut self) -> PResult<(&'t str, Ty<'t>)> {
        if self.matchx("int") {
            let name = self.types.get_str(self.require_id()?);
            self.require(";")?;
            return Ok((name, self.types.ty_int_bot));
        }
        Err("A field type is expected, only type 'int' is supported at present".to_string())
    }

    /// Parse a struct declaration, and return the following statement.
    /// Only allowed in top level scope.
    /// Structs cannot be redefined.
    ///
    /// @return The statement following the struct

    fn parse_struct(&mut self) -> PResult<()> {
        if self.x_scopes.len() > 1 {
            return Err("struct declarations can only appear in top level scope".to_string());
        }
        let type_name = self.types.get_str(self.require_id()?);
        if self.name_to_type.contains_key(type_name) {
            return Err(format!("struct '{type_name}' cannot be redefined"));
        }

        let mut fields: Vec<(&str, Ty)> = vec![];
        self.require("{")?;
        while !self.peek('}') && !self.lexer.is_eof() {
            let field = self.parse_field()?;
            if fields.iter().any(|&f| f.0 == field.0) {
                return Err(format!(
                    "Field '{}:{}' already defined in struct '{type_name}'",
                    field.0, field.1
                ));
            }
            fields.push(field);
        }
        self.require("}")?;

        // Build and install the TypeStruct
        let ts = self.types.get_struct(type_name, &fields);
        self.name_to_type.insert(type_name, ts); // Insert the struct name in the collection of all struct names
        self.nodes.add_mem_proj(self.nodes.start, ts, self.scope); // Insert memory edges
        self.parse_statement()
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
        self.nodes.keep(*head);

        // Clone the head Scope to create a new Scope for the body.
        // Create phis eagerly as part of cloning
        self.scope = self.scope.dup(true, &mut self.nodes); // The true argument triggers creating phis
        self.x_scopes.push(self.scope);

        // Parse predicate
        let pred = self.parse_expression()?;
        self.require(")")?;

        // IfNode takes current control and predicate
        let if_node = self.peephole(Node::make_if(ctrl, pred));

        // Setup projection nodes
        self.nodes.keep(if_node);
        let if_true = self.peephole(Node::make_proj(if_node, 0, "True"));
        self.nodes.unkeep(if_node);
        self.nodes.keep(if_true);
        let if_false = self.peephole(Node::make_proj(if_node, 1, "False"));

        // Clone the body Scope to create the break/exit Scope which accounts for any
        // side effects in the predicate.  The break/exit Scope will be the final
        // scope after the loop, and its control input is the False branch of
        // the loop predicate.  Note that body Scope is still our current scope.
        self.set_ctrl(if_false);
        let break_scope = self.scope.dup(false, &mut self.nodes);
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
            self.nodes.kill(*self.scope);
            self.scope = self.continue_scope.unwrap();
        }

        // The true branch loops back, so whatever is current _scope.ctrl gets
        // added to head loop as input.  endLoop() updates the head scope, and
        // goes through all the phis that were created earlier.  For each phi,
        // it sets the second input to the corresponding input from the back
        // edge.  If the phi is redundant, it is replaced by its sole input.
        let exit = self.break_scope.unwrap();
        head.end_loop(self.scope, exit, &mut self.nodes);
        self.nodes.unkeep(*head);
        self.nodes.kill(*head);

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

    fn jump_to(&mut self, to_scope: Option<ScopeId>) -> ScopeId {
        let cur = self.scope.dup(false, &mut self.nodes);

        let ctrl = self.peephole(Node::make_constant(self.nodes.start, self.types.ty_xctrl));
        self.set_ctrl(ctrl); // Kill current scope

        // Prune nested lexical scopes that have depth > than the loop head
        // We use _breakScope as a proxy for the loop head scope to obtain the depth
        let break_scopes_len = self.nodes[self.break_scope.unwrap()].scopes.len();
        while self.nodes[cur].scopes.len() > break_scopes_len {
            cur.pop(&mut self.nodes);
        }

        // If this is a continue then first time the target is null
        // So we just use the pruned current scope as the base for the
        // continue
        let Some(to_scope) = to_scope else {
            return cur;
        };

        // toScope is either the break scope, or a scope that was created here
        debug_assert!(self.nodes[to_scope].scopes.len() <= break_scopes_len);
        let region = to_scope.merge(cur, &mut self.nodes);
        self.nodes.set_def(*to_scope, 0, Some(region)); // set ctrl
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
        self.nodes.keep(pred);

        // IfNode takes current control and predicate
        let if_ctrl = self.ctrl();
        let if_node = self.peephole(Node::make_if(if_ctrl, pred));

        // Setup projection nodes
        self.nodes.keep(if_node);
        let if_true = self.peephole(Node::make_proj(if_node, 0, "True"));
        self.nodes.unkeep(if_node);
        self.nodes.keep(if_true);
        let if_false = self.peephole(Node::make_proj(if_node, 1, "False"));
        self.nodes.keep(if_false);

        // In if true branch, the ifT proj node becomes the ctrl
        // But first clone the scope and set it as current
        let n_defs = self.nodes.inputs[self.scope].len();
        let mut false_scope = self.scope.dup(false, &mut self.nodes);
        self.x_scopes.push(false_scope);

        // Parse the true side
        self.nodes.unkeep(if_true);
        self.set_ctrl(if_true);
        self.scope.upcast(if_true, pred, false, &mut self.nodes); // Up-cast predicate
        self.parse_statement()?;
        let true_scope = self.scope;

        // Parse the false side
        self.scope = false_scope;
        self.nodes.unkeep(if_false);
        self.set_ctrl(if_false);
        if self.matchx("else") {
            self.scope.upcast(if_false, pred, true, &mut self.nodes); // Up-cast predicate
            self.parse_statement()?;
            false_scope = self.scope;
        }
        self.nodes.unkeep(pred);

        if self.nodes.inputs[true_scope].len() != n_defs
            || self.nodes.inputs[false_scope].len() != n_defs
        {
            return Err("Cannot define a new name on one arm of an if".to_string());
        }

        // Merge results
        self.scope = true_scope;
        self.x_scopes.pop();

        let region = true_scope.merge(false_scope, &mut self.nodes);
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

        // We lookup memory slices by the naming convention that they start with $
        // We could also use implicit knowledge that all memory projects are at offset >= 2
        let names = self.scope.reverse_names(&self.nodes);
        for name in names.into_iter().map(Option::unwrap) {
            if name.starts_with("$") && name == "$ctrl" {
                let v = self.scope.lookup(name, &mut self.nodes).unwrap();
                self.nodes.add_def(ret, Some(v));
            }
        }

        self.nodes.add_def(*self.stop, Some(ret));
        let ctrl = self.peephole(Node::make_constant(self.nodes.start, self.types.ty_xctrl));
        self.set_ctrl(ctrl);
        Ok(())
    }

    /// Dumps out the node graph
    pub fn show_graph(&mut self) {
        let dot = graph_visualizer::generate_dot_output(
            &self.nodes,
            self.stop,
            Some(self.scope),
            &self.x_scopes,
            self.lexer.source,
            false,
        )
        .unwrap();
        if !self.disable_show_graph_println {
            println!("{dot}");
            // uncomment to open in browser:
            // graph_visualizer::run_graphviz_and_chromium(dot);
        }
    }

    pub fn print(&mut self, node: impl Into<NodeId>) -> String {
        self.nodes.print(Some(node.into())).to_string()
    }

    /// Parses an expression statement or a declaration statement where type is a struct
    ///
    /// <pre>
    ///      name;         // Error
    /// type name;         // Define name with default initial value
    /// type name = expr;  // Define name with given   initial value
    ///      name = expr;  // Reassign existing
    ///             expr   // Something else
    /// </pre>
    ////
    fn parse_expression_statement(&mut self) -> PResult<()> {
        let old = self.lexer.remaining;
        let t = self.parse_type();
        let name = self.require_id()?;

        let expr: NodeId;
        if self.match_(";") {
            // Assign a default value
            if let Some(t) = t {
                let init = self.types.make_init(t).unwrap();
                expr = self.peephole(Node::make_constant(self.nodes.start, init))
            } else {
                // No type and no expr is an error
                return Err("expression".to_string());
            }
        } else if self.match_("=") {
            // Assign "= expr;"
            expr = self.parse_expression()?;
            self.require(";")?;
        } else {
            // Neither, so just a normal expression parse
            self.lexer.remaining = old;
            self.parse_expression()?;
            return self.require(";");
        };

        // Defining a new variable vs updating an old one
        let name = self.types.get_str(name);
        let t = if let Some(t) = t {
            if self.scope.define(name, t, expr, &mut self.nodes).is_err() {
                return Err(format!("Redefining name '{name}'"));
            }
            t
        } else if let Some((_, t)) = self.nodes[self.scope].lookup(name).copied() {
            self.scope.update(name, expr, &mut self.nodes).unwrap();
            t
        } else {
            return Err(format!("Undefined name '{name}'"));
        };

        let expr_ty = self.nodes.ty[expr].unwrap();
        if !self.types.isa(expr_ty, t) {
            return Err(format!(
                "Type {} is not of declared type {}",
                expr_ty.str(),
                t.str()
            ));
        }
        Ok(())
    }

    fn parse_type(&mut self) -> Option<Ty<'t>> {
        let old = self.lexer.remaining;
        let tname = self.lexer.match_id()?;
        if tname == "int" {
            Some(self.types.ty_int_bot)
        } else if let Some(&obj) = self.name_to_type.get(tname) {
            let nil = self.match_("?");
            Some(self.types.get_pointer(obj, nil))
        } else {
            // Not a type; unwind the parse
            self.lexer.remaining = old;
            None
        }
    }
    /// <pre>
    ///     type name = expression ';'
    /// </pre>
    fn parse_decl(&mut self, t: Ty<'t>) -> PResult<()> {
        let name = self.require_id()?;
        let expr = if self.peek(';') {
            // Assign a null value
            let v = self.types.make_init(t).unwrap();
            self.peephole(Node::make_constant(self.nodes.start, v))
        } else {
            // Assign "= expr;"
            self.require("=")?;
            self.parse_expression()?
        };
        self.require(";")?;

        let expr_ty = self.nodes.ty[expr].unwrap();
        if !self.types.isa(expr_ty, t) {
            return Err(format!(
                "Type {} is not of declared type {}",
                expr_ty.str(),
                t.str()
            ));
        }

        let name = self.types.get_str(name);
        self.scope
            .define(name, t, expr, &mut self.nodes)
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
            lhs = lhs.peephole(&mut self.nodes);
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
            lhs = lhs.peephole(&mut self.nodes);
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
            lhs = lhs.peephole(&mut self.nodes);
        }
    }

    /// <pre>
    ///     unaryExpr : ('-') | '!') unaryExpr | postfixExpr | primaryExpr
    /// </pre>
    fn parse_unary(&mut self) -> PResult<NodeId> {
        if self.match_("-") {
            self.parse_unary()
                .map(|expr| self.peephole(Node::make_minus(expr)))
        } else if self.match_("!") {
            self.parse_unary()
                .map(|expr| self.peephole(Node::make_not(expr)))
        } else {
            let primary = self.parse_primary()?;
            self.parse_postfix(primary)
        }
    }

    /// <pre>
    ///     primaryExpr : integerLiteral | Identifier | true | false | null | new Identifier | '(' expression ')'
    /// </pre>
    fn parse_primary(&mut self) -> PResult<NodeId> {
        if self.lexer.peek_number() {
            self.parse_integer_literal()
        } else if self.match_("(") {
            let e = self.parse_expression()?;
            self.require(")")?;
            Ok(e)
        } else if self.matchx("true") {
            Ok(self.peephole(Node::make_constant(self.nodes.start, self.types.ty_int_one)))
        } else if self.matchx("false") {
            Ok(self.peephole(Node::make_constant(
                self.nodes.start,
                self.types.ty_int_zero,
            )))
        } else if self.matchx("null") {
            Ok(self.peephole(Node::make_constant(
                self.nodes.start,
                self.types.ty_pointer_null,
            )))
        } else if self.matchx("new") {
            let ty_name = self.require_id()?;
            let ty = self.name_to_type.get(ty_name);
            if let Some(ty) = ty {
                Ok(self.new_struct(*ty))
            } else {
                Err(format!("Unknown struct type '{ty_name}'"))
            }
        } else if let Some(name) = self.lexer.match_id() {
            self.scope
                .lookup(name, &mut self.nodes)
                .map_err(|()| format!("Undefined name '{name}'"))
        } else {
            Err(self.error_syntax("an identifier or expression"))
        }
    }

    /// Return a NewNode but also generate instructions to initialize it.
    fn new_struct(&mut self, obj: Ty<'t>) -> NodeId {
        let ptr_ty = self.types.get_pointer(obj, false);
        let ctrl = self.ctrl();
        let n = self.peephole(Node::make_new(ptr_ty, ctrl));
        self.nodes.keep(n);

        let init_value = self.peephole(Node::make_constant(
            self.nodes.start,
            self.types.ty_int_zero,
        ));

        let Type::Struct(Struct::Struct { name, fields }) = *obj else {
            unreachable!()
        };

        let mut alias = *self.nodes[self.nodes.start].alias_starts.get(name).unwrap();
        for &(fname, _) in fields {
            let mem_slice = self.mem_alias_lookup(alias).unwrap();
            let store = self.peephole(Node::make_store(fname, alias, [mem_slice, n, init_value]));
            self.mem_alias_update(alias, store).unwrap();
            alias += 1;
        }
        self.nodes.unkeep(n);
        n
    }

    /// We set up memory aliases by inserting special vars in the scope these
    /// variables are prefixed by $ so they cannot be referenced in Simple code.
    /// Using vars has the benefit that all the existing machinery of scoping
    /// and phis work as expected
    fn mem_alias_lookup(&mut self, alias: u32) -> Result<NodeId, ()> {
        let name = self.types.get_str(&Self::mem_name(alias));
        self.scope.lookup(name, &mut self.nodes)
    }

    fn mem_alias_update(&mut self, alias: u32, st: NodeId) -> Result<NodeId, ()> {
        let name = self.types.get_str(&Self::mem_name(alias));
        self.scope.update(name, st, &mut self.nodes)
    }

    pub(crate) fn mem_name(alias: u32) -> String {
        format!("${alias}")
    }

    /// Parse postfix expression. For now this is just a field
    /// expression, but in future could be array index too.
    ///
    /// <pre>
    ///     expr ('.' IDENTIFIER)* [ = expr ]
    /// </pre>
    fn parse_postfix(&mut self, expr: NodeId) -> PResult<NodeId> {
        if !self.match_(".") {
            return Ok(expr);
        }

        let expr_ty = self.nodes.ty[expr].unwrap();
        let Type::Pointer(ptr) = *expr_ty else {
            return Err(format!(
                "Expected struct reference but got {}",
                expr_ty.str()
            ));
        };
        let Type::Struct(Struct::Struct {
            name: sname,
            fields,
        }) = *ptr.to
        else {
            unreachable!()
        };

        let name = self.types.get_str(self.require_id()?);

        let Some(idx) = fields.iter().position(|&f| f.0 == name) else {
            return Err(format!(
                "Accessing unknown field '{name}' from '{}'",
                expr_ty.str()
            ));
        };

        let alias = self.nodes[self.nodes.start]
            .alias_starts
            .get(sname)
            .unwrap()
            + idx as u32;

        let old = self.lexer.remaining;
        if self.match_("=") {
            // Disambiguate "obj.fld==x" boolean test from "obj.fld=x" field assignment

            if self.peek('=') {
                self.lexer.remaining = old;
            } else {
                let val = self.parse_expression()?;
                let mem_slice = self.mem_alias_lookup(alias).unwrap();
                let store = self.peephole(Node::make_store(name, alias, [mem_slice, expr, val]));
                self.mem_alias_update(alias, store).unwrap();
                return Ok(expr); // "obj.a = expr" returns the expression while updating memory
            }
        }

        let declared_type = fields[idx].1;
        let mem_slice = self.mem_alias_lookup(alias).unwrap();
        let load = self.peephole(Node::make_load(
            name,
            alias,
            declared_type,
            [mem_slice, expr],
        ));
        self.parse_postfix(load)
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

    /// Match must be exact and not followed by more ID characters.
    /// Prevents identifier "ifxy" from matching an "if" statement.
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

    fn peek_is_id(&mut self) -> bool {
        self.skip_whitespace();
        self.peek().is_some_and(is_id_start)
    }

    fn match_id(&mut self) -> Option<&'a str> {
        self.peek_is_id().then(|| self.parse_id())
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
