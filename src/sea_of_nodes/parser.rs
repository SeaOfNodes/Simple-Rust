use crate::sea_of_nodes::graph_visualizer;
use crate::sea_of_nodes::nodes::node::{
    Add, And, CProj, Constant, If, Load, Loop, Minus, New, Not, Or, Proj, ReadOnly, Return,
    RoundF32, Sar, Scope, ScopeMin, Shl, Shr, Start, Stop, Store, Struct, ToFloat, XCtrl, Xor,
};
use crate::sea_of_nodes::nodes::{BoolOp, Node, Nodes, Op, Phi};
use crate::sea_of_nodes::types::{Field, Ty, TyMemPtr, TyStruct, Types};
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

    /// Next available memory alias number
    alias: u32,

    /// returned after parsing
    pub(crate) stop: Stop,

    /// The current scope changes as we parse.
    pub(crate) scope: Scope,

    /// stack of scopes for graph visualization
    pub(crate) x_scopes: Vec<Scope>,

    continue_scope: Option<Scope>,

    /// Merge all the while-breaks here
    break_scope: Option<Scope>,

    /// Mapping from a type name to a Type.  The string name matches
    /// `type.str()` call.  No TypeMemPtrs are in here, because Simple does not
    /// have C-style '*ptr' references.
    name_to_type: HashMap<&'t str, Ty<'t>>,

    /// Mapping from a type name to the constructor for a Type.
    inits: HashMap<&'t str, Struct>,

    pub disable_show_graph_println: bool,

    altmp: Vec<Option<Node>>,
}

type ParseErr = String;
type PResult<T> = Result<T, ParseErr>;

pub fn is_keyword(s: &str) -> bool {
    matches!(
        s,
        "bool"
            | "break"
            | "byte"
            | "continue"
            | "else"
            | "f32"
            | "f64"
            | "false"
            | "flt"
            | "i16"
            | "i32"
            | "i64"
            | "i8"
            | "if"
            | "int"
            | "new"
            | "null"
            | "return"
            | "struct"
            | "true"
            | "u1"
            | "u16"
            | "u32"
            | "u8"
            | "while"
    )
}

impl<'s, 't> Parser<'s, 't> {
    pub fn new(source: &'s str, types: &'t Types<'t>) -> Self {
        Self::new_with_arg(source, types, *types.int_bot)
    }

    pub fn new_with_arg(source: &'s str, types: &'t Types<'t>, arg: Ty<'t>) -> Self {
        let mut nodes = Nodes::new(types);

        let scope = Scope::new(&mut nodes);
        nodes.ty[scope] = Some(types.bot); // in java this is done by the constructor

        nodes.start = Start::new(&[types.ctrl, arg], &mut nodes);

        let stop = Stop::new(&mut nodes);

        nodes.zero = Constant::new(*types.int_zero, &mut nodes)
            .peephole(&mut nodes)
            .keep(&mut nodes)
            .to_constant(&nodes)
            .unwrap();
        nodes.xctrl = XCtrl::new(&mut nodes)
            .peephole(&mut nodes)
            .keep(&mut nodes)
            .to_xctrl(&nodes)
            .unwrap();

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
            alias: 2, // alias 0 for the control, 1 for memory
            name_to_type: Self::default_types(types),
            inits: HashMap::new(),
            altmp: vec![],
            disable_show_graph_println: false,
        }
    }

    fn default_types(types: &'t Types<'t>) -> HashMap<&'t str, Ty<'t>> {
        HashMap::from([
            ("bool", *types.int_u1),
            ("byte", *types.int_u8),
            ("f32", *types.float_b32),
            ("f64", *types.float_bot),
            ("flt", *types.float_bot),
            ("i16", *types.int_i16),
            ("i32", *types.int_i32),
            ("i64", *types.int_bot),
            ("i8", *types.int_i8),
            ("int", *types.int_bot),
            ("u1", *types.int_u1),
            ("u16", *types.int_u16),
            ("u32", *types.int_u32),
            ("u8", *types.int_u8),
            ("val", types.top), // Marker type, indicates type inference
            ("var", types.bot), // Marker type, indicates type inference
        ])
    }

    fn ctrl(&mut self) -> Node {
        self.nodes.inputs[self.scope][0].expect("has ctrl")
    }
    fn set_ctrl(&mut self, node: Node) {
        self.scope.set_def(0, node, &mut self.nodes);
    }

    pub fn parse(&mut self) -> PResult<Stop> {
        self.x_scopes.push(self.scope);

        // Enter a new scope for the initial control and arguments
        self.scope.push(false, &mut self.nodes);
        let mem = ScopeMin::new(&mut self.nodes);
        mem.add_def(None, &mut self.nodes);

        // project ctrl and arg0 from start
        let start = self.nodes.start;
        self.scope
            .define(
                Scope::CTRL,
                self.types.ctrl,
                false,
                CProj::new(start, 0, Scope::CTRL, &mut self.nodes).peephole(&mut self.nodes),
                &mut self.nodes,
            )
            .expect("not in scope");
        let mem0 = Proj::new(start, 1, Scope::MEM0, &mut self.nodes).peephole(&mut self.nodes);
        mem.add_def(mem0, &mut self.nodes);
        self.scope
            .define(
                Scope::MEM0,
                *self.types.mem_top,
                false,
                mem0.peephole(&mut self.nodes),
                &mut self.nodes,
            )
            .expect("not in scope");
        self.scope
            .define(
                Scope::ARG0,
                *self.types.int_bot,
                false,
                Proj::new(start, 2, Scope::ARG0, &mut self.nodes).peephole(&mut self.nodes),
                &mut self.nodes,
            )
            .expect("not in scope");

        // Parse whole program
        self.parse_block(false)?;

        if self.ctrl().ty(&self.nodes) == Some(self.types.ctrl) {
            let ret = Return::new(
                self.ctrl(),
                *self.nodes.zero,
                Some(self.scope),
                &mut self.nodes,
            )
            .peephole(&mut self.nodes);
            self.stop.add_def(ret, &mut self.nodes);
        }

        self.scope.kill(&mut self.nodes);
        self.x_scopes.pop();

        for init in self.inits.values() {
            init.unkeep(&mut self.nodes).kill(&mut self.nodes);
        }
        self.inits.clear();

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

    pub fn iterate(&mut self, stop: Stop) {
        self.nodes.iterate(stop)
    }

    pub fn type_check(&mut self, stop: Stop) -> Result<(), String> {
        self.nodes.type_check(stop)
    }

    /// <pre>
    ///     '{' statements '}'
    /// </pre>
    ///
    /// Does not parse the opening or closing `{}`
    fn parse_block(&mut self, inCon: bool) -> PResult<()> {
        self.scope.push(inCon, &mut self.nodes);
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
        } else if self.match_("{") {
            self.parse_block(false)?;
            self.require("}")
        } else if self.matchx("if") {
            self.parse_if()
        } else if self.matchx("while") {
            self.parse_while()
        } else if self.matchx("for") {
            self.parse_for()
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
            self.parse_declaration_statement() // Declaration or normal assignment/expression
        }
    }

    /// <pre>
    ///     while ( expression ) statement
    /// </pre>
    fn parse_while(&mut self) -> PResult<()> {
        self.require("(")?;
        self.parse_looping(false)
    }

    /// <pre>
    ///     for( var x=init; test; incr ) body
    /// </pre>
    fn parse_for(&mut self) -> PResult<()> {
        // {   var x=init,y=init,...;
        //     while( pred ) {
        //         body;
        //         next;
        //     }
        // }
        self.require("(")?;
        self.scope.push(false, &mut self.nodes); // Scope for the index variables
        if !self.match_(";") {
            // Can be empty init "for(;test;next) body"
            self.parse_declaration_statement(); // Non-empty init
        }
        let rez = self.parse_looping(true);
        self.scope.pop(&mut self.nodes); // Exit index variable scope
        rez
    }

    /// Shared by `for` and `while`
    fn parse_looping(&mut self, do_for: bool) -> PResult<()> {
        let saved_continue_scope = self.continue_scope;
        let saved_break_scope = self.break_scope;

        // Loop region has two control inputs, the first is the entry
        // point, and second is back edge that is set after loop is parsed
        // (see end_loop() call below).  Note that the absence of back edge is
        // used as an indicator to switch off peepholes of the region and
        // associated phis; see {@code inProgress()}.

        let ctrl = Loop::new(self.ctrl(), &mut self.nodes).peephole(&mut self.nodes); // Note we set back edge to null here
        self.set_ctrl(ctrl);

        // At loop head, we clone the current Scope (this includes all
        // names in every nesting level within the Scope).
        // We create phis eagerly for all the names we find, see dup().

        // Save the current scope as the loop head
        let head = self.scope;
        head.keep(&mut self.nodes);

        // Clone the head Scope to create a new Scope for the body.
        // Create phis eagerly as part of cloning
        self.scope = self.scope.dup(true, &mut self.nodes); // The true argument triggers creating phis
        self.x_scopes.push(self.scope);

        // Parse predicate
        let pred = if self.peek(';') {
            *self.int_con(1)
        } else {
            self.parse_asgn()?
        };
        self.require(if do_for { ";" } else { ")" })?;

        // IfNode takes current control and predicate
        let if_node = If::new(ctrl, Some(pred.keep(&mut self.nodes)), &mut self.nodes)
            .peephole(&mut self.nodes);

        // Setup projection nodes
        let if_true = CProj::new(if_node.keep(&mut self.nodes), 0, "True", &mut self.nodes)
            .peephole(&mut self.nodes)
            .keep(&mut self.nodes);
        let if_false = CProj::new(if_node.unkeep(&mut self.nodes), 1, "False", &mut self.nodes)
            .peephole(&mut self.nodes);

        // for( ;;next ) body
        let mut next_pos = "dummy_pos";
        let mut next_end = "dummy_pos";
        if do_for {
            // Skip the next expression and parse it later
            next_pos = self.pos();
            self.skip_asgn()?;
            next_end = self.pos();
            self.require(")")?;
        }

        // Clone the body Scope to create the break/exit Scope which accounts for any
        // side effects in the predicate.  The break/exit Scope will be the final
        // scope after the loop, and its control input is the False branch of
        // the loop predicate.  Note that body Scope is still our current scope.
        self.set_ctrl(if_false);
        let break_scope = self.scope.dup(false, &mut self.nodes);
        self.break_scope = Some(break_scope);
        self.x_scopes.push(break_scope);
        break_scope.add_guards(if_false, Some(pred), true, &mut self.nodes); // Up-cast predicate

        // No continues yet
        self.continue_scope = None;

        // Parse the true side, which corresponds to loop body
        // Our current scope is the body Scope
        if_true.unkeep(&mut self.nodes);
        self.set_ctrl(if_true);
        self.scope.add_guards(
            if_true,
            Some(pred.unkeep(&mut self.nodes)),
            false,
            &mut self.nodes,
        ); // Up-cast predicate
        self.parse_statement()?; // Parse loop body
        self.scope.remove_guards(if_true, &mut self.nodes);

        // Merge the loop bottom into other continue statements
        if self.continue_scope.is_some() {
            self.continue_scope = Some(self.jump_to(self.continue_scope));
            self.scope.kill(&mut self.nodes);
            self.scope = self.continue_scope.unwrap();
        }

        // Now append the next code onto the body code
        if do_for {
            let old = self.pos();
            self.set_pos(next_pos);
            if !self.peek(')') {
                self.parse_asgn()?;
            }
            if self.pos() != next_end {
                return Err(self.error_syntax("Unexpected code after expression"));
            }
            self.set_pos(old);
        }

        // The true branch loops back, so whatever is current _scope.ctrl gets
        // added to head loop as input.  endLoop() updates the head scope, and
        // goes through all the phis that were created earlier.  For each phi,
        // it sets the second input to the corresponding input from the back
        // edge.  If the phi is redundant, it is replaced by its sole input.
        let exit = self.break_scope.unwrap();
        head.end_loop(self.scope, exit, &mut self.nodes);
        head.unkeep(&mut self.nodes);
        head.kill(&mut self.nodes);

        // Cleanup
        self.x_scopes.pop();
        self.x_scopes.pop();

        self.continue_scope = saved_continue_scope;
        self.break_scope = saved_break_scope;

        // At exit the false control is the current control, and
        // the scope is the exit scope after the exit test.
        *self.x_scopes.last_mut().unwrap() = exit;
        self.scope = exit;
        Ok(())
    }

    fn jump_to(&mut self, to_scope: Option<Scope>) -> Scope {
        let cur = self.scope.dup(false, &mut self.nodes);
        self.set_ctrl(**self.nodes.xctrl); // Kill current scope

        // Prune nested lexical scopes that have depth > than the loop head
        // We use _breakScope as a proxy for the loop head scope to obtain the depth
        let break_scopes_len = self.nodes[self.break_scope.unwrap()].lex_size.len();
        while self.nodes[cur].lex_size.len() > break_scopes_len {
            cur.pop(&mut self.nodes);
        }

        // If this is a continue then first time the target is null
        // So we just use the pruned current scope as the base for the
        // continue
        let Some(to_scope) = to_scope else {
            return cur;
        };

        // toScope is either the break scope, or a scope that was created here
        debug_assert!(self.nodes[to_scope].lex_size.len() <= break_scopes_len);
        let region = to_scope
            .merge_scopes(cur, &mut self.nodes)
            .peephole(&mut self.nodes);
        to_scope.set_def(0, region, &mut self.nodes); // set ctrl
        to_scope
    }

    fn check_loop_active(&self) -> PResult<()> {
        if self.break_scope.is_none() {
            Err("No active loop for a break or continue".to_string())
        } else {
            Ok(())
        }
    }

    fn parse_continue(&mut self) -> PResult<()> {
        self.check_loop_active()?;
        self.continue_scope = Some(self.jump_to(self.continue_scope));
        self.require(";")
    }

    fn parse_break(&mut self) -> PResult<()> {
        self.check_loop_active()?;
        // At the time of the break, and loop-exit conditions are only valid if
        // they are ALSO valid at the break.  It is the intersection of
        // conditions here, not the union.
        let break_scope = self.break_scope.unwrap();
        break_scope.remove_guards(break_scope.ctrl(&mut self.nodes).unwrap(), &mut self.nodes);
        self.break_scope = Some(self.jump_to(self.break_scope));
        self.require(";")?;
        self.break_scope.unwrap().add_guards(
            self.break_scope.unwrap().ctrl(&mut self.nodes).unwrap(),
            None,
            false,
            &mut self.nodes,
        );
        Ok(())
    }

    /// Look for an unbalanced `)`, skipping balanced
    fn skip_asgn(&mut self) -> PResult<()> {
        let mut paren = 0;
        loop {
            // Next X char handles skipping complex comments
            let pos = self.lexer.remaining;
            match self.lexer.next_x_char() {
                None => return Err("TODO: not yet implemented".to_string()),
                Some(')') => {
                    paren -= 1;
                    if paren < 0 {
                        self.set_pos(pos); // Leave the `)` behind
                        return Ok(());
                    }
                }
                Some('(') => paren += 1,
                _ => {}
            }
        }
    }

    /// <pre>
    ///     if ( expression ) statement [else statement]
    /// </pre>
    fn parse_if(&mut self) -> PResult<()> {
        // Parse predicate
        self.require("(")?;
        let pred = self.parse_asgn()?;
        self.require(")")?;
        self.parse_trinary(pred, true, "else")?;
        Ok(())
    }

    /// Parse a conditional expression, merging results.
    fn parse_trinary(&mut self, pred: Node, stmt: bool, fside: &str) -> PResult<Node> {
        pred.keep(&mut self.nodes);

        // IfNode takes current control and predicate
        let if_node = If::new(self.ctrl(), Some(pred), &mut self.nodes).peephole(&mut self.nodes);

        // Setup projection nodes
        let if_true = CProj::new(if_node.keep(&mut self.nodes), 0, "True", &mut self.nodes)
            .peephole(&mut self.nodes)
            .keep(&mut self.nodes);
        let if_false = CProj::new(if_node.unkeep(&mut self.nodes), 1, "False", &mut self.nodes)
            .peephole(&mut self.nodes)
            .keep(&mut self.nodes);

        // In if true branch, the ifT proj node becomes the ctrl
        // But first clone the scope and set it as current
        let n_defs = self.nodes.inputs[self.scope].len();
        let mut false_scope = self.scope.dup(false, &mut self.nodes);
        self.x_scopes.push(false_scope);

        // Parse the true side
        self.set_ctrl(if_true.unkeep(&mut self.nodes));
        self.scope
            .add_guards(if_true, Some(pred), false, &mut self.nodes); // Up-cast predicate
        let mut lhs = if stmt {
            self.parse_statement()?;
            None
        } else {
            Some(self.parse_asgn()?.keep(&mut self.nodes))
        };
        self.scope.remove_guards(if_true, &mut self.nodes);

        let true_scope = self.scope;

        // Parse the false side
        self.scope = false_scope; // Restore scope, then parse else block if any
        self.set_ctrl(if_false.unkeep(&mut self.nodes)); // Ctrl token is now set to ifFalse projection

        // Up-cast predicate, even if not else clause, because predicate can
        // remain true if the true clause exits: `if( !ptr ) return 0; return ptr.fld;`
        self.scope
            .add_guards(if_false, Some(pred), true, &mut self.nodes);
        let do_rhs = self.match_(fside);
        let mut rhs = match (do_rhs, stmt) {
            (true, true) => {
                self.parse_statement()?;
                None
            }
            (true, false) => Some(self.parse_asgn()?),
            (false, true) => None,
            (false, false) => Some(self.con(lhs.unwrap().ty(&self.nodes).make_zero(&self.types))),
        };
        self.scope.remove_guards(if_false, &mut self.nodes);
        if do_rhs {
            false_scope = self.scope;
        }

        // Check for `if(pred) int x=17;`
        if self.nodes.inputs[true_scope].len() != n_defs
            || self.nodes.inputs[false_scope].len() != n_defs
        {
            return Err("Cannot define a new name on one arm of an if".to_string());
        }

        // Check the trinary widening int/flt
        if !stmt {
            rhs = Some(
                self.widen_int(rhs, lhs.unwrap().ty(&self.nodes).unwrap())
                    .keep(&mut self.nodes),
            );
            lhs = Some(
                self.widen_int(
                    lhs.unwrap().unkeep(&mut self.nodes),
                    rhs.unwrap().ty(&self.nodes),
                )
                .keep(&mut self.nodes),
            );
            rhs.unwrap().unkeep(&mut self.nodes);
        }

        // Merge results
        self.scope = true_scope;
        self.x_scopes.pop();

        let r = true_scope.merge_scopes(false_scope, &mut self.nodes);
        self.set_ctrl(**r);
        let ret = if stmt {
            r.to_node()
        } else {
            self.peep(Phi::new(
                "",
                self.types.meet(
                    lhs.unwrap().ty(&self.nodes).unwrap(),
                    rhs.unwrap().ty(&self.nodes).unwrap(),
                ),
                vec![Some(**r), Some(lhs.unwrap().unkeep(&mut self.nodes)), rhs],
                &mut self.nodes,
            ))
        };
        r.peephole(&mut self.nodes);
        Ok(ret)
    }

    /// <pre>
    ///     'return' expr ;
    /// </pre>
    ///
    /// Parses a return statement; "return" already parsed.
    /// The $ctrl edge is killed.
    fn parse_return(&mut self) -> PResult<()> {
        let expr = self.parse_asgn()?;
        self.require(";")?;
        let ctrl = self.ctrl();
        let ret =
            Return::new(ctrl, expr, Some(self.scope), &mut self.nodes).peephole(&mut self.nodes);

        self.stop.add_def(ret, &mut self.nodes);
        self.set_ctrl(**self.nodes.xctrl);
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

    pub fn print(&mut self, node: impl Into<Node>) -> String {
        node.into().print(&self.nodes).to_string()
    }

    /// <pre>
    ///  [name '='] expr
    /// </pre>
    fn parse_asgn(&mut self) -> PResult<Node> {
        let old = self.pos();
        let name = self.lexer.match_id();
        // Just a plain expression, no assignment.
        // Distinguish `var==expr` from `var=expr`

        let Some(name) = name else {
            self.set_pos(old);
            return self.parse_expression();
        };
        if is_keyword(name) || !self.match_opx(*b"==") {
            self.set_pos(old);
            return self.parse_expression();
        }

        // Parse assignment expression
        let expr = self.parse_asgn()?;

        // Final variable to update
        let Ok(def) = self.scope.lookup(name, &mut self.nodes) else {
            return Err(format!("Undefined name '{name}'"));
        };

        // TOP fields are for late-initialized fields; these have never
        // been written to, and this must be the final write.  Other writes
        // outside the constructor need to check the final bit.
        if self.scope.inputs(&self.nodes)[def].unwrap().ty(&self.nodes) != Some(self.types.top)
            && self.nodes[self.scope].vars[def].final_field
            && !self.scope.in_con(&self.nodes)
        {
            return Err(format!("Cannot reassign final '{name}'"));
        }

        // Lift expression, based on type
        let lift = self.lift_expr(
            expr,
            self.nodes[self.scope].vars[def].ty(),
            self.nodes[self.scope].vars[def].final_field,
        )?;
        // Update
        self.scope.update(name, lift, &mut self.nodes);
        // Return un-lifted expr
        Ok(expr)
    }

    /// Make finals deep; widen ints to floats; narrow wide int types.
    /// Early error if types do not match variable.
    fn lift_expr(&mut self, mut expr: Node, t: Ty<'t>, xfinal: bool) -> PResult<Node> {
        assert!(!expr
            .ty(&self.nodes)
            .is_some_and(|t| t.to_mem_ptr().is_none_or(|t| !t.is_fref())));
        // Final is deep on ptrs
        if xfinal {
            if let Some(tmp) = t.to_mem_ptr() {
                t = tmp.make_ro();
                expr = self.peep(ReadOnly::new(expr, &mut self.nodes));
            }
        }
        // Auto-widen int to float
        expr = self.widen_int(expr, t);

        // Auto-narrow wide ints to narrow ints
        expr = self.zs_mask(expr, t)?;

        // Type is sane
        if !expr.ty(&self.nodes).isa(t) {
            return Err(format!(
                "Type {} is not of declared type {}",
                expr.ty(&self.nodes).unwrap().str(),
                t.str()
            ));
        }

        Ok(expr)
    }

    fn widen_int(&mut self, expr: Node, t: Ty<'t>) -> Node {
        if t.is_float() && expr.ty(&self.nodes).is_some_and(|i| i.is_int()) {
            self.peep(ToFloat::new(expr, &mut self.nodes))
        } else {
            expr
        }
    }

    /// <pre>
    ///     declStmt = type var['=' exprAsgn][, var['=' exprAsgn]]* ';' | exprAsgn ';'
    ///     exprAsgn = var '=' exprAsgn | expr
    /// </pre>
    fn parse_declaration_statement(&mut self) -> PResult<()> {
        let Some(t) = self.type_()? else {
            self.parse_asgn()?;
            self.require(";")?;
            return Ok(());
        };

        // now parse var['=' asgnexpr] in a loop
        self.parse_declaration(t)?;
        while self.match_(";") {
            self.parse_declaration(t)?;
        }
        self.require(";")
    }

    /// <pre>
    ///     [!]var['=' asgn]
    /// </pre>
    fn parse_declaration(&mut self, mut t: Ty<'t>) -> PResult<()> {
        // Has var/val instead of a user-declared type
        let infer_type = t == self.types.top || t == self.types.bot;
        let has_bang = self.match_("!");
        let name = self.require_id()?;

        // Optional initializing expression follows
        let mut xfinal = false;
        let expr;
        if self.match_("=") {
            expr = self.parse_asgn()?;
            // `val` is always final
            xfinal = t == self.types.top ||
                // var is always not-final, final if no Bang AND TMP since primitives are not-final by default
                (t != self.types.bot && !has_bang && t.is_mem_ptr());
            // var/val, then type comes from expression
            if infer_type {
                t = expr.ty(&self.nodes).glb();
            }
        } else {
            if infer_type && !self.scope.in_con(&self.nodes) {
                return Err(self.error_syntax("=expression"));
            }
            expr = *self.con(t.make_init());
        }

        // Lift expression, based on type
        let lift = self.lift_expr(expr, t, xfinal)?;
        if xfinal {
            if let Some(tmp) = t.to_mem_ptr() {
                t = tmp.make_ro();
            }
        }

        // Define a new name,
        self.scope
            .define(name, t, xfinal, lift, &mut self.nodes)
            .map_err(|()| format!("Redefining name '{name}'"))
    }

    /// Parse a struct declaration, and return the following statement.
    /// Only allowed in top level scope.
    /// Structs cannot be redefined.
    fn parse_struct(&mut self) -> PResult<()> {
        if self.x_scopes.len() > 1 {
            return Err(self.error_syntax("struct declarations can only appear in top level scope"));
        }
        let type_name = self.require_id()?;
        let t = self.name_to_type.get(type_name);
        if t.is_some_and(|t| !t.to_mem_ptr().is_some_and(|tmp| tmp.is_fref())) {
            return Err(self.error_syntax(&format!("struct '{type_name}' cannot be redefined")));
        }

        // A Block scope parse, and inspect the scope afterward for fields.
        self.scope.push(true, &mut self.nodes);
        self.x_scopes.push(self.scope);
        self.require("{")?;
        while !self.peek('}') && !self.lexer.is_eof() {
            self.parse_statement()?;
        }

        // Grab the declarations and build fields and a Struct
        let lexlen = *self.nodes[self.scope].lex_size.last().unwrap();
        let varlen = self.nodes[self.scope].vars.len();
        let s = Struct::new(&mut self.nodes);
        let mut fs = Vec::with_capacity(varlen - lexlen);
        for i in lexlen..varlen {
            s.add_def(self.scope.inputs(&self.nodes)[i], &mut self.nodes);
            let v = &mut self.nodes[self.scope].vars[i];
            fs.push(Field {
                fname: v.name,
                ty: v.ty(&self.name_to_type, self.types),
                alias: self.alias,
                final_field: v.final_field,
            });
            self.alias += 1;
        }
        let ts = self.types.get_struct(type_name, fs);
        self.name_to_type
            .insert(type_name, *self.types.get_mem_ptr(ts, false));
        self.inits.insert(
            type_name,
            s.peephole(&mut self.nodes)
                .keep(&mut self.nodes)
                .to_struct(&mut self.nodes)
                .unwrap(),
        );

        // Done with struct/block scope
        self.require("}")?;
        self.require(";")?;
        self.x_scopes.pop();
        self.scope.pop(&mut self.nodes);
        Ok(())
    }

    /// Parse and return a type or null.  Valid types always are followed by an
    /// 'id' which the caller must parse.  This lets us distinguish forward ref
    /// types (which ARE valid here) from local vars in an (optional) forward
    /// ref type position.
    /// <pre>
    ///     t = int|i8|i16|i32|i64|u8|u16|u32|u64|byte|bool | flt|f32|f64 | val | var | struct[?]
    /// </pre>
    fn type_(&mut self) -> PResult<Option<Ty<'t>>> {
        let old1 = self.pos();
        let Some(tname) = self.lexer.match_id() else {
            return Ok(None);
        };
        // Convert the type name to a type.
        let t0 = self.name_to_type.get(tname).copied();
        // No new types as keywords
        if t0.is_none() && is_keyword(tname) {
            self.set_pos(old1);
            return Ok(None);
        }
        let t1 = t0.unwrap_or_else(|| {
            // Null: assume a forward ref type
            *self
                .types
                .get_mem_ptr(self.types.make_struct_fref(tname), false)
        });
        // Nest arrays and '?' as needed
        let mut t2 = t1;
        loop {
            debug_assert!(!t2.is_struct());
            if self.match_("?") {
                if let Some(tmp) = t2.to_mem_ptr() {
                    if tmp.data().nil {
                        return Err(format!("Type {t2} already allows null"));
                    }
                    t2 = *self.types.get_mem_ptr(tmp.data().to, true);
                } else {
                    return Err(format!("Type {} cannot be null", t0.unwrap()));
                }
            } else if self.match_("[]") {
                t2 = *self.type_ary(t2)?;
            } else {
                break;
            }
        }

        // Check no forward ref
        if t0.is_some() {
            return Ok(Some(t2));
        }

        // Check valid forward ref, after parsing all the type extra bits.
        // Cannot check earlier, because cannot find required 'id' until after "[]?" syntax
        let old2 = self.pos();
        let id = self.lexer.match_id();
        self.set_pos(old2); // Reset lexer to reparse
        if id.is_none() || self.scope.lookup(id.unwrap(), &mut self.nodes).is_ok() {
            self.set_pos(old1); // Reset lexer to reparse
            return Ok(None);
        }
        // Yes a forward ref, so declare it
        self.name_to_type.insert(tname, t1);
        Ok(Some(t2))
    }

    /// Make an array type of t
    fn type_ary(&mut self, t: Ty<'t>) -> PResult<TyMemPtr<'t>> {
        if t.to_mem_ptr().is_some_and(|t| !t.data().nil) {
            return Err("Arrays of reference types must always be nullable".to_string());
        };
        let tname = self.types.get_str(&format!("[{}]", t.str()));
        if let Some(ta) = self.name_to_type.get(tname) {
            return Ok(ta.to_mem_ptr().unwrap());
        }
        // Need make an array type.
        let ts = self
            .types
            .make_ary(self.types.int_bot, self.alias, t, self.alias + 1);
        self.alias += 2;
        debug_assert_eq!(ts.str(), tname);
        let tary = self.types.get_mem_ptr(ts, false);
        self.name_to_type.insert(tname, *tary);
        Ok(tary)
    }

    /// Fixup forward refs lazily.  Basically a Union-Find flavored read
    /// barrier.
    fn lazy_f_ref(&mut self, t: Ty<'t>) -> PResult<Ty<'t>> {
        //if( !t.isFRef() ) return t;
        //Type def = Parser.TYPES.get(((TypeMemPtr)t)._obj._name);
        Err("Not yet implemented".to_string())
    }

    /// <pre>
    ///     expr : bitwise [? expr [: expr]]
    /// </pre>
    fn parse_expression(&mut self) -> PResult<Node> {
        let expr = self.parse_bitwise()?;
        if self.match_("?") {
            self.parse_trinary(expr, false, ":")
        } else {
            Ok(expr)
        }
    }

    /// <pre>
    ///     bitwise : compareExpr (('&' | '|' | '^') compareExpr)*
    /// </pre>
    fn parse_bitwise(&mut self) -> PResult<Node> {
        let mut lhs = self.parse_comparison()?;
        loop {
            lhs = if self.match_("&") {
                And::new(lhs, None, &mut self.nodes).to_node()
            } else if self.match_("|") {
                Or::new(lhs, None, &mut self.nodes).to_node()
            } else if self.match_("^") {
                Xor::new(lhs, None, &mut self.nodes).to_node()
            } else {
                return Ok(lhs);
            };
            let rhs = self.parse_comparison()?;
            lhs.set_def(2, rhs, &mut self.nodes);
            lhs = self.peep(lhs);
        }
    }

    /// <pre>
    ///     expr : additiveExpr op additiveExpr
    /// </pre>
    fn parse_comparison(&mut self) -> PResult<Node> {
        let mut lhs = self.parse_shift()?;
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
                Op::Bool(op),
                if idx == 2 {
                    vec![None, Some(lhs), None]
                } else {
                    vec![None, None, Some(lhs)]
                },
            ));
            lhs.set_def(idx, self.parse_shift()?, &mut self.nodes);
            lhs = lhs.widen(&mut self.nodes);
            lhs = self.peep(lhs);
            if negate {
                lhs = *Not::new(lhs, &mut self.nodes);
                lhs = self.peep(lhs);
            }
        }
    }

    /// <pre>
    ///     shiftExpr : additiveExpr (('<<' | '>>' | '>>>') additiveExpr)*
    /// </pre>
    fn parse_shift(&mut self) -> PResult<Node> {
        let mut lhs = self.parse_addition()?;
        loop {
            if self.match_("<<") {
                lhs = Shl::new(lhs, None, &mut self.nodes).to_node();
            } else if self.match_(">>>") {
                lhs = Shr::new(lhs, None, &mut self.nodes).to_node();
            } else if self.match_(">>") {
                lhs = Sar::new(lhs, None, &mut self.nodes).to_node();
            } else {
                return Ok(lhs);
            }
            lhs.set_def(2, self.parse_addition()?, &mut self.nodes);
            lhs = lhs.widen(&mut self.nodes);
            lhs = self.peep(lhs);
        }
    }

    /// <pre>
    ///     additiveExpr : multiplicativeExpr (('+' | '-') multiplicativeExpr)*
    /// </pre>
    fn parse_addition(&mut self) -> PResult<Node> {
        let mut lhs = self.parse_multiplication()?;
        loop {
            let op = if self.match_("+") {
                Op::Add
            } else if self.match_("-") {
                Op::Sub
            } else {
                return Ok(lhs);
            };
            lhs = self.nodes.create((op, vec![None, Some(lhs), None]));
            let rhs = self.parse_multiplication()?;
            lhs.set_def(2, rhs, &mut self.nodes);
            lhs = lhs.widen(&mut self.nodes);
            lhs = self.peep(lhs);
        }
    }

    /// <pre>
    ///     multiplicativeExpr : unaryExpr (('*' | '/') unaryExpr)*
    /// </pre>
    fn parse_multiplication(&mut self) -> PResult<Node> {
        let mut lhs = self.parse_unary()?;
        loop {
            let op = if self.match_("*") {
                Op::Mul
            } else if self.match_("/") {
                Op::Div
            } else {
                return Ok(lhs);
            };
            lhs = self.nodes.create((op, vec![None, Some(lhs), None]));
            let rhs = self.parse_unary()?;
            lhs.set_def(2, rhs, &mut self.nodes);
            lhs = lhs.widen(&mut self.nodes);
            lhs = self.peep(lhs);
        }
    }

    /// <pre>
    ///     unaryExpr : ('-') unaryExpr | '!') unaryExpr | postfixExpr | primaryExpr | '--' Id | '++' Id
    /// </pre>
    fn parse_unary(&mut self) -> PResult<Node> {
        // Pre-dec/pre-inc
        let old = self.pos();
        let dec = self.match_("--");
        if dec || self.match_("++") {
            let delta = if dec { -1 } else { 1 }; // Pre vs post
            if let Some(name) = self.lexer.match_id() {
                if let Ok(n) = self.scope.lookup(name, &mut self.nodes) {
                    let v = &mut self.nodes[self.scope].vars[n];
                    if v.final_field {
                        return Err(format!("Cannot reassign final '{}'", v.name));
                    }
                    let t = v.ty(&self.name_to_type, self.types);
                    let add = Add::new(
                        self.scope.inputs(&self.nodes)[n].unwrap(),
                        self.int_con(delta).to_node(),
                        &mut self.nodes,
                    );
                    let add = self.peep(add);
                    let expr = self.zs_mask(add, t)?;
                    self.scope.update_var_index(n, Some(expr), &mut self.nodes);
                    return Ok(expr);
                }
            }
            // Reset, try again
            self.set_pos(old);
        }
        if self.match_("-") {
            self.parse_unary()
                .map(|expr| Minus::new(expr, &mut self.nodes).widen(&mut self.nodes))
                .map(|expr| self.peep(expr))
        } else if self.match_("!") {
            self.parse_unary()
                .map(|expr| Not::new(expr, &mut self.nodes))
                .map(|expr| self.peep(expr))
        } else {
            let primary = self.parse_primary()?;
            self.parse_postfix(primary)
        }
    }

    /// <pre>
    ///     primaryExpr : integerLiteral | true | false | null | new Type | '(' expression ')' | Id['++','--']
    /// </pre>
    fn parse_primary(&mut self) -> PResult<Node> {
        if self.lexer.peek().is_some_and(is_number) {
            self.parse_literal().map(|n| *n)
        } else if self.matchx("true") {
            Ok(*self.int_con(1))
        } else if self.matchx("false") {
            Ok(*self.nodes.zero)
        } else if self.matchx("null") {
            Ok(*self.con(*self.types.ptr_null))
        } else if self.match_("(") {
            let e = self.parse_asgn()?;
            self.require(")")?;
            Ok(e)
        } else if self.matchx("new") {
            self.alloc()
        } else {
            // Expect an identifier now
            let n = self.require_lookup_id("an identifier or expression")?;
            let n_name = self.nodes[self.scope].vars[n].name;

            let rvalue = self.scope.inputs(&self.nodes)[n].unwrap();
            if rvalue.ty(&self.nodes) == Some(self.types.bot) {
                return Err(format!("Cannot read uninitialized field '{n_name}'"));
            }
            // Check for assign-update, x += e0;
            let Some(ch) = self.lexer.match_oper_assign() else {
                return Ok(rvalue);
            };
            if self.nodes[self.scope].vars[n].final_field {
                return Err(format!("Cannot reassign final '{n_name}'"));
            }
            let op = match ch {
                '+' => self.nodes.create((Op::Add, vec![None, Some(rvalue), None])),
                '-' => self.nodes.create((Op::Sub, vec![None, Some(rvalue), None])),
                '*' => self.nodes.create((Op::Mul, vec![None, Some(rvalue), None])),
                '/' => self.nodes.create((Op::Div, vec![None, Some(rvalue), None])),
                '\u{1}' => self
                    .nodes
                    .create((Op::Add, vec![None, Some(rvalue), Some(*self.int_con(1))])),
                '\u{FFFF}' => self
                    .nodes
                    .create((Op::Add, vec![None, Some(rvalue), Some(*self.int_con(-1))])),
                _ => return Err("Not yet implemented".to_string()),
            };
            // Return pre-value (x+=1) or post-value (x++)
            let pre = "+-*/".contains(ch);
            // Parse RHS argument as needed
            if pre {
                op.keep(&mut self.nodes)
                    .set_def(2, self.parse_asgn()?, &mut self.nodes);
                op.unkeep(&mut self.nodes);
            } else {
                rvalue.keep(&mut self.nodes); // Keep post-value across peeps
            }
            let n_ty = self.nodes[self.scope].vars[n].ty(&self.name_to_type, self.types);
            let op = self.zs_mask(self.peep(op), n_ty)?;
            self.scope.update(n, op, &mut self.nodes);
            Ok(if pre {
                op
            } else {
                rvalue.unkeep(&mut self.nodes)
            })
        }
    }

    fn require_lookup_id(&mut self, msg: &str) -> PResult<usize> {
        let Some(id) = self.lexer.match_id().filter(|s| !is_keyword(s)) else {
            return Err(self.error_syntax(msg));
        };
        self.scope
            .lookup(id, &mut self.nodes)
            .map_err(|()| format!("Undefined name '{id}'"))
    }

    fn alloc(&mut self) -> PResult<Node> {
        let Some(t) = self.type_()? else {
            return Err("Expected a type".to_string());
        };
        // Parse ary[ length_expr ]
        if self.match_("[") {
            let len = self.parse_asgn()?;
            if !len.ty(&self.nodes).is_some_and(|t| t.is_int()) {
                return Err(format!(
                    "Cannot allocate an array with length {}",
                    len.ty(&self.nodes).unwrap()
                ));
            }
            self.require("]")?;
            let tmp = self.type_ary(t)?;
            return Ok(self.new_array(tmp.data().to, len));
        }

        let Some(tmp) = t.to_mem_ptr() else {
            return Err(format!("Cannot allocate a {}", t.str()));
        };

        // Parse new struct { default_initialization }
        let Some(&s) = self.inits.get(tmp.data().to.name()) else {
            return Err(format!("Unknown struct type '{}'", tmp.data().to.name()));
        };

        let fs = self.nodes[s].fields();
        // if the object is fully initialized, we can skip a block here.
        // Check for constructor block:
        let has_constructor = self.match_("{");

        let mut init = s.to_node();
        let mut idx = 0;
        if has_constructor {
            idx = self.scope.inputs(&self.nodes).len();
            // Push a scope, and pre-assign all struct fields.
            self.scope.push(false, &mut self.nodes);
            for (i, f) in fs.iter().enumerate() {
                let s_in_i = s.inputs(&self.nodes)[i].unwrap();
                let t = if s_in_i.ty(&self.nodes) == Some(self.types.top) {
                    *self.con(self.types.bot)
                } else {
                    s_in_i
                };
                self.scope
                    .define(f.fname, f.ty, f.final_field, t, &mut self.nodes)
                    .expect("scope was empty");
            }

            // Parse the constructor body
            self.parse_block(true)?;
            self.require("}")?;
            init = self.scope.to_node();
        }

        // Check that all fields are initialized
        for i in idx..init.inputs(&self.nodes).len() {
            if
            /*init.at(i)._type == Type.TOP ||*/
            init.inputs(&self.nodes)[i].unwrap().ty(&self.nodes) == Some(self.types.bot) {
                let fname = fs[i - idx].fname;
                return Err(format!("'{}' is not fully initialized, field '{fname}' needs to be set in a constructor", tmp.data().to.name()));
            }
        }
        let size = self.int_con(tmp.data().to.offset(fs.len()));
        self.altmp.clear();
        self.altmp.copy_from_slice(&init.inputs(&self.nodes)[idx..]);
        let ptr = self.new_struct(tmp.data().to, *size);
        if has_constructor {
            self.scope.pop(&mut self.nodes);
        }
        Ok(ptr)
    }

    /// Return a NewNode initialized memory.
    ///
    /// differs from java: idx, init are passed in self.altmp
    fn new_struct(&mut self, obj: TyStruct<'t>, size: Node) -> Node {
        let fs = obj.fields();
        //         if( fs==null )
        //             throw error("Unknown struct type '" + obj._name + "'");

        let mut ns = Vec::with_capacity(2 + 2 * fs.len());
        ns.push(Some(self.ctrl())); // Control in slot 0
                                    // Total allocated length in bytes
        ns.push(Some(size));
        // Memory aliases for every field
        for f in fs {
            ns.push(Some(self.mem_alias_lookup(f.alias)));
        }
        // Initial values for every field
        for i in 0..fs.len() {
            ns.push(self.altmp[i]);
        }
        let nnn = New::new(self.types.get_mem_ptr(obj, false), ns, &mut self.nodes)
            .peephole(&mut self.nodes);
        for (i, f) in fs.iter().enumerate() {
            let name = self.types.get_str(&Parser::mem_name(f.alias));
            let proj = Proj::new(nnn, i + 2, name, &mut self.nodes).peephole(&mut self.nodes);
            self.mem_alias_update(f.alias, proj);
        }
        Proj::new(nnn, 1, obj.name(), &mut self.nodes).peephole(&mut self.nodes)
    }

    fn new_array(&mut self, ary: TyStruct<'t>, len: Node) -> Node {
        let base = ary.ary_base();
        let scale = ary.ary_scale();

        let con_base = self.int_con(base);
        let con_scale = self.int_con(scale);

        let shl = self.peep(Shl::new(
            len.keep(&mut self.nodes),
            Some(*con_scale),
            &mut self.nodes,
        ));
        let size = self.peep(Add::new(*con_base, shl, &mut self.nodes));
        self.altmp.clear();
        self.altmp.push(Some(len.unkeep(&mut self.nodes)));
        self.altmp
            .push(Some(*self.con(ary.fields()[1].ty.make_init())));
        self.new_struct(ary, size)
    }

    /// We set up memory aliases by inserting special vars in the scope these
    /// variables are prefixed by $ so they cannot be referenced in Simple code.
    /// Using vars has the benefit that all the existing machinery of scoping
    /// and phis work as expected
    fn mem_alias_lookup(&mut self, alias: u32) -> Node {
        self.scope
            .mem_read(alias as usize, &mut self.nodes)
            .unwrap()
    }

    fn mem_alias_update(&mut self, alias: u32, st: Node) {
        self.scope.mem_write(alias as usize, st, &mut self.nodes)
    }

    pub(crate) fn mem_name(alias: u32) -> String {
        format!("${alias}")
    }

    /// Parse postfix expression. For now this is just a field
    /// expression, but in future could be array index too.
    ///
    /// <pre>
    ///     expr ('.' IDENTIFIER)* [ = expr ]
    ///     expr #
    ///     expr ('[' expr ']')* = [ = expr ]
    /// </pre>
    fn parse_postfix(&mut self, expr: Node) -> PResult<Node> {
        let name = if self.match_(".") {
            self.require_id()?
        } else if self.match_("#") {
            "#"
        } else if self.match_("[") {
            "[]"
        } else {
            return Ok(expr); // No postfix
        };

        // Sanity check expr for being a reference
        let expr_ty = expr.ty(&self.nodes).unwrap();
        let Some(ptr) = expr_ty.to_mem_ptr() else {
            return Err(format!("Expected reference but got {}", expr_ty.str()));
        };

        // Happens when parsing known dead code, which often has other typing
        // issues.  Since the code is dead, possibly due to inlining, lets not
        // spoil the user experience with error messages.
        if self.ctrl().ty(&self.nodes) == Some(self.types.xctrl) {
            // Exit out via parsing the trailing expression
            return if self.match_opx(*b"==") {
                self.parse_asgn()
            } else {
                self.parse_postfix(*self.con(self.types.top))
            };
        }

        // Sanity check field name for existing
        let Some(tmp) = self.name_to_type.get(ptr.data().to.name()) else {
            return Err(format!("Accessing unknown field '{name}' from '{ptr}'"));
        };
        let tmp = tmp.to_mem_ptr().unwrap();

        let base = tmp.data().to;
        let Some(fidx) = base.find(name) else {
            return Err(format!(
                "Accessing unknown field '{name}' from '{}'",
                ptr.str()
            ));
        };

        // Get field type and layout offset from base type and field index fidx
        let f = &base.fields()[fidx]; // Field from field index
        let mut tf = f.ty;
        if let Some(ftmp) = tf.to_mem_ptr() {
            if ftmp.is_fref() {
                tf = ftmp.make_from(
                    self.name_to_type
                        .get(ftmp.data().to.name())
                        .unwrap()
                        .to_mem_ptr()
                        .unwrap()
                        .data()
                        .to,
                );
            }
        }

        // Field offset; fixed for structs, computed for arrays
        let off = if name == "[]" {
            // If field is an array body
            // Array index math
            let b = self.int_con(base.ary_base());

            let index = self.parse_asgn()?;
            self.require("]")?;
            let offset = Shl::new(index, Some(self.int_con(base.ary_scale())), &mut self.nodes);

            self.peep(Add::new(*b, self.peep(offset), &mut self.nodes))
        } else {
            // Struct field offsets are hardwired
            self.int_con(base.offset(fidx)).keep(&mut self.nodes)
        };

        // Disambiguate "obj.fld==x" boolean test from "obj.fld=x" field assignment
        if self.match_opx(*b"==") {
            let val = self.parse_asgn()?.keep(&mut self.nodes);
            let lift = self.lift_expr(val, tf, f.final_field)?;

            let st = Store::new(
                name,
                f.alias,
                tf,
                [
                    self.mem_alias_lookup(f.alias),
                    expr,
                    off.unkeep(&mut self.nodes),
                    lift,
                ],
                false,
                &mut self.nodes,
            );
            // Arrays include control, as a proxy for a safety range check.
            // Structs don't need this; they only need a NPE check which is
            // done via the type system.
            if base.is_ary() {
                st.set_def(0, self.ctrl(), &mut self.nodes);
            }
            self.mem_alias_update(f.alias, st.peephole(&mut self.nodes));
            return Ok(val.unkeep(&mut self.nodes)); // "obj.a = expr" returns the expression while updating memory
        }

        let load = Load::new(
            name,
            f.alias,
            tf.glb(),
            self.mem_alias_lookup(f.alias),
            expr.keep(&mut self.nodes),
            off,
        );
        // Arrays include control, as a proxy for a safety range check
        // Structs don't need this; they only need a NPE check which is
        // done via the type system.
        if base.is_ary() {
            load.set_def(0, self.ctrl(), &mut self.nodes);
        }
        let load = self.peep(load);

        // ary[idx]++ or ptr.fld++
        let inc = self.matchx("++");
        if inc || self.matchx("--") {
            if f.final_field && !f.fname == "[]" {
                return Err(format!("Cannot reassign final '{}'", f.fname));
            }
            let delta = self.int_con(if inc { 1 } else { -1 });
            let inc = self.peep(Add::new(load, *delta, &mut self.nodes));
            let val = self.zs_mask(inc, tf);
            let st = Store::new(
                name,
                f.alias,
                tf,
                [
                    self.mem_alias_lookup(f.alias),
                    expr.unkeep(&mut self.nodes),
                    off,
                    val,
                ],
                false,
                &mut self.nodes,
            );
            // Arrays include control, as a proxy for a safety range check.
            // Structs don't need this; they only need a NPE check which is
            // done via the type system.
            if base.is_ary() {
                st.set_def(0, self.ctrl(), &mut self.nodes);
            }
            self.mem_alias_update(f.alias, self.peep(st));
            // And use the original loaded value as the result
        } else {
            expr.unkill(&mut self.nodes);
        }
        off.unkill(&mut self.nodes);

        self.parse_postfix(load)
    }

    /// zero/sign extend.  "i" is limited to either classic unsigned (min==0) or
    /// classic signed (min=minus-power-of-2); max=power-of-2-minus-1.
    fn zs_mask(&mut self, val: Node, t: Ty<'t>) -> PResult<Node> {
        if let Some(tval) = val.ty(&self.nodes).and_then(|t| t.to_int()) {
            if let Some(t0) = t.to_int() {
                if !self.types.isa(tval, t0) {
                    if t0.min() == 0 {
                        self.peep(And::new(val, self.int_con(t0.max()))) // Unsigned
                    }
                    // Signed extension
                    let shift = t0.max().leading_zeros() - 1;
                    let shf = self.int_con(shift);
                    if shf.ty(&self.nodes) == Some(*self.types.int_zero) {
                        return Ok(val);
                    }
                    return Ok(self.peep(Sar::new(
                        self.peep(Shl::new(
                            val,
                            Some(shf.keep(&mut self.nodes)),
                            &mut self.nodes,
                        )),
                        Some(shf.unkeep(&mut self.nodes)),
                        &mut self.nodes,
                    )));
                }
            }
        }
        if let Some(tval) = val.ty(&self.nodes).and_then(|t| t.to_float()) {
            if let Some(t0) = t.to_float() {
                if !self.types.isa(tval, t0) {
                    // Float rounding
                    return Ok(self.peep(RoundF32::new(val, &mut self.nodes)));
                }
            }
        }
        Ok(val)
    }

    /// <pre>
    ///     integerLiteral: [1-9][0-9]* | [0]
    ///     floatLiteral: [digits].[digits]?[e [digits]]?
    /// </pre>
    fn parse_literal(&mut self) -> PResult<Constant> {
        self.lexer.parse_number(self.types).map(|ty| self.con(ty))
    }

    fn int_con(&mut self, con: i64) -> Constant {
        let ty = self.types.get_int(con);
        self.con(*ty)
    }

    fn con(&mut self, t: Ty<'t>) -> Constant {
        Constant::new(t, &mut self.nodes)
            .peephole(&mut self.nodes)
            .to_constant(&self.nodes)
            .unwrap()
    }

    fn peep(&mut self, n: impl Into<Node>) -> Node {
        // Peephole, then improve with lexically scoped guards
        self.scope
            .upcast_guard(n.into().peephole(&mut self.nodes), &mut self.nodes)
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

    fn match_opx(&mut self, c: [u8; 2]) -> bool {
        self.lexer.match_opx(c)
    }

    /// Return true and do NOT skip if `c` is next
    fn peek(&mut self, c: char) -> bool {
        self.lexer.peek_char(c)
    }

    fn pos(&mut self) -> &'s str {
        self.lexer.remaining
    }
    fn set_pos(&mut self, pos: &'s str) {
        self.lexer.remaining = pos;
    }

    /// Require and return an identifier
    fn require_id(&mut self) -> PResult<&'s str> {
        let id = self.lexer.match_id();
        id.filter(|i| !is_keyword(i))
            .ok_or_else(|| format!("Expected an identifier, found '{}'", id.unwrap_or("null")))
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
    "=;[]<>()+-/*&|^".contains(c)
}

impl<'a> Lexer<'a> {
    fn is_eof(&self) -> bool {
        self.remaining.is_empty()
    }

    fn peek(&self) -> Option<char> {
        self.remaining.chars().next()
    }

    // Just crash if misused
    fn peek_offset(&self, offset: isize) -> u8 {
        debug_assert_eq!(
            self.source[self.remaining.len()..].as_ptr(),
            self.remaining.as_ptr()
        );
        self.source.as_bytes()[self.remaining.len().checked_add_signed(offset).unwrap()]
    }

    fn next_char(&mut self) -> Option<char> {
        self.peek()
            .inspect(|c| self.remaining = &self.remaining[c.len_utf8()..])
    }

    fn is_whitespace(&self) -> bool {
        self.peek().as_ref().is_some_and(char::is_ascii_whitespace)
    }

    fn skip_whitespace(&mut self) {
        loop {
            if self.is_whitespace() {
                self.next_char();
            } else if self.remaining.starts_with("//") {
                if let Some(i) = self.remaining.find('\n') {
                    self.remaining = &self.remaining[i + 1..];
                } else {
                    self.remaining = "";
                }
            } else {
                break;
            }
        }
    }

    /// Next non-white-space character, or EOF
    fn next_x_char(&mut self) -> Option<char> {
        self.skip_whitespace();
        self.next_char()
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

    fn match_opx(&mut self, c: [u8; 2]) -> bool {
        self.skip_whitespace();
        if self.remaining.as_bytes().starts_with(&c) {
            self.remaining = &self.remaining[2..];
            true
        } else {
            false
        }
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

    /// Return a constant Type, either TypeInteger or TypeFloat
    fn parse_number<'t>(&mut self, types: &Types<'t>) -> PResult<Ty<'t>> {
        let (flt, snum) = self.is_long_or_double();
        if !flt {
            if snum.chars().next() == Some('0') {
                Err("Syntax error: integer values cannot start with '0'".to_string())
            } else {
                snum.parse().map(|i| *types.get_int(i)).map_err(|_| {
                    format!("{snum} could not be parsed to a positive signed 64 bit integer")
                })
            }
        } else {
            snum.parse()
                .map(|f| *types.get_float(f))
                .map_err(|_| format!("{snum} could not be parsed to a floating point value"))
        }
    }

    fn parse_number_string(&mut self) -> &'a str {
        self.is_long_or_double().1
    }

    /// Return +len that ends a long
    /// Return -len that ends a double
    fn is_long_or_double(&mut self) -> (bool, &'a str) {
        let old = self.remaining;
        while self.peek().is_some_and(|c| c.is_ascii_digit()) {
            self.next_char();
        }
        let c = self.peek();
        if self.remaining == old || !(c == Some('e') || c == Some('.')) {
            return (false, &old[..old.len() - self.remaining.len()]);
        }
        self.next_char();
        while self
            .peek()
            .is_some_and(|c| c == 'e' || c == '.' || c.is_ascii_digit())
        {
            self.next_char();
        }
        (true, &old[..old.len() - self.remaining.len()])
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

    /// Next oper= character, or 0.
    /// As a convenience, mark "++" as a char 1 and "--" as char -1 (65535)
    /// Disallow e.g. "arg--1" which should parse as "arg - -1"
    fn match_oper_assign(&mut self) -> Option<char> {
        self.skip_whitespace();
        let mut r = self.remaining.chars();
        let ch0 = r.next()?;
        let ch1 = r.next()?;
        let ch2 = r.next()?;

        if !"+-/*&|^".contains(ch0) {
            return None;
        }
        if ch1 == '=' {
            self.remaining = &self.remaining[2..];
            Some(ch0)
        } else if is_id_letter(ch2) {
            None
        } else if ch0 == '+' && ch1 == '+' {
            self.remaining = &self.remaining[2..];
            Some('\u{1}')
        } else if ch0 == '-' && ch1 == '-' {
            self.remaining = &self.remaining[2..];
            Some('\u{FFFF}')
        } else {
            None
        }
    }
}
